# VERWERKING BRONDATA
#####################################################################

# SHAPEFILES

## Shapefile met archeoregio's
archeoregios <- 
     sf::st_read(here::here("shapefiles", "archeoregios", "archeoregios_02_2020.shp")) |> 
     sf::st_set_crs(31370) |>  
     dplyr::select(aard) |> 
     dplyr::rename(archeoregio = aard) |> 
     dplyr::mutate(archeoregio = forcats::fct_relevel(archeoregio,
      "duinen",
      "polders",
      "zandstreek",
      "(zand)leemstreek",
      "kempen",
      "maaskant")
     )
saveRDS(archeoregios, here::here("data_processed", "archeoregios.rds"))

## Provinciegrenzen
provincies <-
  roeference::get_vrbg_sf('Refprv', crs = 'EPSG:31370') |>
  dplyr::select(NAAM) |>
  dplyr::rename(provincie = NAAM) |>
  dplyr::mutate(provincie = dplyr::if_else(provincie == "Vlaams Brabant", "Vlaams-Brabant", provincie)) |>
  tidyr::drop_na() 

saveRDS(provincies, here::here("data_processed", "provincies.rds"))


## gemeentegrenzen
gemeentes <-
  roeference::get_vrbg_sf(crs = 'EPSG:31370') |>
  dplyr::select(NAAM) |>
  dplyr::rename(gemeente = NAAM) |> 
  tidyr::drop_na() 

saveRDS(gemeentes, here::here("data_processed", "gemeentes.rds"))

######################################################################

# BRONGEGEVENS UIT HET ARCHEOLOGIEPORTAAL

## archeologienota's en nota's
ANN <- roeference::get_oe_notas() |>
     dplyr::mutate(KADER = "vergunningsplichtige ingrepen")

## eindverslagen
EV <- roeference::get_oe_eindverslagen()

## archeologierapporten <- kan niet via roeference! Downlaod via geoportaal (archeologie_intern.zip)
path_GIS <-
  "C:/Users/hanecakr/Documents/R_projects/0000_EVARCH/GIS_shapefiles/"
AR <-
  sf::st_read(here::here(path_GIS, "archeologierapporten_intern.shp"))

## overzicht ingediende documenten op het archeologieportaal (AP) t.e.m huidige werkjaar
AP_docs <-
  rbind(EV, AR, ANN[, names(EV)]) |>
  dplyr::mutate(
    TYPE_NAAM = dplyr::case_when(
      TYPE_NAAM == "archeologienotas" ~ "archeologienota",
      TYPE_NAAM == "notas" ~ "nota",
      .default = TYPE_NAAM
    ),
    jaar = lubridate::year(DATUM_IND)
  ) |>
  dplyr::filter(jaar < werkjaar + 1)

######################################################################

# TOEVOEGEN ADMINISTRATIEVE GEGEVENS AAN DOCUMENTEN AP 
AP_docs <- 
     AP_docs |> 
     sf::st_as_sf() |> 
     # berekenen van geometrie oversnijding
     sf::st_intersection(provincies) |> 
     dplyr::mutate(provincie = as.character(provincie)) |> 
     # oppervlakte projectgebied/provincie
     dplyr::mutate(oppervlakte = sf::st_area(geometry)) |> 
     # per ID die meerdere prov. doorsnijdt, enkel deze weerhouden met grootste opp
     dplyr::group_by(ID) |> 
     dplyr::top_n(1, oppervlakte) |> 
     dplyr::ungroup() |>
     # oorspronkelijke geometry
     dplyr::select(ID, provincie) |> 
     sf::st_set_geometry(NULL) |>
     dplyr::left_join(AP_docs, by = "ID") |> 
     sf::st_as_sf() |> 
     # berekenen van geometrie oversnijding archeoregios
     sf::st_intersection(archeoregios) |> 
     dplyr::mutate(archeoregio = as.character(archeoregio)) |> 
     # oppervlakte porjectgebied/archeoregio
     dplyr::mutate(oppervlakte = sf::st_area(geometry)) |> 
     # per ID die meerdere archeoregio's doorsnijdt, enkel deze weerhouden met grootste opp
     dplyr::group_by(ID) |> 
     dplyr::top_n(1, oppervlakte) |> 
     dplyr::ungroup() |> 
     # oorspronkelijke geometry
     dplyr::select(ID, provincie, archeoregio) |> 
     sf::st_set_geometry(NULL) |> 
     dplyr::left_join(AP_docs, by = "ID") |> 
     sf::st_as_sf() |> 
     # berekenen van geometrie oversnijding gemeentes
     sf::st_intersection(gemeentes) |> 
     dplyr::mutate(gemeente = as.character(gemeente)) |> 
     # oppervlakte projectgebied/gemeente
     dplyr::mutate(oppervlakte = sf::st_area(geometry)) |> 
     # per ID die meerdere gemeentes's doorsnijdt, enkel deze weerhouden met grootste opp
     dplyr::group_by(ID) |> 
     dplyr::top_n(1, oppervlakte) |> 
     dplyr::ungroup() |> 
     # oorspronkelijke geometry
     dplyr::select(ID, gemeente, provincie, archeoregio) |> 
     sf::st_set_geometry(NULL) |> 
     dplyr::left_join(AP_docs, by = "ID") |> 
     dplyr::mutate(opp_projectgebied = sf::st_area(geometry)) |> 
     dplyr::mutate(opp_projectgebied = round(opp_projectgebied, 0)) 


## Correcties
AP_docs <- AP_docs |>
  dplyr::mutate(
    NAAM = dplyr::if_else(
      URI == "https://id.erfgoed.net/archeologie/eindverslagen/2750",
      paste(NAAM, "rapportage van een toevalsvondst"),
      NAAM
    )
  )

## Volgorde variabelen bepalen
AP_docs <-
  AP_docs |> 
  dplyr::mutate(
    TYPE_NAAM = factor(
      TYPE_NAAM,
      levels = c(
        "archeologienota",
        "nota",
        "archeologierapport",
        "eindverslag")),
    KADER_kort = dplyr::case_when(
          stringr::str_detect(NAAM, "oevalsvondst") ~ "TV",
          stringr::str_detect(KADER, "wetenschappelijke") ~ "WV",
          stringr::str_detect(KADER, "vergunningsplichtige") ~ "VI",
          stringr::str_detect(KADER, "vergunnningsplichtige") ~ "VI"
     ),
    KADER_kort = factor(
         KADER_kort,
         levels = c("VI", "WV", "TV")),
    rapportage = factor(
      paste(TYPE_NAAM, KADER_kort),
      levels =
        c(
          "archeologienota VI",
          "nota VI",
          "archeologierapport VI",
          "archeologierapport WV",
          "archeologierapport TV",
          "eindverslag VI",
          "eindverslag WV",
          "eindverslag TV"
        )
    ),
    provincie = factor(
      provincie,
      levels = c(
        "West-Vlaanderen",
        "Oost-Vlaanderen",
        "Antwerpen",
        "Vlaams-Brabant",
        "Limburg"
      )
    ),
    archeoregio = factor(
      archeoregio,
      levels = c(
        "duinen",
        "polders",
        "zandstreek",
        "(zand)leemstreek",
        "kempen",
        "maaskant"
      )
    )
  ) |> 
  dplyr::relocate(geometry, .after = last_col()) |> 
  sf::st_as_sf()

saveRDS(AP_docs, here::here("data_processed", "AP_docs.rds"))