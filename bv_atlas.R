## VERSION FINALISEE AU 20230627
## En cours d'ajout sur la partie analytique

# Library ----
#library(plyr)
library(tidyverse)
# library(lubridate)
# library(RcppRoll)
# library(DT)
# library(readxl)
# library(dbplyr)
# library(RPostgreSQL)
# library(rsdmx)
library(sf)
#library(stringi)

source(file = "R/functions.R")

# Chargement données----

troncons_topage <- 
  sf::read_sf(dsn = "data/TronconHydrographique_BV_cotiers_Bretagne_non_aqueduc_strahler.shp")

bv_bretagne <- 
  sf::read_sf(dsn = "data/bv_20230720_w_indicateurs.shp")

# Compléments géographiques----

## Ajouter la longueur des tronçons----

troncons_topage2 <- troncons_topage %>%
  mutate(longueur = st_length(troncons_topage))

## Ajouter la surface des BV----

bv_bretagne <- bv_bretagne %>%
  mutate(surface = st_area(bv_bretagne),
         surface = as.numeric(surface))

## Ajouter des classes de surface de BV----

bv_bretagne <- bv_bretagne %>%
  mutate(classe_taille_bv =  case_when(
    (surface > 0 & surface <1000000) ~ "Moins de 100 ha", 
    (surface > 1000000 & surface <5000000) ~ "Entre 100 et 1000 ha",
    (surface > 5000000 & surface <10000000) ~ "Entre 500 et 1000 ha", 
    (surface > 10000000 & surface <50000000) ~ "Entre 1000 et 5000 ha",
    (surface > 50000000 & surface <100000000) ~ "Entre 5000 et 10000 ha",
    (surface > 100000000) ~ "Plus de 10000 ha"))

## Ajouter des classes de linéaire 

bv_bretagne <- bv_bretagne %>%
  mutate(classe_lineaire_bv = case_when(
    (long_topag <1000) ~ "Entre 0 et 1 km", 
    (long_topag >= 1000 & long_topag <5000) ~ "Entre 1 et 5 km",
    (long_topag >= 5000 & long_topag <10000) ~ "Entre 5 et 10 km", 
    (long_topag >= 10000 & long_topag <50000) ~ "Entre 10 et 50 km",
    (long_topag >= 50000 & long_topag <100000) ~ "Entre 50 et 100 km",
    (long_topag >= 100000 & long_topag <500000) ~ "Entre 100 et 500 km",
    (long_topag >= 500000 & long_topag <1000000) ~ "Entre 500 et 1000 km"))

# Calcul des indicateurs ----

## Linéaire total de réseau ----

lineaire_total <- troncons_topage2 %>%
  sf::st_drop_geometry() %>% 
  select(CdOH, longueur) %>%
  as.data.frame() %>%
  summarise(longueur_totale_m = sum(longueur)) %>%
  mutate(longueur_totale_km = longueur_totale_m/1000) %>%
  select(longueur_totale_km)

## Linéaire de réseau par rang de strahler ----

lineaire_rang <- troncons_topage2 %>%
  sf::st_drop_geometry() %>% 
  select(CdOH, longueur, StreamOrde) %>%
  as.data.frame() %>%
  group_by(StreamOrde) %>%
  summarise(long_totale_m = sum(longueur), na.rm = T) %>%
  mutate(long_totale_km = long_totale_m/1000) %>%
  select(StreamOrde, long_totale_km)

openxlsx::write.xlsx(lineaire_rang,
                     file = "outputs/lineaire_hydro_strahler.xlsx")

## BV moyen et median

bv_median_moy <- bv_bretagne2 %>%
  sf::st_drop_geometry() %>% 
  select(IDD, surface, long_topag) %>%
  filter(long_topag != 0) %>%
  as.data.frame() %>%
  summarise(surface_mediane = median(surface), 
            surface_moyenne = mean(surface))

## Nombre de BV selon la classe de surface

surface_classe_bv <- bv_bretagne %>%
  sf::st_drop_geometry() %>% 
  select(IDD, long_topag, classe_taille_bv) %>%
  filter(long_topag != 0) %>%
  as.data.frame() %>%
  group_by(classe_taille_bv) %>%
  summarise(nbre_BV = n())

openxlsx::write.xlsx(surface_classe_bv,
                     file = "outputs/surface_classe_bv.xlsx")

## Nombre de BV selon la classe de linéaire

lineaire_classe_bv <- bv_bretagne %>%
  sf::st_drop_geometry() %>% 
  select(IDD, long_topag, classe_lineaire_bv) %>%
  filter(long_topag != 0) %>%
  as.data.frame() %>%
  group_by(classe_lineaire_bv) %>%
  summarise(nbre_BV = n())

openxlsx::write.xlsx(surface_classe_bv,
                     file = "outputs/surface_classe_bv.xlsx")

# Graph d'indicateurs          

lineaire_rang %>% 
  ggplot(aes(x = StreamOrde, y = long_totale_km)) +
  geom_bar(fill = "blue") +
  coord_flip() +
  labs(x = "Rang de Strahler",
       y = "Linéaire",
       title = "Linéaire de réseau hydrographique par rang de Strahler")
