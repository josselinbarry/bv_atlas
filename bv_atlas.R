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

# Chargement données

troncons_topage <- 
  sf::read_sf(dsn = "data/TronconHydrographique_Bretagne_Pays_de_la_Loire_non_aqueduc_strahler.shp")

bv_bretagne <- 
  sf::read_sf(dsn = "data/bv_20230720_w_indicateurs.shp")

# Ajouter la longueur des tronçons

troncons_topage2 <- troncons_topage %>%
  mutate(longueur = st_length(troncons_topage))

# Ajouter la surface des BV

bv_bretagne2 <- bv_bretagne %>%
  mutate(surface = st_area(bv_bretagne))

# Ajouter des classes de BV

bv_bretagne3 <- bv_bretagne2 %>%
  mutate(classe_taille_bv = case_when(
    (surface > 0 & surface <1000000) ~ "Moins de 100 ha", 
    (surface > 1000000 & surface <5000000) ~ "Entre 100 et 1000 ha",
    (surface > 5000000 & surface <10000000) ~ "Entre 500 et 1000 ha", 
    (surface > 10000000 & surface <50000000) ~ "Entre 1000 et 5000 ha",
    (surface > 50000000 & surface <100000000) ~ "Entre 5000 et 10000 ha",
    (surface > 100000000) ~ "Plus de 10000 ha"))

# Calcul des indicateurs

lineaire_total <- troncons_topage2 %>%
  sf::st_drop_geometry() %>% 
  select(CdOH, longueur) %>%
  as.data.frame() %>%
  summarise(longueur_totale_m = sum(longueur)) %>%
  mutate(longueur_totale_km = longueur_totale_m/1000) %>%
  select(longueur_totale_km)

lineaire_rang <- troncons_topage2 %>%
  sf::st_drop_geometry() %>% 
  select(CdOH, longueur, StreamOrde) %>%
  as.data.frame() %>%
  group_by(StreamOrde) %>%
  summarise(long_totale_m = sum(longueur), na.rm = T) %>%
  mutate(long_totale_km = long_totale_m/1000) %>%
  select(StreamOrde, long_totale_km)

bv_median <- bv_bretagne2 %>%
  sf::st_drop_geometry() %>% 
  select(IDD, surface) %>%
  as.data.frame() %>%
  summarise(surface_mediane = median(surface))

# Graph d'indicateurs          

lineaire_rang %>% 
  ggplot(aes(x = StreamOrde, y = long_totale_km)) +
  geom_bar(fill = "blue") +
  coord_flip() +
  labs(x = "Rang de Strahler",
       y = "Linéaire",
       title = "Linéaire de réseau hydrographique par rang de Strahler")
