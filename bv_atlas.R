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
  sf::read_sf(dsn = "data/bv_20230720_indicateurs.shp")

# Ajouter la longueur des lignes

troncons_topage2 <- troncons_topage %>%
  mutate(longueur = st_length(troncons_topage))


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

# Graph d'indicateurs            