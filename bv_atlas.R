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
  sf::read_sf(dsn = "data/TronconHydrographique_BV_cotiers_Bretagne_non_aqueduc_strahler.shp")

bv_bretagne <- 
  sf::read_sf(dsn = "data/bv_20230720_indicateurs.shp")

# Calcul des indicateurs

lineaire_rang <- troncons_topage %>%
  sf::st_drop_geometry() %>% 
  select(cdOH, longueur, StreamOrder) %>%
  as.data.frame() %>%
  group_by(StreamOrder) %>%
  summarise(long_totale = sum(longueur), na.rm = T)
            