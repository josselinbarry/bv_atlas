## VERSION FINALISEE AU 20230720

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

troncons_topage <- troncons_topage %>%
  mutate(longueur_m = st_length(troncons_topage))

## Ecarter les tronçons intermittents ----

troncons_permanents <- troncons_topage %>%
  filter(Persistanc != "intermittent")

## Ecarter les tronçons de rang 0 ----

troncons_topage_strahler <- troncons_topage %>%
  filter(StreamOrde != 0)

troncons_permanents_strahler <- troncons_permanents %>%
  filter(StreamOrde != 0)

## Ajouter la surface des BV----

bv_bretagne <- bv_bretagne %>%
  mutate(surface_m = st_area(bv_bretagne),
         surface_m = as.numeric(surface_m),
         surface_ha = surface_m/10000)

 ## Filter les BV ayant un linéaire hydrographique ----

bv_bretagne_topage <- bv_bretagne %>%
  filter(long_topag != 0)

## Filtrer les BV ayant un linéaire permanent ----

bv_bretagne_permanent <- bv_bretagne %>%
  filter(long_perma != 0)

## Ajouter des classes de surface de BV----

#bv_bretagne <- bv_bretagne %>%
#  mutate(classe_taille_bv =  case_when(
#    (surface > 0 & surface <1000000) ~ "Moins de 100 ha", 
#    (surface > 1000000 & surface <5000000) ~ "Entre 100 et 500 ha",
#    (surface > 5000000 & surface <10000000) ~ "Entre 500 et 1000 ha", 
#    (surface > 10000000 & surface <50000000) ~ "Entre 1000 et 5000 ha",
#    (surface > 50000000 & surface <100000000) ~ "Entre 5000 et 10000 ha",
#    (surface > 100000000) ~ "Plus de 10000 ha"))

## Ajouter des classes de linéaire 

#bv_bretagne <- bv_bretagne %>%
#  mutate(classe_lineaire_bv = case_when(
#    (long_topag <1000) ~ "Entre 0 et 1 km", 
#    (long_topag >= 1000 & long_topag <5000) ~ "Entre 1 et 5 km",
#    (long_topag >= 5000 & long_topag <10000) ~ "Entre 5 et 10 km", 
#    (long_topag >= 10000 & long_topag <50000) ~ "Entre 10 et 50 km",
#    (long_topag >= 50000 & long_topag <100000) ~ "Entre 50 et 100 km",
#    (long_topag >= 100000 & long_topag <500000) ~ "Entre 100 et 500 km",
#    (long_topag >= 500000 & long_topag <1000000) ~ "Entre 500 et 1000 km", 
#    (long_topag >= 1000000 & long_topag <5000000) ~ "Entre 1000 et 5000 km",
#    (long_topag >= 5000000) ~ "Plus de 5000 km"))

# Calcul des indicateurs ----

## Linéaire total de réseau ----

lineaire_total <- troncons_topage %>%
  sf::st_drop_geometry() %>% 
  select(CdOH, longueur_m) %>%
  as.data.frame() %>%
  summarise(longueur_totale_m = sum(longueur_m)) %>%
  mutate(longueur_totale_km = longueur_totale_m/1000) %>%
  select(longueur_totale_km)

lineaire_permanent_total <- troncons_permanents %>%
  sf::st_drop_geometry() %>% 
  select(CdOH, longueur_m) %>%
  as.data.frame() %>%
  summarise(longueur_totale_m = sum(longueur_m)) %>%
  mutate(longueur_totale_km = longueur_totale_m/1000) %>%
  select(longueur_totale_km)

## Linéaire de réseau par rang de strahler ----

lineaire_rang <- troncons_topage %>%
  sf::st_drop_geometry() %>% 
  select(CdOH, longueur_m, StreamOrde) %>%
  as.data.frame() %>%
  group_by(StreamOrde) %>%
  summarise(long_totale_m = sum(longueur_m), na.rm = T) %>%
  mutate(long_totale_km = long_totale_m/1000) %>%
  select(StreamOrde, long_totale_km)

lineaire_permanent_rang <- troncons_permanents %>%
  sf::st_drop_geometry() %>% 
  select(CdOH, longueur_m, StreamOrde) %>%
  as.data.frame() %>%
  group_by(StreamOrde) %>%
  summarise(long_totale_m = sum(longueur_m), na.rm = T) %>%
  mutate(long_totale_km = long_totale_m/1000) %>%
  select(StreamOrde, long_totale_km)


openxlsx::write.xlsx(lineaire_rang,
                     file = "outputs/lineaire_hydro_strahler.xlsx")

## Linéaire moyen et median par BV

lineaire_median_moy_km <- bv_bretagne_topage %>%
  sf::st_drop_geometry() %>% 
  select(IDD, long_topag) %>%
  as.data.frame() %>%
  summarise(lineaire_total_km = sum(long_topag/1000),
            lineaire_median_km = median(long_topag/1000), 
            lineaire_moyen_km = mean(long_topag/1000))

lineaire_permanent_median_moy_km <- bv_bretagne_permanent %>%
  sf::st_drop_geometry() %>% 
  select(IDD, long_perma) %>%
  as.data.frame() %>%
  summarise(lineaire_total_km = sum(long_perma/1000),
            lineaire_median_km = median(long_perma/1000), 
            lineaire_moyen_km = mean(long_perma/1000))

## BV moyen et median

bv_median_moy_ha <- bv_bretagne_topage %>%
  sf::st_drop_geometry() %>% 
  select(IDD, surface_m) %>%
  as.data.frame() %>%
  summarise(surface_mediane_ha = median(surface_m/10000), 
            surface_moyenne_ha = mean(surface_m/10000))

bv_permanent_median_moy_ha <- bv_bretagne_permanent %>%
  sf::st_drop_geometry() %>% 
  select(IDD, surface_m) %>%
  as.data.frame() %>%
  summarise(surface_mediane_ha = median(surface_m/10000), 
            surface_moyenne_ha = mean(surface_m/10000))

## Taux de drainage médian et moyen

tx_drainage_median_moy_km_km2 <- bv_bretagne_topage %>%
  sf::st_drop_geometry() %>% 
  select(IDD, surface_m, long_topag) %>%
  as.data.frame() %>%
  summarise(tx_drainage_median_km_km2 = median((long_topag/1000)/(surface_m/1000000)), 
            tx_drainage_moyen_km_km2 = mean((long_topag/1000)/(surface_m/1000000)))

tx_drainage_permanent_median_moy_km_km2 <- bv_bretagne_permanent %>%
  sf::st_drop_geometry() %>% 
  select(IDD, surface_m, long_perma) %>%
  as.data.frame() %>%
  summarise(tx_drainage_median_km_km2 = median((long_perma/1000)/(surface_m/1000000)), 
            tx_drainage_moyen_km_km2 = mean((long_perma/1000)/(surface_m/1000000)))

## Nombre de BV selon la classe de surface

#surface_classe_bv <- bv_bretagne %>%
#  sf::st_drop_geometry() %>% 
#  select(IDD, long_topag, classe_taille_bv) %>%
#  filter(long_topag != 0) %>%
#  as.data.frame() %>%
#  group_by(classe_taille_bv) %>%
#  summarise(nbre_BV = n())

#openxlsx::write.xlsx(surface_classe_bv,
#                     file = "outputs/surface_classe_bv.xlsx")

## Nombre de BV selon la classe de linéaire

#lineaire_classe_bv <- bv_bretagne %>%
#  sf::st_drop_geometry() %>% 
#  select(IDD, long_topag, classe_lineaire_bv) %>%
#  filter(long_topag != 0) %>%
#  as.data.frame() %>%
#  group_by(classe_lineaire_bv) %>%
#  summarise(nbre_BV = n())

#openxlsx::write.xlsx(lineaire_classe_bv,
#                     file = "outputs/lineaire_classe_bv.xlsx")

# Graph d'indicateurs  

## Linéaire de Topage par rang de Strahler ----

histo_lineaire_rang <-
  ggplot(data = troncons_topage_strahler,
         aes(x = StreamOrde)) +  geom_bar(fill = "blue") +
  coord_flip() + labs(
           x = "Rang de Strahler",
           y = "Linéaire de réseau hydrographique",
           title = "Répartition du linéaire de réseau hydrographique selon le rang de Strahler")

histo_lineaire_rang

histo_lineaire_permanent_rang <-
  ggplot(data = troncons_permanents_strahler,
         aes(x = StreamOrde)) +  geom_bar(fill = "blue") +
  coord_flip() + labs(
    x = "Rang de Strahler",
    y = "Linéaire de réseau hydrographique",
    title = "Répartition du linéaire de réseau hydrographique selon le rang de Strahler")

histo_lineaire_permanent_rang

## Bassins versant selon leur surface ----

histo_classe_surface_bv <-
  ggplot(data = bv_bretagne_topage, 
         aes(x = surface_ha)) + geom_histogram() + scale_x_log10() + labs(
           x = "Surface du bassin versant (Hectares)",
           y = "Nombre de bassin versant",
           title = "Répartition des bassins versant bretons selon leur taille")

histo_classe_surface_bv

histo_classe_surface_bv_permanent <-
  ggplot(data = bv_bretagne_permanent, 
         aes(x = surface_ha)) + geom_histogram() + scale_x_log10() + labs(
           x = "Surface du bassin versant (Hectares)",
           y = "Nombre de bassin versant",
           title = "Répartition des bassins versant bretons selon leur taille")

histo_classe_surface_bv_permanent

## Bassins versant selon leur linéaire hydrographique ----

histo_classe_lineaire_bv <-
  ggplot(data = bv_bretagne_topage, 
         aes(x = long_topag/1000)) + geom_histogram() + scale_x_log10() + labs(
           x = "Longueur de cours d'eau BD Topage (Km)",
           y = "Nombre de bassin versant",
           title = "Répartition des bassins versant bretons selon leur linéaire hydrographique")

histo_classe_lineaire_bv

histo_classe_lineaire_bv_permanent <-
  ggplot(data = bv_bretagne_permanent, 
         aes(x = long_perma/1000)) + geom_histogram() + scale_x_log10() + labs(
           x = "Longueur de cours d'eau BD Topage (Km)",
           y = "Nombre de bassin versant",
           title = "Répartition des bassins versant bretons selon leur linéaire hydrographique")

histo_classe_lineaire_bv_permanent

## Bassins versant selon leur taux de drainage ----

histo_classe_tx_drainage_bv <-
  ggplot(data = bv_bretagne_topage, 
         aes(x = (long_topag/1000)/(surface_m/1000000))) + geom_histogram() + scale_x_log10() + labs(
           x = "Taux de drainage du bassin versant (Km/Km²)",
           y = "Nombre de bassin versant",
           title = "Répartition des bassins versant bretons selon leur taux de drainage")

histo_classe_tx_drainage_bv

histo_classe_tx_drainage_bv_permanent <-
  ggplot(data = bv_bretagne_permanent, 
         aes(x = (long_perma/1000)/(surface_m/1000000))) + geom_histogram() + scale_x_log10() + labs(
           x = "Taux de drainage du bassin versant (Km/Km²)",
           y = "Nombre de bassin versant",
           title = "Répartition des bassins versant bretons selon leur taux de drainage")

histo_classe_tx_drainage_bv_permanent




lineaire_rang %>% 
  ggplot(data = lineaire_rang,
         aes(x = StreamOrde, y = long_totale_km)) +
  geom_bar(fill = "blue") +
  coord_flip() +
  labs(x = "Rang de Strahler",
       y = "Linéaire",
       title = "Linéaire de réseau hydrographique par rang de Strahler")
