## VERSION FINALISEE AU 20230726

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

## Filtrer les BV ayant un linéaire hydrographique permanent ----

bv_bretagne_permanent <- bv_bretagne %>%
  filter(long_perma != 0)

# Calcul des indicateurs ----

## Linéaire total moyen et median par BV

lineaire_topage_median_moy_km <- bv_bretagne_topage %>%
  sf::st_drop_geometry() %>% 
  select(IDD, long_topag) %>%
  as.data.frame() %>%
  summarise(lineaire_total_km = sum(long_topag/1000),
            lineaire_median_km = median(long_topag/1000), 
            lineaire_moyen_km = mean(long_topag/1000))

openxlsx::write.xlsx(lineaire_topage_median_moy_km,
                     file = "outputs/vf/1a_lineaire_topage_total_median_moyen.xlsx")

lineaire_permanent_median_moy_km <- bv_bretagne_permanent %>%
  sf::st_drop_geometry() %>% 
  select(IDD, long_perma) %>%
  as.data.frame() %>%
  summarise(lineaire_total_km = sum(long_perma/1000),
            lineaire_median_km = median(long_perma/1000), 
            lineaire_moyen_km = mean(long_perma/1000))

openxlsx::write.xlsx(lineaire_permanent_median_moy_km,
                     file = "outputs/vf/1b_lineaire_permanent_total_median_moyen.xlsx")

## Linéaire de réseau par rang de strahler ----

lineaire_topage_rang <- troncons_topage_strahler %>%
  sf::st_drop_geometry() %>% 
  select(CdOH, longueur_m, StreamOrde) %>%
  as.data.frame() %>%
  group_by(StreamOrde) %>%
  summarise(long_totale_m = sum(longueur_m), na.rm = T) %>%
  mutate(long_totale_km = long_totale_m/1000,
         long_totale_prct = long_totale_km*100/47804.4) %>%
  select(StreamOrde, long_totale_km, long_totale_prct)

openxlsx::write.xlsx(lineaire_topage_rang,
                     file = "outputs/vf/2a_lineaire_topage_strahler.xlsx")

lineaire_permanent_rang <- troncons_permanents_strahler %>%
  sf::st_drop_geometry() %>% 
  select(CdOH, longueur_m, StreamOrde) %>%
  as.data.frame() %>%
  group_by(StreamOrde) %>%
  summarise(long_totale_m = sum(longueur_m), na.rm = T) %>%
  mutate(long_totale_km = long_totale_m/1000,
         long_totale_prct = long_totale_km*100/21567.02) %>%
  select(StreamOrde, long_totale_km, long_totale_prct)


openxlsx::write.xlsx(lineaire_permanent_rang,
                     file = "outputs/vf/2b_lineaire_permanent_strahler.xlsx")

## BV moyen et median

bv_median_moy_ha <- bv_bretagne_topage %>%
  sf::st_drop_geometry() %>% 
  select(IDD, surface_m) %>%
  as.data.frame() %>%
  summarise(surface_mediane_ha = median(surface_m/10000), 
            surface_moyenne_ha = mean(surface_m/10000))

openxlsx::write.xlsx(bv_median_moy_ha,
                     file = "outputs/vf/3a_bv_median_moy_ha.xlsx")

bv_permanent_median_moy_ha <- bv_bretagne_permanent %>%
  sf::st_drop_geometry() %>% 
  select(IDD, surface_m) %>%
  as.data.frame() %>%
  summarise(surface_mediane_ha = median(surface_m/10000), 
            surface_moyenne_ha = mean(surface_m/10000))

openxlsx::write.xlsx(bv_permanent_median_moy_ha,
                     file = "outputs/vf/3b_bv_permanent_median_moy_ha.xlsx")

## Taux de drainage médian et moyen

tx_drainage_median_moy_km_km2 <- bv_bretagne_topage %>%
  sf::st_drop_geometry() %>% 
  select(IDD, surface_m, long_topag) %>%
  as.data.frame() %>%
  summarise(tx_drainage_median_km_km2 = median((long_topag/1000)/(surface_m/1000000)), 
            tx_drainage_moyen_km_km2 = mean((long_topag/1000)/(surface_m/1000000)))

openxlsx::write.xlsx(tx_drainage_median_moy_km_km2,
                     file = "outputs/vf/4a_tx_drainage_median_moy_km_km2.xlsx")

tx_drainage_permanent_median_moy_km_km2 <- bv_bretagne_permanent %>%
  sf::st_drop_geometry() %>% 
  select(IDD, surface_m, long_perma) %>%
  as.data.frame() %>%
  summarise(tx_drainage_median_km_km2 = median((long_perma/1000)/(surface_m/1000000)), 
            tx_drainage_moyen_km_km2 = mean((long_perma/1000)/(surface_m/1000000)))

openxlsx::write.xlsx(tx_drainage_permanent_median_moy_km_km2,
                     file = "outputs/vf/4b_tx_drainage_permanent_median_moy_km_km2.xlsx")

# Graph d'indicateurs  

## Linéaire de Topage par rang de Strahler ----

histo_lineaire_rang <-
  ggplot(data = lineaire_topage_rang %>% 
           mutate(long_totale_km = as.numeric(long_totale_km)),
         aes(x = StreamOrde,
             y = long_totale_km)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Rang de Strahler",
       y = "Linéaire de réseau hydrographique",
       title = str_wrap("Répartition du réseau hydrographique selon le rang de Strahler", width=60))

histo_lineaire_rang

## Linéaire de Topage permanent par rang de Strahler ----

histo_lineaire_permanent_rang <-
  ggplot(data = lineaire_permanent_rang %>% 
           mutate(long_totale_km = as.numeric(long_totale_km)),
         aes(x = StreamOrde,
             y = long_totale_km)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Rang de Strahler",
       y = "Linéaire de réseau hydrographique permanent",
       title = str_wrap("Répartition du réseau hydrographique permanent selon le rang de Strahler", width=60))

histo_lineaire_permanent_rang

## Bassins versant selon leur surface ----

histo_surface_bv <-
  ggplot(data = bv_bretagne_topage, 
         aes(x = surface_ha)) + 
  geom_histogram(fill="blue") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + labs(
           x = "Surface du bassin versant (Hectares)",
           y = "Nombre de bassin versant",
           title = str_wrap("Répartition des bassins versant bretons selon leur surface", width=60))

histo_surface_bv

## Bassins versant permanent selon leur surface ----

histo_surface_bv_permanent <-
  ggplot(data = bv_bretagne_permanent, 
         aes(x = surface_ha)) + 
  geom_histogram(fill = "blue") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Surface du bassin versant (Hectares)",
       y = "Nombre de bassin versant",
       title = str_wrap("Répartition des bassins versant bretons selon leur surface", width=60))

histo_surface_bv_permanent

## Bassins versant selon leur linéaire hydrographique ----

histo_lineaire_bv <-
  ggplot(data = bv_bretagne_topage, 
         aes(x = long_topag/1000)) + 
  geom_histogram(fill = "blue") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Longueur de cours d'eau BD Topage (Km)",
       y = "Nombre de bassin versant",
       title = str_wrap("Répartition des bassins versant bretons selon leur linéaire hydrographique", width=60))

histo_lineaire_bv

## Bassins versant selon leur linéaire hydrographique permanent ----

histo_lineaire_permanent_bv <-
  ggplot(data = bv_bretagne_permanent, 
         aes(x = long_perma/1000)) + 
  geom_histogram(fill = "blue") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Longueur de cours d'eau BD Topage (Km)",
       y = "Nombre de bassin versant",
       title = str_wrap("Répartition des bassins versant bretons selon leur linéaire hydrographique", width = 60))

histo_lineaire_permanent_bv

## Bassins versant selon leur taux de drainage ----

histo_tx_drainage_bv <-
  ggplot(data = bv_bretagne_topage, 
         aes(x = (long_topag/1000)/(surface_m/1000000))) + 
  geom_histogram(fill = "blue") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Taux de drainage du bassin versant (Km/Km²)",
       y = "Nombre de bassin versant",
       title = str_wrap("Répartition des bassins versant bretons selon leur taux de drainage", width=60))

histo_tx_drainage_bv

## Bassins versant selon leur taux de drainage ----

histo_tx_drainage_bv_permanent <-
  ggplot(data = bv_bretagne_permanent, 
         aes(x = (long_perma/1000)/(surface_m/1000000))) + 
  geom_histogram(fill = "blue") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Taux de drainage du bassin versant (Km/Km²)",
       y = "Nombre de bassin versant",
       title = str_wrap("Répartition des bassins versant bretons selon leur taux de drainage", width=60))

histo_tx_drainage_bv_permanent

# Sauvegarde

save(troncons_topage, 
     troncons_topage_strahler,
     troncons_permanents,
     troncons_permanents_strahler,
     lineaire_topage_median_moy_km,
     lineaire_permanent_median_moy_km,
     lineaire_topage_rang,
     lineaire_permanent_rang,
     bv_bretagne, 
     bv_bretagne_topage, 
     bv_bretagne_permanent,
     bv_median_moy_ha,
     bv_permanent_median_moy_ha,
     tx_drainage_median_moy_km_km2,
     tx_drainage_permanent_median_moy_km_km2,
     histo_lineaire_rang,
     histo_lineaire_permanent_rang,
     histo_surface_bv,
     histo_surface_bv_permanent,
     histo_lineaire_bv,
     histo_lineaire_permanent_bv,
     histo_tx_drainage_bv,
     histo_tx_drainage_bv_permanent,
     file = "outputs/bv_atlas.RData")
