## VERSION FINALISEE AU 20230726

# Library ----

install.packages("maptools")

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

load(file = "outputs/atlas_bv.RData")

troncons_topage <- 
  sf::read_sf(dsn = "data/TronconHydrographique_BV_cotiers_Bretagne_non_aqueduc_strahler.shp")

bv_bretagne <- 
  sf::read_sf(dsn = "data/bv_20230720_w_indicateurs.shp")

rpg_bzh <- 
  sf::read_sf(dsn = "data/RPG_2-0__SHP_LAMB93_R53_2021-01-01/PARCELLES_GRAPHIQUES.shp")

rpg_nor <- 
  sf::read_sf(dsn = "data/RPG_2-0__SHP_LAMB93_R28_2021-01-01/PARCELLES_GRAPHIQUES.shp")

rpg_pdl <- 
  sf::read_sf(dsn = "data/RPG_2-0__SHP_LAMB93_R52_2021-01-01/PARCELLES_GRAPHIQUES.shp")

# Compléments géographiques----

## Ajouter la longueur des tronçons----

troncons_topage <- troncons_topage %>%
  mutate(longueur_m = st_length(troncons_topage)) %>%
  mutate(longueur_m = as.numeric(longueur_m))

## Ecarter les tronçons intermittents ----

troncons_permanents <- troncons_topage %>%
  filter(Persistanc != "intermittent")

## Anlyser le réseau selon son statut de persistance

persistance_lineaire_topage <- troncons_topage %>%
  sf::st_drop_geometry() %>% 
  select(CdOH, StreamOrde,longueur_m, Persistanc) %>%
  filter(StreamOrde != 0) %>%
  as.data.frame() %>%
  group_by(Persistanc, StreamOrde) %>%
  summarise(long_totale_m = sum(longueur_m), na.rm = T) %>%
  mutate(long_totale_km = long_totale_m/1000,
         long_totale_prct = long_totale_km*100/47804.4) %>%
  mutate(long_totale_km = as.numeric(long_totale_km),
         long_totale_prct = as.numeric(long_totale_prct)) %>%
  select(StreamOrde, Persistanc, long_totale_km, long_totale_prct)

## Ecarter les tronçons de rang 0 ----

troncons_topage_strahler <- troncons_topage %>%
  filter(StreamOrde != 0)

troncons_permanents_strahler <- troncons_permanents %>%
  filter(StreamOrde != 0)

## Ajouter la surface des BV----

bv_bretagne <- bv_bretagne %>%
  mutate(surface_m2 = st_area(bv_bretagne),
         surface_m2 = as.numeric(surface_m2),
         surface_ha = surface_m2/10000)


## Ajouter une classe de surface de BV ----

bv_bretagne <- bv_bretagne %>%
  mutate(surface_ha = as.numeric(surface_ha),
         classe_surface = case_when(
           (surface_ha > 0 & surface_ha < 100) ~ 'Moins de 100 ha',
           (surface_ha >= 100 & surface_ha < 500) ~ 'Entre 100 et 500 ha',
           (surface_ha >= 500 & surface_ha < 1000) ~ 'Entre 500 et 1000 ha',
           (surface_ha >= 1000 & surface_ha < 5000) ~ 'Entre 1000 et 5000 ha',
           (surface_ha >= 5000 & surface_ha < 10000) ~ 'Entre 5000 et 10000 ha',
           (surface_ha >= 10000 ) ~ 'Plus de 10000 ha',
           long_perma != "0" ~ 'Permanent'
         ))

## Ajouter l'information sur la persistance du réseau hydro ----

bv_bretagne <- bv_bretagne %>%
  mutate(long_topag = as.numeric(long_topag),
       persistance = case_when(
         (long_topag != "0" & long_perma == "0") ~ 'Intermittent',
         (long_topag != "0" & long_perma != "0") ~ 'Permanent',
         (long_topag == "0" & long_perma == "0") ~ 'Pas de topage'
       ))

## Calculer le taux naturel de drainage

bv_bretagne <- bv_bretagne %>%
  mutate(taux_drainage_km_km2 = as.numeric((long_topag/1000)/(surface_m2/1000000)))

## Calculer le rang de strahler max

bv_strahler_max <- troncons_topage %>%  
  sf::st_drop_geometry() %>%
  group_by(bv_IDD, StreamOrde) %>%
  summarise() %>%
  mutate(strahler_max = max(StreamOrde)) %>%
  select(-StreamOrde) %>%
  unique()

bv_bretagne <- bv_bretagne %>% 
  dplyr::left_join(bv_strahler_max, 
                   by = c("IDD" = "bv_IDD"))
  
## Calculer la proportion totale, la moyenne et la médiane du parcellaire (RPG)

rpg_bretagne <- 
  rbind(rpg_bzh, rpg_pdl, rpg_nor)

# tres long
parcelles_bv <- rpg_bretagne %>%
  st_intersection(bv_bretagne)

synth_parcelles_bv <- parcelles_bv %>%
  select(ID_PARCEL, IDD) %>%
  mutate(surface_m2_rpg = st_area(parcelles_bv),
         surface_m2_rpg = as.numeric(surface_m2_rpg)) %>%
  sf::st_drop_geometry() %>%
  group_by(IDD) %>%
  summarise(surf_moy_rpg = mean(surface_m2_rpg),
            surf_med_rpg = median(surface_m2_rpg),
            surf_tot_rpg = sum(surface_m2_rpg)) %>%
    unique()

bv_bretagne <- bv_bretagne %>% 
  dplyr::left_join(synth_parcelles_bv, 
                   by = c("IDD" = "IDD")) %>%
  mutate(prct_rpg = surf_tot_rpg * 100 / surface_m2) %>%
  mutate(surf_moy_rpg = recoder_manquantes_en_zero(surf_moy_rpg),
         surf_med_rpg = recoder_manquantes_en_zero(surf_med_rpg),
         surf_tot_rpg = recoder_manquantes_en_zero(surf_tot_rpg),
         prct_rpg = recoder_manquantes_en_zero(prct_rpg))

## Filter les BV ayant un linéaire hydrographique ----

bv_bretagne_topage <- bv_bretagne %>%
  filter(long_topag != 0)

## Filtrer les BV ayant un linéaire hydrographique permanent ----

bv_bretagne_permanent <- bv_bretagne %>%
  filter(long_perma != 0)

persistance_lineaire_bv <- bv_bretagne_topage %>%
  sf::st_drop_geometry() %>% 
  select(IDD, surface_m, long_topag, long_perma) %>%
  mutate(long_topag = as.numeric(long_topag),
         persistance = case_when(
    long_perma == "0" ~ 'Intermittent',
    long_perma != "0" ~ 'Permanent'
  )) %>%
  as.data.frame() %>%
  group_by(persistance) %>%
  summarise(surface_totale_m = sum(surface_m), na.rm = T) %>%
  mutate(surface_totale_km = surface_totale_m/1000000,
         surface_totale_km = as.numeric(surface_totale_km)) %>%
  select(persistance, surface_totale_km)

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
  mutate(long_totale_km = as.numeric(long_totale_km),
         long_totale_prct = as.numeric(long_totale_prct)) %>%
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
  mutate(long_totale_km = as.numeric(long_totale_km),
         long_totale_prct = as.numeric(long_totale_prct)) %>%
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
  geom_bar(stat = "identity", fill = "#2374ee") +
  labs(x = "Rang de Strahler",
       y = "Linéaire de réseau hydrographique",
       title = str_wrap("Répartition du réseau hydrographique selon le rang de Strahler", width=40))

histo_lineaire_rang

## Linéaire de Topage permanent par rang de Strahler ----

histo_lineaire_permanent_rang <-
  ggplot(data = lineaire_permanent_rang %>% 
           mutate(long_totale_km = as.numeric(long_totale_km)),
         aes(x = StreamOrde,
             y = long_totale_km)) +
  geom_bar(stat = "identity", fill = "#2374ee") +
  labs(x = "Rang de Strahler",
       y = "Linéaire de réseau hydrographique permanent",
       title = str_wrap("Répartition du réseau hydrographique permanent selon le rang de Strahler", width=40))

histo_lineaire_permanent_rang

## Longueur de Topage selon le rang de Strahler et le niveau de persistance ----

histo_lineaire_persistance_rang <- 
  ggplot(data = persistance_lineaire_topage, 
         aes(x = StreamOrde, y = long_totale_km)) +
  geom_col(aes(fill = Persistanc), width = 0.7) + 
  scale_fill_manual(values = c("#d9d9d9", "#18d0f0", "#2374ee", "#fb01ff"))+
  labs(
    x = "Rang de Strahler",
    y = "Linéaire de réseau hydrographique (km)",
    title = str_wrap("Répartition du linéaire hydrographique selon leur rang de Strahler et leur persistance", width=50))


histo_lineaire_persistance_rang

## Bassins versant selon leur surface ----

histo_surface_bv <-
  ggplot(data = bv_bretagne_topage, 
         aes(x = surface_ha)) + 
  geom_histogram(fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + labs(
           x = "Surface du bassin versant (Hectares)",
           y = "Nombre de bassin versant",
           title = str_wrap("Répartition des bassins versant bretons selon leur surface", width=40))

histo_surface_bv

## Bassins versant permanent selon leur surface ----

histo_surface_bv_permanent <-
  ggplot(data = bv_bretagne_permanent, 
         aes(x = surface_ha)) + 
  geom_histogram(fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Surface du bassin versant (Hectares)",
       y = "Nombre de bassin versant",
       title = str_wrap("Répartition des bassins versant bretons selon leur surface", width=40))

histo_surface_bv_permanent

## Bassins versant selon leur surface et leur pesistance ----
# pti bug 'aes(x = ...' ne marche qu'avec les classes de surface (classe_surface) et non avec une distribution des surfaces (surface_ha)

histo_bv_persistance_surface <- 
  ggplot(data = bv_bretagne_topage, 
         aes(x = classe_surface, y = sum(surface_ha))) +
  geom_col(aes(fill = persistance), width = 0.7) + 
  scale_fill_manual(values = c( "#18d0f0", "#2374ee", "#fb01ff"))+
  labs(
    x = "Surface (Ha)",
    y = "Surfaces cumulées (ha)",
    title = str_wrap("Surface cumulée de bassins versant selon leur surface et leur persistance", width=50))

histo_bv_persistance_surface

## Bassins versant selon leur linéaire hydrographique ----

histo_lineaire_bv <-
  ggplot(data = bv_bretagne_topage, 
         aes(x = long_topag/1000)) + 
  geom_histogram(fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Longueur de cours d'eau BD Topage (Km)",
       y = "Nombre de bassin versant",
       title = str_wrap("Répartition des bassins versant bretons selon leur linéaire hydrographique", width=40))

histo_lineaire_bv

## Bassins versant selon leur linéaire hydrographique permanent ----

histo_lineaire_permanent_bv <-
  ggplot(data = bv_bretagne_permanent, 
         aes(x = long_perma/1000)) + 
  geom_histogram(fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Longueur de cours d'eau BD Topage (Km)",
       y = "Nombre de bassin versant",
       title = str_wrap("Répartition des bassins versant bretons selon leur linéaire hydrographique", width = 40))

histo_lineaire_permanent_bv

## Bassins versant selon leur taux de drainage ----

histo_tx_drainage_bv <-
  ggplot(data = bv_bretagne_topage, 
         aes(x = (long_topag/1000)/(surface_m/1000000))) + 
  geom_histogram(fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Taux de drainage du bassin versant (Km/Km²)",
       y = "Nombre de bassin versant",
       title = str_wrap("Répartition des bassins versant bretons selon leur taux de drainage", width=40))

histo_tx_drainage_bv

## Bassins versant selon le pourcentage de RPG ----

histo_prct_rpg <-
  ggplot(data = bv_bretagne %>% 
         filter(persistance != "Pas de topage"),
         aes(x = prct_rpg)) + 
  geom_histogram(fill="#2374ee") + 
  labs(x = "Pourcentage d'occupation surfacique en culture (RPG)",
       y = "Nombre de bassin versant",
       title = str_wrap("Répartition des bassins versant bretons selon leur proportion de RPG", width=40))

histo_prct_rpg

histo_tx_drainage_bv_permanent <-
  ggplot(data = bv_bretagne_permanent, 
         aes(x = (long_perma/1000)/(surface_m/1000000))) + 
  geom_histogram(fill="#2374ee") + 
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) + 
  labs(x = "Taux de drainage du bassin versant (Km/Km²)",
       y = "Nombre de bassin versant",
       title = str_wrap("Répartition des bassins versant bretons selon leur taux de drainage", width=40))

histo_tx_drainage_bv_permanent


## Export des données ----

sf::write_sf(obj = bv_bretagne, dsn = "data/outputs/bv_bretagne_20230928.gpkg")

# Sauvegarde ----

save( bv_bretagne,
      parcelles_bv,
      synth_parcelles_bv,
     file = "outputs/atlas_bv.RData")

save(troncons_topage, 
     troncons_topage_strahler,
     troncons_permanents,
     troncons_permanents_strahler,
     persistance_lineaire_topage,
     persistance_lineaire_bv,
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
     histo_lineaire_persistance_rang,
     histo_surface_bv,
     histo_surface_bv_permanent,
     histo_lineaire_bv,
     histo_lineaire_permanent_bv,
     histo_tx_drainage_bv,
     histo_tx_drainage_bv_permanent,
     file = "outputs/bv_atlas.RData")

