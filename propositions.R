ggplot(data = lineaire_topage_rang %>% 
         mutate(long_totale_km = as.numeric(long_totale_km)),
       aes(x = StreamOrde,
           y = long_totale_km)) +
  geom_bar(stat = "identity")


histo_classe_surface_bv <-
  ggplot(data = bv_bretagne_topage, 
         aes(x = surface_ha)) +
  geom_histogram() +
  scale_x_log10(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  labs(x = "Surface du bassin versant (Hectares)",
       y = "Nombre de bassin versant",
       title = "Répartition des bassins versant bretons selon leur taille")

histo_classe_lineaire_bv <-
  ggplot(data = bv_bretagne_topage, 
         aes(x = long_topag/1000)) + geom_histogram() + scale_x_log10() + labs(
           x = "Longueur de cours d'eau BD Topage (Km)",
           y = "Nombre de bassin versant",
           title = str_wrap("Répartition des bassins versant bretons selon leur linéaire hydrographique",
                            width = 40))



