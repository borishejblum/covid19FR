library(ggplot2)
library(maps)
mfr <- map_data("france")
na_dpts <- c("Gironde", "Pyrenees-Atlantiques", "Landes", "Lot-et-Garonne", 
             "Dordogne", "Correze", "Creuse", "Haute-Vienne", "Charente", "Vienne",
             "Deux-Sevres", "Charente-Maritime")

rna <- mfr %>% 
  filter(region %in% na_dpts)

ggplot(rna, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_map() +
  theme_void() +
  ggtitle("Nouvelle Aquitaine")
