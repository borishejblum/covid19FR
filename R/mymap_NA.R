rm(list=ls())
library(maps)
mfr <- ggplot2::map_data("france")
na_dpts <- c("Gironde", "Pyrenees-Atlantiques", "Landes", "Lot-et-Garonne", 
             "Dordogne", "Correze", "Creuse", "Haute-Vienne", "Charente", "Vienne",
             "Deux-Sevres", "Charente-Maritime")

rna <- mfr %>% 
  filter(region %in% na_dpts)

library(readxl)
library(lubridate)
cas_NA_ARS <- suppressWarnings(read_xlsx("data/raw/Tableau_decompte_des_cas_confirmes_NA_publi13-03-2020.xlsx", 
                    skip=2, col_types = c("date", "text", "numeric", "text"), ))
colnames(cas_NA_ARS)[1] <- "Date"
cas_NA_ARS <- cas_NA_ARS %>% 
  select(1:3) %>% 
  filter(!is.na(Date))
dpt <- read.csv("data/departements-region.csv")

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  text <- gsub(" ", "", text)
  return(text)
}

library(dplyr)
dpt$dep_name <- Unaccent(dpt$dep_name)
cas_NA_ARS_dpt <- merge(cas_NA_ARS, dpt, by.x="Dpt", by.y="num_dep")
cas_NA_ARS_dpt <- cas_NA_ARS_dpt %>% 
  arrange(Date) %>% 
  group_by(Dpt) %>% 
  #mutate(CumNb = cumsum(Nombre)) %>% 
  mutate(Date = ymd(Date)) 

rna_full <- list()
all_days <- seq(from = min(unique(cas_NA_ARS_dpt$Date)), 
                to = max(unique(cas_NA_ARS_dpt$Date)),
                by=1)
for (d in as.character(all_days)){
  rna_full[[d]] <- rna
  rna_full[[d]]$region
  rna_full[[d]]$Date <- ymd(d)
  rna_full[[d]] <- left_join(rna_full[[d]], cas_NA_ARS_dpt, by=c("region"="dep_name", "Date"))
  rna_full[[d]]$Nombre[is.na(rna_full[[d]]$Nombre)] <- 0
}
rna_full2plot <- do.call(rbind.data.frame, rna_full)
rna_full2plot <- rna_full2plot %>% 
  arrange(Date) %>% 
  group_by(order) %>% 
  mutate(CumNb = cumsum(Nombre)) %>% 
  select(-subregion, -Dpt, -region_name)

#rna_full2plot %>% filter(region == "Gironde")
#View(rna_full2plot %>% filter(order == 9107))

  
library(ggplot2)
#temp <- full_join(rna, cas_NA_ARS_dpt, by=c("region"="dep_name"))
p <- rna_full2plot %>% 
  #filter(Date == ymd("2020-03-12")) %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill=CumNb), colour = "black") +
    scale_fill_gradientn("Incidence cumulée\nd'infections à COVID19\nconfirmées",
                         colours=c("ivory", "lightskyblue1", "deepskyblue", "dodgerblue2", 
                                   "blue", "midnightblue")
                         , values=scales::rescale(c(0,2,10,20,30,40), to=c(0,1))
                         #, minor_breaks=c(5,20)
                         , breaks=c(0,10,20,30,40)
                         #, trans = "log1p"
                         , limits=c(0,40) 
                         ) +
    coord_map() +
    theme_void() +
    labs(caption = "Source : ARS Nouvelle Aquitaine") +
    #ggtitle("Nouvelle Aquitaine") +
    #facet_wrap(~Date) +
    NULL
p + ggtitle("Nouvelle Aquitaine") +
  facet_wrap(~Date) +
  NULL


library(gganimate)
library(transformr)
anim <- p + 
  ggtitle("Nouvelle Aquitaine", subtitle = '{closest_state}') +
  transition_states(Date, transition_length = 0, state_length = 1)
animate(anim, fps=10, end_pause=15, nframes = 170)
anim_save(filename = "COVID19_NouvAqui_13mars.gif")


