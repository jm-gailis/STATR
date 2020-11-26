#### Cartographie des tournages à Paris (données Paris Opendata)

# 1. Chargement des packages et import des données----

library(tidyverse)
library(sf)
library(cartography)
library(rcartocolor)
library(rgdal)
library(raster)
library(conflicted)

conflict_prefer("select", "dplyr") # Et oui...
conflict_prefer("filter", "dplyr")


movies <- st_read("data/lieux-de-tournage-a-paris.shp", # Keep objet intermédiaire pour further manip
                  stringsAsFactors = FALSE)

# 2. Recodages----

## 2.1. Tournages par arrondissement en 2017 et 2018

ardt_sf <- st_read("data/parispc_com.shp", crs = 2154, # Polygones correspondant aux arrondissements
                   stringsAsFactors = FALSE) %>%
  select(INSEE_COM) %>%
  filter(str_detect(INSEE_COM, "^75") == TRUE) %>%
  rename(ardt = INSEE_COM) %>%
  mutate(ardt = as.numeric(ardt),
         ardt = ardt - 75100) 

movies_ardt <-  movies %>%
  st_set_geometry(NULL) %>%
  rename(ardt = ardt_lieu,
         annee = annee_tourn) %>%
  filter(!(ardt %in% c(93200, 93500, 94320)),
         annee %in% c(2017, 2018)) %>%
  mutate(ardt = as.numeric(ardt),
         ardt = if_else(ardt == 75116, ardt - 75100, ardt - 75000)) %>%
  group_by(annee, ardt) %>%
  summarise(nb_tournages = n()) %>%
  split(.$annee) %>%
  map(~ left_join(ardt_sf, .)) # Fonctionnal programming avec purr pour un code plus concis


## 2.2. Tournages de Emily in Paris et Engrenages

emily_engrenages <- movies %>%
  st_transform(crs = 2154) %>%
  rename(tournage = nom_tournag) %>%
  mutate(tournage = str_to_lower(tournage),
         tournage = case_when(
           str_detect(tournage, "^engrenages") == TRUE ~ "Engrenages",
           str_detect(tournage, "^emily") == TRUE ~ "Emily in Paris"
           ),
         tournage = as.factor(tournage), #pas nécessaire
         map = 1
         ) %>%
  filter(tournage %in% c("Engrenages", "Emily in Paris")) # Sûrement il y a plus concis, avec na.rm () ?

# 3. Cartographie----
pdf("graphiques/Boulakia_carte_tournages.pdf",
    width = 7,
    height = 7,
    useDingbats = FALSE)

paris_osm <- getTiles(x = ardt_sf, type = "osm", crop = TRUE) %>%
  mask(ardt_sf)

## 3.1. Cartes du nombre de tournages par arrondissement en 2017 et 2018

carte_ardt <- function(x) {
  tilesLayer(paris_osm)
  
  plot(st_geometry(movies_ardt[[x]]),
       border = "grey80",
       add = TRUE)
  
  propSymbolsLayer(x = movies_ardt[[x]],
                    var = "nb_tournages",
                    legend.pos = "topright",
                    legend.title.txt = "Nombre \n de tournages",
                    inches = 0.15,
                    add = TRUE)
  
  layoutLayer(title = paste("Nombre de tournages par arrondissement en", names(movies_ardt)[[x]],
                            sep = " "),
              author = "Auteur: Théo Boulakia",
              sources = "Sources: opendata Paris",
              tabtitle = TRUE,
              frame = TRUE
              )

}

for (i in seq_along(movies_ardt)) {
  carte_ardt(i)
}

## 3.2. Carte des tournages de Emily in Paris et Engrenages

tilesLayer(paris_osm)

plot(st_geometry(emily_engrenages),
     add = TRUE)

propSymbolsTypoLayer(x = emily_engrenages,
                     var = "map",
                     var2 = "tournage",
                     col = carto_pal(n = 2, name = "Bold"),
                     inches = 0.04,
                     legend.var.pos = "none",
                     legend.var2.title.txt = "Tournages",
                     legend.var2.pos = "topright",
                     add = TRUE)

layoutLayer(title = "Paris des flics et Paris des clichés",
            author = "Auteur: Théo Boulakia, 2020",
            sources = "Sources: opendata Paris, 2020",
            tabtitle = TRUE,
            frame = TRUE
            )

dev.off()

# COMMENTAIRE : beau script. Mes remarques portent essentiellement sur la sémiologie graphique
# Vous mettez un fond de carte qui comporte du vert, et les tournages d'Emily sont en vert aussi. 
# Donc confusion en terme de lisibilité. 
# Vous êtes manifestement très à l'aise avec les fonctionnalités découvertes.
# Les variables de taux devraient être plus intéressantes vus vos acquis. 