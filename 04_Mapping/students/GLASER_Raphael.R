# Charger les packages 
library(sf)
library(tidyverse)
library(cartography)
library(raster)

#### Prealables #####

# On cree un objet SF
movies_sf <- st_read(dsn = "data/lieux-de-tournage-a-paris.shp",
                  crs = 4326,
                  stringsAsFactors = F)

# On le reprojette dans le systeme Lambert
movies_sf <- st_transform(movies_sf, 2154)

# On l explore
names(movies_sf)
head(movies_sf)
str(movies_sf)


#### Nombre de tournages par arrondissement (2017) ####

## On regroupe par arrondissement de tournage
arrond_sf <- movies_sf %>%
  group_by(ardt_lieu) %>% 
  summarise(Total = n()) %>%
  ungroup()

# COMMENTAIRE : si vous n'utilisez pas la variable "annee_tourn" pour faire un calcul groupé; 
# Il est logique qu'elle n'apparaisse plus dans le nouveau tableau. 
# Vous ne pouvez donc pas filtrer ce tableau par la suite
# La fonction group_by (par défaut) conserve, dans les calculs ultérieurs, seulement les variables que vous lui donnez !
# Il faut donc faire :
arrond_sf <- movies_sf %>%
  group_by(ardt_lieu,annee_tourn ) %>% # COMM Les deux variables pour calculer la fréquence par année et par arrondissement
  summarise(Total = n()) %>%
  ungroup()

  ## On se procure un fond de carte
parisOSM <- getTiles(x = movies_sf,
                     type = "CartoDB.Positron", 
                     crop = TRUE)

## On clippe le raster
parisOSM <- mask(parisOSM, arrond_sf)
  
tilesLayer(parisOSM)

plot(st_geometry(arrond_sf %>% filter(annee_tourn == "2017")),
     col = "coral1",
     border = "forestgreen")

# COMM Pour un fond de carte, il vous faut un objet spatial de POLYGONES.
# C'est celui des arrondissements de Paris. 

## Total est une variable quantitative absolue
## Donc carte de symboles proportionnels


propSymbolsLayer(arrond_sf, # COM : là aussi, vous devriez filtrer sur la varible année, mais vous ne 
                            # l'avez pas inclus dans le group_by
                 var = "Total", 
                 legend.pos = "bottomright", 
                 inches = 0.1, 
                 legend.style = "e", 
                 legend.title.txt = "Nombre de tournages",
                 add = T)

layoutLayer(title = "Nombre de tournages par arrondissement de Paris en 2018", 
            author = "Auteur: R. Glaser",
            sources = "Source : Ville de Paris",
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)

# COMMENTAIRE : il manque les autres exercices. Merci de me les renvoyer assez rapidement.
