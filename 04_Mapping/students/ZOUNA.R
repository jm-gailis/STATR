#Cours09/11
#JoelZouna 



# COMMENTAIRE : vous n'avez pas à installer les packages à chaque fois. 
# Une seule fois suffit.
# Apris, il vous gaut seulement lees 

install.packages('sf')
library(sf)

install.packages('tidyverse')
library(tidyverse)

install.packages(c("cartography", "tmap", "viridis", "RColorBrewer"))

library(cartography)

library(mapview)

#Nombre de tournages en 2017

tournage_sf <- st_read(dsn = "data/lieux-de-tournage-a-paris.shp", crs = 4326, stringsAsFactors = F)
tournage_pj <- st_transform(tournage_sf, 2154)
# COMMENTAIRE : pas besoin de créer deux fichiers : cela amène de la confusion pour rien.
tournage_sf <- st_transform(tournage_sf, 2154)

head(tournage_pj)
idf_sf <- st_read(dsn = "data/parispc_com.shp" , crs=2154, )

tournage_numb <- tournage_pj  %>%  filter(annee_tourn == 2017) %>% group_by(ardt_lieu,annee_tourn) %>% summarise(Total_tourn = n())

tournage_numb18 <- tournage_pj  %>%  filter(annee_tourn == 2018) %>% group_by(ardt_lieu,annee_tourn) %>% summarise(Total_tourn = n())

install.packages('sp')
library(sp)

library (raster)
install.packages('osmdata')
library(osmdata)

# COMMENTAIRE : votre script est mal organisé. Chargez TOUS les packages à l'entame de votre script.

#paris <- tournage_sf %>%  st_as_sf(place, coords = c("coord_x", "coord_y"), crs = 2154)

paris <- idf_sf %>% filter(NOM_DEPT == "PARIS")

parisOSM <- getTiles(x = paris,
                     type = "CartoDB.Positron", 
                     crop = TRUE,
                     zoom = NULL)

tilesLayer(parisOSM)

library(raster)
parisOSM <-mask(parisOSM, paris)
 
tilesLayer(parisOSM)

plot(st_geometry(idf_sf %>% filter(NOM_DEPT == "PARIS")),
     col=NA,  
     border="black", 
     bg = "yellow", 
     lwd = 1, 
     add = TRUE)

propSymbolsLayer(tournage_numb,
                 var = "Total_tourn",
                 legend.pos = "topright",
                 inches = 0.1,
                 legend.style = "e",
                 legend.title.txt = "nombre de tournages",
                 add = TRUE)   

layoutLayer(title = "Tournages en Ile de France par arrondissement en 2017", 
            author = "Auteur: J. Zouna",
            sources = "Source : INSEE, 2020", # COMM La source est Open Data Paris. 
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)

propSymbolsLayer(tournage_numb,
                 var = "n",
                 legend.pos = "bottomleft",
                 inches = 0.1,
                 legend.style = "e",
                 legend.title.txt = "nombre de tournages",
                 add = T ) 


layoutLayer(title = "Tournages en Ile de France par arrondissement en 2017", 
            author = "Auteur: J. Zouna",
            sources = "Source : INSEE, 2020",
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)

#Nombre de tournages en 2018

propSymbolsLayer(tournage_numb18,
                 var = "Total_tourn",
                 legend.pos = "topright",
                 inches = 0.1,
                 legend.style = "e",
                 legend.title.txt = "nombre de tournages",
                 add = TRUE) 

layoutLayer(title = "Tournages en Ile de France par arrondissement en 2018", 
            author = "Auteur: J. Zouna",
            sources = "Source : INSEE, 2020",
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)

#Localisation de Emily in Paris et Engrenages 

# COMMENTAIRE : à nouveau, vous laissez des espaces dans vos fonctions...

local_en <- tournage_pj %>%  mutate ( Tournage = ifelse( nom_tournag == "Emily in Paris" , "Lieu de tournage Emily in Paris" , 
                                        ifelse(nom_tournag == "ENGRENAGES SAISON 8" , "Lieu de tournage Engrenages" , "Autres" ))) %>%
  mutate( Map=1) 
# AJOUTER UN FILTER ICI, par exemple : 
#filter(!nom_tournag = "Autres"), pour ne garder que les unités spatiales pertinentes


parisOSM <-mask(parisOSM, paris)

tilesLayer(parisOSM)

plot(st_geometry(idf_sf %>% filter(NOM_DEPT == "PARIS")),
     col=NA,  
     border="black", 
     bg = "ivory", 
     lwd = 1, 
     add = TRUE)

library(viridis) #IDEM, pas sa place ici.

propSymbolsTypoLayer(local_en,
                     var = "Map",
                     var2 = "Tournage",
                     inches = 0.025,
                     col = viridis(3),
                     legend.var.pos = "none",  
                     legend.var2.pos = "bottomleft",
                     add = T)

# COMMENTAIRE : la carte est illisible. Il suffit d'utiliser la fonction filter afin de sélectionner les unités spatiales
# Travail précipité.

layoutLayer(title ="Lieux de tournage des series Emily in Paris et Engrenages",
            author = "Auteur: J. Zouna")


elections_sf <- st_read(dsn="data/us_elections.gpkg")

#Script sur les elections presidentielles aux USA (oui j'ai decide de faire l'habillage en ANG)

#Gagnant par etat 

head(elections_sf)
winner_states <- elections_sf %>%  # COMM : inutile de créer un nouvel objet.
  mutate ( winner = ifelse( Biden > Trump , "Biden" , "Trump")) %>% # COMMENTAIRE : prenez l'habitude de revenir à la ligne après chaque %>%
  # mutate( Map = geom) %>% # Je ne comprends pas cette ligne....
  mutate( Val = 1) 


plot(st_geometry(elections_sf),
     col=NA,  
     border="black", 
     bg = "ivory", 
     lwd = 1)

propSymbolsTypoLayer(winner_states,
                     var = "Val",
                     var2 = "winner",
                     inches = 0.06,
                     col = c("red", "blue"),
                     legend.var.pos = "none",  
                     legend.var2.pos = "topleft",
                     add = T)    

layoutLayer(title ="Winner per state, 2020 US elections",
            author = "Author: J. Zouna")

# COMMENTAIRE : erreur dans la fonction. Vous représentez une variable catégorielle sur des polygones, 
# Donc la fonction est typoLayer(). Soyez pluss attentif à la sémiologie graphique et au nombre de variables. 


#Nombre de votes obtenu par Biden par etat 


plot(st_geometry(elections_sf),
     col=NA,  
     border="black", 
     bg = "ivory", 
     lwd = 1)

propSymbolsLayer(winner_states,
                 var = "Biden",
                 legend.pos = "topright",
                 col = "blue",
                 inches = 0.1,
                 legend.style = "e",
                 legend.title.txt = "votes counts",
                 add = TRUE)

layoutLayer(title ="Votes for Biden, 2020 US elections",
            author = "Author: J. Zouna")

#Nombre de votes obtenu par Trump par etat 


plot(st_geometry(elections_sf),
     col=NA,  
     border="black", 
     bg = "ivory", # COMM : ce fond de couleur n'est pas du meilleur effet. Soignez votre carte.
     lwd = 1)

propSymbolsLayer(winner_states,
                 var = "Trump",
                 legend.pos = "topright",
                 col = "red",
                 inches = 0.1,
                 legend.style = "e",
                 legend.title.txt = "votes counts",
                 add = TRUE)

layoutLayer(title ="Votes for Trump, 2020 US elections",
            author = "Author: J. Zouna") # Date ?

#fin du script 

# COMMENTAIRE : Quelques erreurs dans le choix des fonctions. 
#Le lien entre type de variable, sémiologie graphique et types de cartes ne semble pas encore compris. 
# La carte des tournages est illisible : il suffisait d'utiliser une fonction filter. Le doublon du 16e arrondissement n'est pas vu.
# Au niveau du code : 
# - Vous devez progresser sur l'organisation du script, chaotique par moment, entre le chargement des packages et le traitement des données. 
# - Ne multipliez pas les objets de manière inutile
# - ne laissez pas d'espaces. 
# Bref, plus de rigueur, sur le fond (la cartographie) et la forme (le script).
# Vous en êtes largement capable.