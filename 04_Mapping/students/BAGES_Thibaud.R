#nouveaux pckages installés pour colorer

install.packages(c("cartography", "tmap", "viridis", "RColorBrewer"))

#packages chargés

library(sf)
library(dplyr)
library(cartography)
library(mapview)
library(viridis)
library(tidyverse)


------------
  #exercice 1
------------
  
  

idf_sf<- st_read("data/parispc_com.shp", #ouvrir idf_sf
                 crs=2154,
                 stringsAsFactors = F)

movies_paris <- st_read("data/lieux-de-tournage-a-paris.shp",#
                        crs = 4326,
                        stringsAsFactors = F)

movies_paris <- st_transform(movies_paris, 2154) #modifier la projection


idf_sf<- filter(idf_sf, NOM_DEPT == 'PARIS') #consrver uniquement les arrondissements parisiens


#créer la carte du nombre de tournages en 2017


nombre_tournage_2017 <- movies_paris %>%
  filter(annee_tourn == '2017') %>% 
  group_by(ardt_lieu) %>%
  summarise(total=n())

png("Output/tournages_2017.png") #ouvrir la création de la carte

plot(st_geometry(idf_sf), #visualer la forme gépmétrique par arrondissement
     col="ivory",
     border = "ivory3")

propSymbolsLayer(nombre_tournage_2017, 
                 var = "total", #variable choisie 
                 legend.pos = "topleft", #légende en haut à gauche
                 inches = 0.15, #taille du plus gros symbole
                 legend.style = "e", #légende "étendue"
                 legend.title.txt = "tournages", #titre de la légende
                 add = T) #ajouter au fond de carte précédent

layoutLayer(title = "Nombre de tournages par arrondissement en 2017",#titre de la carte 
            author = "Auteur: T. Bages",#nom de l'auteur
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)

# COMM : Ok, il manque les sources ! Et le doublon pour le 16e reste comme tel...

dev.off()#fin de création de la carte


# créer la carte du nombre de tournages en 2018 (code identique au précédent)


nombre_tournage_2018 <- movies_paris %>%
  filter(annee_tourn == '2018') %>% 
  group_by(ardt_lieu) %>%
  summarise(total=n())

png("Output/tournages_2018.png")

plot(st_geometry(idf_sf),
     col="ivory",
     border = "ivory3")

propSymbolsLayer(nombre_tournage_2018, 
                 var = "total", #variable choisie 
                 legend.pos = "topleft", #légende en haut à gauche
                 inches = 0.15, #taille du plus gros symbole
                 legend.style = "e", #légende "étendue"
                 legend.title.txt = "tournages", #titre de la légende
                 add = T) #ajouter au fond de carte précédent

layoutLayer(title = "Nombre de tournages par arrondissement en 2018", 
            author = "Auteur: T. Bages",
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)

dev.off()#fin de création de la carte


#créer la colonne "Tournage" regroupant la saison 8 de Engrenages et Emily in Paris


movies_paris_EE <- movies_paris %>%
  filter(nom_tournag == 'ENGRENAGES SAISON 8' | nom_tournag == 'Emily in Paris')#filtrer les deux séries traitées

movies_paris_EE <- mutate(movies_paris_EE, Tournage = if_else(nom_tournag == "ENGRENAGES SAISON 8", "Engrenages", "Emily in Paris"))#on crée un troisième colonne dont la valeur est définie par la fonction if_else

movies_paris_EE <- movies_paris_EE %>%
  mutate(Map = 2) #créer une variable supplémentaire pour utiliser propSymbolsTypoLayer

png("Output/tournages_EE.png") #ouvrir création d'une carte et désignation de l'enregristrement

plot(st_geometry(idf_sf), #créer fond de carte par arrondissement de Paris
     col="ivory",
     border = "ivory3")

propSymbolsTypoLayer(movies_paris_EE, #fonction pour des figurés ponctuels
                     var = "Map",
                     var2 = "Tournage",
                     inches = 0.05, #taille du figuré
                     col = viridis(2), #couleur de viridis, 2 variables
                     legend.var.pos = "none",  
                     add = T
                     )

layoutLayer(title = "Les tournages de Engrenages et Emily in Paris",#titre
            author = "Auteur: T. Bages", #auteur
            sources = "Source = Ville de Paris") #source

dev.off() #fin de création de la carte




------------
  #exercice 2
------------

#1 créer la carte typo
  
elections <- st_read("data/us_elections.gpkg") #ouvrir fichier us_elections

plot(elections) #visualiser le fichier

elections <- mutate(elections, vainqueur = if_else(Biden > Trump, "Biden", "Trump")) #colonne supplémentaite avec le nom du vainqueur

png("Output/victoire2020.png") # COMM : pensez à modifier les paramètres de taille.

typoLayer(elections,#créer une carte type
          var = "vainqueur", #choix de la variable
          col = c("blue", "red"), #choix des deux couleurs
          legend.title.txt = "Vainqueur de l'Etat", 
          legend.pos = "topleft"
) 

layoutLayer(
  title = "Election Américaine 2020", 
  author = "T. Bages, 2020.", 
)

dev.off() #fin de création de la carte
  



#2 carte des résultats de Biden par Etat

png("Output/biden.png")

plot(st_geometry(elections),#creation d'un fond de carte et choix des couleurs
     col="ivory",
     border="ivory3")

propSymbolsLayer(elections, #carte avec figurés ponctuels proportionnels
                 var = "Biden", #variable choisie
                 legend.pos = "topleft", #légende en haut à gauche
                 inches = 0.15, #taille du plus gros symbole
                 col = "blue",
                 legend.style = "e", #légende "étendue"
                 legend.title.txt = "Nombre de votes",
                 add = T) #titre de la légende

layoutLayer(title = "Votes en faveur de Joe Biden en 2020", 
            author = "Auteur: T. Bages",
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)

dev.off()




#3 carte des résultats de Trump par Etat


png("Output/trump.png")

plot(st_geometry(elections),
     col="ivory",
     border="ivory3")

propSymbolsLayer(elections, 
                 var = "Trump", #variable choisie
                 legend.pos = "topleft", #légende en haut à gauche
                 inches = 0.15, #taille du plus gros symbole
                 legend.style = "e", #légende "étendue"
                 legend.title.txt = "Nombre de votes",
                 add = T) #titre de la légende

layoutLayer(title = "Votes en faveur de Donald Trump en 2020", 
            author = "Auteur: T. Bages",
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)

dev.off()

# COMMENTAIRE : Travail correct 
# (très similaire, dans le code et le style, à celuir de B. Frel-Cazenave)
# Vous n'avez néanmoins pas repéré qu'il y avait deux codes pour le 16e arrondissement, ce qui entraîne un doublon ur la carte. 
# De plus, les cercles proportionnels ne sont pas représentés au centre du polygone.
# Pensez à regarder l'aide de chaque fonction
?propSymbolsLayer
# Les rubriques précisent comment rendre deux cartes de symboles proportionnels comparables.
# Ce qui aurait été nécessaire ici. 