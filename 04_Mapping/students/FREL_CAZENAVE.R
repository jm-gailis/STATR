##https://jmigozzi.github.io/statsmappingwithR/04_Mapping/04_Mapping.html#1 
#Lien séance et exercice 


library(sf)
library(dplyr)
library(cartography)
library(mapview)
library(viridis)
library(tidyverse)
library(RColorBrewer)



#####EXERCICE 1#####

movies_paris <- st_read("data/lieux-de-tournage-a-paris.shp",  #Chargement de fichier
                        crs = 4326,
                        stringsAsFactors = F)

movies_paris <- st_transform(movies_paris, 2154) #Changement de projection

idf_sf <- st_read(dsn = "data/parispc_com.shp",
                  crs = 2154, 
                  stringsAsFactors = F) #Chargement du découpage communale

paris <- filter(idf_sf, NOM_DEPT == "PARIS") #Filtrage des arrondissements de Paris

#1 : le nombre de tournages par arrondissement en 2017 ;

nombre_tournage_2017 <- movies_paris %>%
  filter(annee_tourn == '2017') %>% 
  group_by(ardt_lieu) %>%
  summarise(total=n())

png("Output/tournages_2017.png") #Création de la carte

plot(st_geometry(paris), #fond de carte avec les arrondissements 
     col="ivory",
     border = "ivory3")

propSymbolsLayer(nombre_tournage_2017, 
                 var = "total", #variable choisie 
                 legend.pos = "topleft", #légende en haut à gauche
                 inches = 0.15, #taille du plus gros symbole
                 legend.style = "e", #légende "étendue"
                 legend.title.txt = "Nombre de tournages", #titre de la légende
                 add = T) #ajouter au fond de carte précédent
#Ajout de la variable cartographiée 


layoutLayer(title = "Tournages à Paris par arrondissement en 2017",#titre de la carte 
            author = "Auteur: B. Frel-Cazenave",#nom de l'auteur
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)
#Ajout de la mise en page 

dev.off() #Enregistrement de la carte 





#2 : le nombre de tournages par arrondissement en 2018 

nombre_tournage_2018 <- movies_paris %>%
  filter(annee_tourn == '2018') %>% 
  group_by(ardt_lieu) %>%
  summarise(total=n())

png("Output/tournages_2018.png") #Création de la carte

plot(st_geometry(paris), #fond de carte avec les arrondissements 
     col="ivory",
     border = "ivory3")

propSymbolsLayer(nombre_tournage_2018, 
                 var = "total", #variable choisie 
                 legend.pos = "topleft", #légende en haut à gauche
                 inches = 0.15, #taille du plus gros symbole
                 legend.style = "e", #légende "étendue"
                 legend.title.txt = "Nombre de tournages", #titre de la légende
                 add = T) #ajouter au fond de carte 
#Ajout de la variable cartographiée 


layoutLayer(title = "Tournages à Paris par arrondissement en 2018",
            author = "Auteur: B. Frel-Cazenave",
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)
#Ajout de la mise en page 

dev.off() #Enregistrement de la carte




#3 : une même carte qui montre les localisations de la saison 8 d'Engrenages ET la série Emily in Paris.


Engrenages_Emily <- filter(movies_paris, nom_tournag == "Emily in Paris" | nom_tournag == "ENGRENAGES SAISON 8") #Sélection des tournages de Emily in Paris et d'Engrenages Saison 8

Engrenages_Emily <- mutate(Engrenages_Emily, Tournage = if_else(nom_tournag == "ENGRENAGES SAISON 8", "Engrenages", "Emily in Paris"))

png("output/Engrenages_Emily.png")
 
plot(st_geometry(paris), #fond de carte avec les arrondissements 
     col="ivory",
     border = "ivory3")


typoLayer(Engrenages_Emily,
          var = "Tournage", 
          col = viridis(2), 
          legend.title.txt = "Lieux de tournage par série", 
          legend.pos = "topleft",
          add = T)

layoutLayer(
  title = "Tournages des séries d'Engrenages et d'Emily in Paris", 
  author = "B. Frel-Cazenave") #Mise en page
                 
dev.off() #Enregistrement de la carte
                 

# COMMENTAIRE : Mieux vaut utiliser la fonction propSymbolTypolayer, le rendu visuel est meileure;, 
# et vous pouvez paramétrer la taille des cercles !
# Comme suit : 
Engrenages_Emily$Map <- 1

plot(st_geometry(paris), #fond de carte avec les arrondissements 
     col="ivory",
     border = "ivory3")

propSymbolsTypoLayer(Engrenages_Emily, 
                     # 1ere variable
                     var = "Map",
                     inches = 0.08,
                     legend.var.pos = "none",
                     # 2e variable
                     var2 = "Tournage",
                     legend.var2.pos = "topleft",
                     col = viridis(2))


#####EXERCICE 2#####

#1: une carte typologique qui montre le vainqueur dans chaque état ;


us_election <-  st_read("data/us_elections.gpkg") #Chargement de fichier

us_election <- mutate(us_election, vainqueur = if_else(Biden > Trump, "Biden", "Trump")) #Création d'une colonne avec le candidat ayant remporté le plus de voix 

# ATTENTION : mieux vaut écrire le nom des variables avec une Majuscule. vainqueur = Vainqueur. 

png("Output/Candidatarrivéen1.png") #Création de la carte

plot(st_geometry(us_election), #fond de carte avec les états 
     col="ivory",
     border = "ivory3")

typoLayer(us_election,
          var = "vainqueur", #Variable utilisée
          col = c("blue","red"), #Choix de la couleur
          legend.title.txt = "Candidat arrivé en tête", #Titre de la légende 
          legend.pos = "topleft", #position de la légende
          add = T) #Ajouter au fond de carte existant 

layoutLayer(
  title = "Candidat arrivé en tête lors de l'élection présidentielle américaine en 2020", 
  # Titre un peu long !! "Les élections américaines en 2020 ?"
  author = "B. Frel-Cazenave")#Mise en page

dev.off()#Enregistrement de la carte


#2 : une carte montrant le nombre de votes obtenus par Biden dans chaque état ;


png("Output/Bidden.png") #Création de la carte

plot(st_geometry(us_election), #fond de carte avec les états 
     col="ivory",
     border = "ivory3")

propSymbolsLayer(us_election, 
                 var = "Biden", #variable choisie 
                 legend.pos = "topleft", #légende en haut à gauche
                 inches = 0.15, #taille du plus gros symbole
                 legend.style = "e", #légende "étendue"
                 fixmax = max(us_election$Biden),
                 legend.title.txt = "Nombre de voix pour Biden", #titre de la légende
                 add = T)

layoutLayer(
  title = "Voix pour Biden par Etat lors de l'élection présidentielle 2020", 
  author = "B. Frel-Cazenave")#Mise en page

dev.off() #Enregistrement de la carte


#3 : une carte montrant le nombre de votes obtenus par Trump dans chaque état ;

png("Output/Trump.png") #Création de la carte

plot(st_geometry(us_election), #fond de carte avec les arrondissements 
     col="ivory",
     border = "ivory3")

?propSymbolsLayer
propSymbolsLayer(us_election, 
                 fixmax = max(us_election$Biden), #pour pouvoir comparer les cartes, voir explication dans l'aide
                  
                 var = "Trump", #variable choisie 
                 legend.pos = "topleft", #légende en haut à gauche
                 inches = 0.15, #taille du plus gros symbole
                 legend.style = "e", #légende "étendue"
                 legend.title.txt = "Nombre de voix pour Trump", #titre de la légende
                 add = T)


layoutLayer(title = "Voix pour Trump par Etat lors de l'élection présidentielle 2020",
  author = "B. Frel-Cazenave") #Mise en page
  
  dev.off() #Enregistrement de la carte
  
  
# COMMENTAIRE
# Bon travail. Vous n'avez néanmoins pas repéré qu'il y avait deux codes pour le 16e arrondissement, ce qui entraîne un doublon ur la carte. 
  # De plus, les cercles proportionnels ne sont pas représentés au centre du polygone. Mais les fonctions et la démarche ont l'air d'être comprises, 
  # et c'est l'essentiel. Pensez à indiquez vos sources sur toute carte. 
  