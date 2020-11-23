#----------------------------#
#       DEXTREIT Natalia
#------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(mapview)
library(cartography)

#----------------------------#
#          Exercice 1 -----
#----------------------------#

# A partir des données proposées par la municipalité de Paris, proposez trois cartes. 

movies_paris <- st_read(dsn="data/lieux-de-tournage-a-paris.shp",crs = 4326, stringsAsFactors=F) # ouverture du fichier
movies_paris <- st_transform(movies_paris, 2154) # dans la bonne projection 

paris <- st_read(dsn="data/arrondissements/arrondissements.shp") # ouverture d'un fichier avec des polygones pour les arrondissements
paris <- st_transform(paris, 2154) # dans la bonne projection
mapview(paris)

# 1) Carte représentant le nombre de tournages par arrondissement en 2017

movies_paris_2017 <- movies_paris %>%
    filter(annee_tourn=="2017" ) %>%
    group_by(annee_tourn,ardt_lieu) %>%
    summarize(nb_tournages=n())  # affiche le nombre de tournages par arrondissements en 2017

pdf("output/Carte_tournages_2017.pdf", # Emplacement et nom du fichier
    width=7, #largeur en pouces
    height=7 , #hauteur en pouces
    useDingbats=FALSE)

plot(st_geometry(paris, add=TRUE), 
     col="beige",#fond de carte
     border = "ivory4") #bordures

propSymbolsLayer(movies_paris_2017, 
                 var = "nb_tournages", #nom de la variable 
                 legend.title.txt = "Nombre de tournages", #titre de la légende
                 legend.title.cex = 0.7, # taille du titre de la légende
                 legend.values.cex = 0.7, # taille des valeurs dans la légende
                 legend.pos = "topleft", #position de la légende
                 inches = 0.15, #taille du plus gros symbole
                 col = "cyan4", # couleur des symboles 
                 legend.style = "e", #légende "étendue"
                 add = T) #ajouter au fond de carte

layoutLayer(title = "Carte du nombre de tournages réalisés en 2017 à Paris par arrondissement", #titre de la carte
            author = "Auteur : Natalia Dextreit, 2020", #auteur
            sources = "Source : Open Paris Data", #source et données 
            col = "black",  # couleur du fond du titre
            coltitle = "white", # couleur du titre
            tabtitle = F, 
            postitle = "center", # positionne le titre au centre
            north = TRUE,# affiche la flèche du Nord
            frame = TRUE, 
            scale = "auto",  # affiche une échelle
            posscale = "bottomright") # position de l'échelle

dev.off() # fin de la fonction pdf
# j'ai découvert qu'il y avait deux codes postaux dans le 16ème arrondissement (75016 et 75116),
# mais comme ils correspondent au Nord et Sud du 16ème et que c'est compréhensible sur la carte, je les ai gardés.
# COMMENTAIRE : OUI, mais du coup la carte comporte une erreur. On aurait pu recoder cette erreur avec un mutate et un ifelse !
# COMMENTAIRE : le titre est trop long. Pas besoin de mettre "Carte". On sait que cela en est une !


#------------------------------------------------------------------------------------------------
# 2) Carte représentant le nombre de tournages par arrondissement en 2018 

movies_paris_2018 <- movies_paris %>%
    filter(annee_tourn=="2018" ) %>%
    group_by(annee_tourn,ardt_lieu) %>%
    summarize(nb_tournages=n()) # affiche le nombre de tournages par arrondissements en 2018

pdf("output/Carte_tournages_2018.pdf", # Emplacement et nom du fichier
    width=7, #largeur en pouces
    height=7 , #hauteur en pouces
    useDingbats=FALSE)

plot(st_geometry(paris), 
     col="beige",#fond de carte
     border = "ivory4") #bordures

propSymbolsLayer(movies_paris_2018, 
                 var = "nb_tournages", #nom de la variable 
                 legend.title.txt = "Nombre de tournages", #titre de la légende
                 legend.title.cex = 0.7, # taille du titre de la légende
                 legend.values.cex = 0.7, # taille des valeurs dans la légende
                 legend.pos = "topleft", #position de la légende
                 inches = 0.15, #taille du plus gros symbole
                 col = "cyan3", # couleur des symboles 
                 legend.style = "e", #légende "étendue"
                 add = T) #ajouter au fond de carte

layoutLayer(title = "Carte du nombre de tournages réalisés en 2018 à Paris par arrondissement", #titre de la carte
            author = "Auteur : Natalia Dextreit, 2020", #auteur
            sources = "Source : Open Paris Data", #source et données 
            col = "black",  # couleur du fond du titre
            coltitle = "white", # couleur du titre
            tabtitle = F, 
            postitle = "center", # positionne le titre au centre
            north = TRUE,# affiche la flèche du Nord
            frame = TRUE, 
            scale = "auto",  # affiche une échelle
            posscale = "bottomright") # position de l'échelle

dev.off() # fin de la fonction pdf

#--------------------------------------------------------------------------------------------------------
# 3) une même carte qui montre les localisations de la saison 8 d'Engrenages ET la série Emily in Paris.

Loc_tournages <- movies_paris %>% 
    filter(nom_tournag == "Emily in Paris" | nom_tournag =="ENGRENAGES SAISON 8") %>%
    mutate(Tournage = if_else(nom_tournag == "Emily in Paris","Emily in Paris","Engrenages")) %>% # création d'une nouvelle variable qualitative à deux modalités
    st_transform(2154) %>% 
    mutate(Map = 1) #Nouvelle variable quanti

pdf("output/Carte_localisation_tournages_Engrenages8_et_Emily.pdf", # Emplacement et nom du fichier
    width=9.5, #largeur en pouces
    height=8 , #hauteur en pouces
    useDingbats=FALSE)

plot(st_geometry(paris), 
     col="beige",#fond de carte
     border = "ivory4") #bordures

propSymbolsTypoLayer(Loc_tournages,
                     var = "Map", # variable numérique des symboles
                     var2 = "Tournage", # variable qualitative pour la couleur des symboles
                     col = c("darkgreen","coral2"), # couleur des symboles
                     legend.var.pos = "none", #position de la légende
                     legend.var2.pos = "topleft", #position de la légende
                     legend.var2.title.txt = "Nom de la série tournée", #titre de la légende
                     inches = 0.07, #taille du plus gros symbole
                     add = T) #ajouter au fond de carte)

layoutLayer(title = "Carte de localisation des tournages réalisés à Paris de la 8ème saison de la série Engrenages et de la série Emily in Paris", #titre de la carte
            author = "Auteur : Natalia Dextreit, 2020", #auteur
            sources = "Source : Open Paris Data", #source et données 
            col = "white",  # couleur du fond du titre
            coltitle = "black", # couleur du titre
            tabtitle = T, 
            postitle = "center", # positionne le titre au centre
            north = TRUE,# affiche la flèche du Nord
            frame = TRUE, 
            scale = "auto",  # affiche une échelle
            posscale = "bottomright") # position de l'échelle

dev.off() # fin de la fonction pdf

# CMMENTAIRE  :idem, titre trop long !!


#----------------------------#
#          Exercice 2 -----
#----------------------------#

# A partir des données sur les élections américaines de 2020, proposez trois cartes. 

USA_elections_2020 <- st_read(dsn="data/us_elections.gpkg",stringsAsFactors=F) # ouverture du fichier 

# 1) une carte typologique qui montre le vainqueur dans chaque état 

USA_elections_2020 <- mutate(USA_elections_2020,winner = if_else(Biden>Trump,
                                                                 "Biden",
                                                                 "Trump")) # création d'une variable qualitative pour le vainqueur

pdf("output/Carte_winner_USA_elections_states.pdf", # Emplacement et nom du fichier
    width=6, #largeur en pouces
    height=5.5, #hauteur en pouces
    useDingbats=FALSE)

plot(st_geometry(USA_elections_2020), 
     col="ivory",#fond de carte
     border = "ivory3") #bordures

# COMMENTAIRE : un fond de carte n'est pas nécessaire ici, puisque vous mettez une carte typo par dessus !

typoLayer(USA_elections_2020, 
                 var = "winner", #nom de la variable 
                 col = c("red3", "royalblue3"),
                 legend.pos = "topleft", #position de la légende
                 legend.title.txt = "Vainqueur", #titre de la légende
                 legend.title.cex = 0.7, # taille du titre de la légende
                 legend.values.cex = 0.6, # taille des valeurs dans la légende
                 add = T) #ajouter au fond de carte

layoutLayer(title = "Vainqueur par état des élections présidentielles américaines de 2020", #titre de la carte
            author = "Auteur : Natalia Dextreit, 2020", #auteur
            # sources = "Source :", # mais je ne connais pas ici la source de vos données...
            col = "black", # couleur du fond du titre
            coltitle = "white", # couleur du titre
            tabtitle = F, 
            postitle = "center", # positionne le titre au centre
            north = TRUE, # affiche la flèche du Nord
            frame = TRUE, 
            scale = "auto", # affiche une échelle
            posscale = "bottomright") # position de l'échelle 

dev.off() # fin de la fonction pdf

#-------------------------------------------------------------------------------------
# 2) une carte montrant le nombre de votes obtenus par Biden dans chaque état 

pdf("output/Carte_votes_Biden_USA_elections.pdf", # Emplacement et nom du fichier
    width=7.5, #largeur en pouces
    height=7, #hauteur en pouces
    useDingbats=FALSE)

plot(st_geometry(USA_elections_2020), 
     col="ivory",#fond de carte
     border = "ivory4") #bordures

propSymbolsLayer(USA_elections_2020, 
          var = "Biden", #nom de la variable 
          legend.pos = "topleft", #position de la légende
          legend.title.txt = "Nombre de votes obtenus par Biden par Etat", #titre de la légende
          legend.title.cex = 0.7, # taille du titre de la légende
          legend.values.cex = 0.6, # taille des valeurs dans la légende
          inches = 0.15, #taille du plus gros symbole
          col = "royalblue3", # couleur des symboles 
          legend.style = "e", #légende "étendue"
          add = T) #ajouter au fond de carte

layoutLayer(title = "Résultats de Biden aux élections présidentielles américaines de 2020", #titre de la carte
            author = "Auteur : Natalia Dextreit, 2020", #auteur
            # sources = "Source :", # mais je ne connais pas ici la source de vos données...
            col = "black",  # couleur du fond du titre
            coltitle = "white", # couleur du titre
            tabtitle = F, 
            postitle = "center", # positionne le titre au centre
            north = TRUE,# affiche la flèche du Nord
            frame = TRUE, 
            scale = "auto",  # affiche une échelle
            posscale = "bottomright") # position de l'échelle

dev.off() # fin de la fonction pdf

#-------------------------------------------------------------------------------------
# 3) une carte montrant le nombre de votes obtenus par Trump dans chaque état 

pdf("output/Carte_votes_Trump_USA_elections.pdf", # Emplacement et nom du fichier
    width=7.5, #largeur en pouces
    height=7, #hauteur en pouces
    useDingbats=FALSE)

plot(st_geometry(USA_elections_2020), 
     col="ivory",#fond de carte
     border = "ivory4") #bordures

propSymbolsLayer(USA_elections_2020, 
                 var = "Trump", #nom de la variable 
                 legend.pos = "topleft", #position de la légende
                 legend.title.txt = "Nombre de votes obtenus par Trump par Etat", #titre de la légende
                 legend.title.cex = 0.7, # taille du titre de la légende
                 legend.values.cex = 0.6, # taille des valeurs dans la légende
                 inches = 0.15, #taille du plus gros symbole
                 col = "red3", # couleur des symboles 
                 legend.style = "e", #légende "étendue"
                 add = T) #ajouter au fond de carte

layoutLayer(title = "Résultats de Trump aux élections présidentielles américaines de 2020", #titre de la carte
            author = "Auteur : Natalia Dextreit, 2020", #auteur
            # sources = "Source :", # mais je ne connais pas ici la source de vos données...
            col = "black",  # couleur du fond du titre
            coltitle = "white", # couleur du titre
            tabtitle = F,
            postitle = "center", # positionne le titre au centre
            north = TRUE,# affiche la flèche du Nord
            frame = TRUE, 
            scale = "auto",  # affiche une échelle
            posscale = "bottomright") # position de l'échelle

dev.off() # fin de la fonction pdf

# Un script clair et des cartographies correctes. Optez pour des titres plus courts. 
# Pensez à regarder les détails des fonctions. Ainsi, la rubrique d'aide de 
# propSymbolsLayer précise comment comparer deux cartes de stock, ce qui permettrait de visualiser les votes 
# pour Trump et Biden avec une légende similaire. 
# Pour accéder à la rubrique d'aide, il suffit de mettre un ? avant la fonction
?propSymbolsLayer

