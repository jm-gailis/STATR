# ouverture des packages necessaires aux exercices
library(sf)
library(tidyverse)
library(RColorBrewer)
library(cartography)
library(mapview)
library(viridisLite)
library(raster)
library(sp)


#ouverture des donnees
movies_paris <- st_read(dsn= "Data/lieux de tournage/lieux-de-tournage-a-paris.shp", 
                        crs = 4326,
                        stringsAsFactors = F) %>%
  st_transform(2154)

idf_sf <- st_read(dsn= "Data/idf/parispc_com.shp", 
                  crs = 2154,
                  stringsAsFactors = F)

#creation d'un fond de carte
paris <- idf_sf %>% filter(NOM_DEPT == "PARIS")
#Télécharger un fond de carte avec la fonction getTiles
parisOSM <- getTiles(x = paris,
                      type = "CartoDB.Positron", 
                      crop = TRUE) 

# carte numero 1: nb de tournages par arrondissement en 2017

# ETAPE 1 : creation du tableau elementaire
tournages_2017 <- movies_paris %>% 
  filter(annee_tourn == 2017) %>% # on filtre sur l'année 2017
  group_by(ardt_lieu) %>% # On regroupe par arrondissement
  summarise(total = n())  %>% # Calcul de la fréquence
  arrange(desc(total)) %>% #classer le dataframe par ordre décroissant
  mutate(Tournage = as.numeric(total))


#ici, il faudrait additionner le doublon qui existe sur le nb de tournages dans le 16E.
#comment faire ? J'y reviens ensuite.

# Avant de cartographier, on place en haut la fonction "pdf" pour sauvegarder
pdf("C:/Users/louka/Documents/Outils en geo/R/Exos_Confines_1/Confined_Project/Output/Carte_Tournages_ardt_2017.pdf", # Emplacement et nom du fichier
    width=7, #largeur en pouces # COMMENTAIRE : est ce le format qu'il vous faut ???
    height=7 , #hauteur en pouces
    useDingbats=FALSE)

# COMMENTAIRE : pensez à définir votre espace de travail avec setwd(). Cela vous évitera d'écrire ce long chemin.
# setwd(C:/Users/louka/Documents/Outils en geo/R/Exos_Confines_1)
# Evitez aussi les espaces dans les noms de dossier. Outils en geo = OUTILS_GEO

parisOSM <- mask(parisOSM, paris) #clipper le raster

# ETAPE 1 : FOND DE CARTE !!!
tilesLayer(parisOSM)

# ETAPE 2 : production de la carte
plot(st_geometry(paris %>% filter(NOM_DEPT == "PARIS")),
     col= NA, #couleur fond de carte
     border="grey", #couleur bordure
     bg = "white", #couleur du fond
     lwd = 1,
     add = T)  #La fonction ADD = T doit être utilisée lorsque un élément graphique a déjà été ajouté,
# ce qui n'est pas le cas ici. Donc soit vous enlever cet argument, soit vous mettez add = F

propSymbolsLayer(tournages_2017, 
                 var = "Tournage", #nom de la variable 
                 legend.pos = "topleft", #position de la légende
                 inches = 0.09, #taille du plus gros symbole
                 legend.style = "e", #légende "étendue"
                 col = "springgreen4", 
                 legend.title.txt = "Nombre de tournages", #titre de la légende
                 legend.title.cex = 0.6,
                 legend.values.cex = 0.45,
                 add = T) #ajouter au fond de carte

layoutLayer(title = "Répartition des tournages à Paris en 2017", 
            author = "Auteur: L. Herse",
            sources = "Source : Mairie de Paris, 2018",
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)

# On clôture la fonction "pdf" ouverte plus haut
dev.off()





# CARTE NUMERO 2 
# Le nombre de tournages par arrondissement en 2018
# Copier-Coller de la portion du script ci-dessous 
# en substituant 2018 à 2017 dans tous les morceaux de code


# ETAPE 1 : creation du tableau elementaire
tournages_2018 <- movies_paris %>% 
  filter(annee_tourn == 2018) %>% # on filtre sur l'année 2018
  group_by(ardt_lieu) %>% # On regroupe par arrondissement
  summarise(total = n())  %>% # Calcul de la fréquence
  arrange(desc(total)) %>% #classer le dataframe par ordre décroissant
  mutate(Tournage = as.numeric(total))


#ici, il faudrait additionner le doublon qui existe sur le nb de tournages dans le 16E.
#comment faire ? J'y reviens ensuite.

# Etape 2: production de la carte
#Avant de cartographier, on place en haut la fonction "pdf" pour sauvegarder
pdf("C:/Users/louka/Documents/Outils en geo/R/Exos_Confines_1/Confined_Project/Output/Carte_Tournages_ardt_2018.pdf", # Emplacement et nom du fichier
    width=7, #largeur en pouces
    height=7 , #hauteur en oouces
    useDingbats=FALSE)


tilesLayer(parisOSM)

# Travail de cartographie
plot(st_geometry(paris %>% filter(NOM_DEPT == "PARIS")),
     col= NA, #couleur fond de carte
     border="grey", #couleur bordure
     bg = "white", #couleur du fond
     lwd = 1,
     add = T)  #La fonction ADD = T doit être utilisée lorsque un élément graphique a déjà été ajouté,
# ce qui n'est pas le cas ici. Donc soit vous enlever cet argument, soit vous mettez add = F

propSymbolsLayer(tournages_2018, 
                 var = "Tournage", #nom de la variable 
                 legend.pos = "topleft", #position de la légende
                 inches = 0.09, #taille du plus gros symbole
                 legend.style = "e", #légende "étendue"
                 col = "springgreen4", 
                 legend.title.txt = "Nombre de tournages", #titre de la légende
                 legend.title.cex = 0.6,
                 legend.values.cex = 0.45,
                 add = T) #ajouter au fond de carte

layoutLayer(title = "Répartition des tournages à Paris en 2018", 
            author = "Auteur: L. Herse",
            sources = "Source : Mairie de Paris, 2018",
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)

# On clôture la fonction "pdf" ouverte plus haut
dev.off()




# CARTE NUMERO 3
#une carte qui montre les localisations de la saison 8 d'Engrenages ET la série Emily in Paris.


# etape 1: création des tableaux élémentaires nécessaires à la cartographie

#Pourquoi les deux lignes ci-dessous ne peuvent-elles fonctionner?
#TE <- tvshowsparis %>%
#  filter(nom_tournag == "Emily in Paris" & nom_tournag=="ENGRENAGES SAISON 8")

# COMMENTAIRE : parce que vous demandez à la fonction filter de sélectionne les individus  
# qui ont A LA FOIS "Emily" et "Engrenage" comme nom de tournages".
# Il n'y a donc aucun individu statistique qui répond à ces conditions. 
# Au lieu d'utiliser "&" (qui équivaut à ET), utilisez "|", qui équivaut à "OU".


# On filtre les tournages des deux series a cartographier
Essai <- movies_paris %>% #choix objet du pipeline
  filter(type_tourna == "Série TV") %>%
  mutate(Tournage = case_when(
    nom_tournag == "ENGRENAGES SAISON 8" ~ "Engrenages S8",
    nom_tournag == "Emily in Paris" ~ "Emily in Paris", 
  ))%>%
  mutate (Map=1)
#COMMENTAIRE : Bonne utilisation de la fonction

#ouverture de la fonction pdf()
#Avant de cartographier, on place en haut la fonction "pdf" pour sauvegarder
pdf("C:/Users/louka/Documents/Outils en geo/R/Exos_Confines_1/Confined_Project/Output/Carte_Emily & Engrenages.pdf", # Emplacement et nom du fichier
    width=7, #largeur en pouces
    height=7 , #hauteur en oouces
    useDingbats=FALSE)

#On affiche le fond de carte predecoupe
tilesLayer(parisOSM)

#on cartographie enfin
plot(st_geometry(paris %>% filter(NOM_DEPT == "PARIS")),
     col=NA, #couleur fond de carte 
     border="black", #coueur bordure 
     bg = "white", #couleur du fond
     lwd = 1, 
     add = T) #largeur bordure

propSymbolsTypoLayer(Essai,
                     var = "Map",
                     var2 = "Tournage",
                     inches = 0.05,
                     col = viridis(2),
                     colNA = NA,
                     border = NA, #COMM = une bordure en noir ne serait pas mal là. Carte plus lisible
                     legend.var.pos = "none",  
                     add = T)
layoutLayer(title = "Le Paris d'Emily et d'Engrenages", 
            author = "Auteur: L. Herse",
            sources = "Source : Mairie de Paris, 2018",
            tabtitle = T,
            frame = TRUE)


#fermeture de la fonction pdf()
dev.off()


#EXERCICES SUITE
#CARTE N4 
#une carte typologique qui montre le vainqueur dans chaque état ;

# appeler les packages necessaires
library(rgdal)


#step 1: ouvrir les donnees necessaires et créer un tableau élémentaire
us_elections <- st_read(dsn = "C:/Users/louka/Documents/Outils en geo/R/Exos_Confines_1/Confined_Project/Data/us_elections.gpkg",
                        crs = 2163,
                        stringsAsFactors = F) %>%
  mutate(Vainqueur = if_else(Biden > Trump, "Biden", "Trump" ))%>%
  mutate (Map=1)

#step 2: cartography
# fonction pdf
pdf("C:/Users/louka/Documents/Outils en geo/R/Exos_Confines_1/Confined_Project/Output/Vote_majoritaire_Etat.pdf",
    width=7, #largeur en pouces
    height=7, #hauteur en pouces
    useDingbats=FALSE)

#différentes recherches pour ajouter un fond de carte, sans succès...
#getTiles(x = ???,
#         type = "CartoDB.Positron", 
#         crop = TRUE)

plot(st_geometry(us_elections),
     col="white", #couleur fond de carte 
     border="black", #coueur bordure 
     bg = "white", #couleur du fond
     lwd = 1, 
     add = F) #largeur bordure

# COMMENTAIRE : erreur dans la fonction. Vous représentez une variable catégorielle sur des polygones, 
# Donc la fonction est typoLayer().

propSymbolsTypoLayer(us_elections,
                     var = "Map",
                     var2 = "Vainqueur",
                     inches = 0.05,
                     col = viridis(2),
                     legend.var.pos = "none",  
                     add = T)
layoutLayer(title = "Le vote majoritaire par Etat aux élections présidentielles 2020", 
            # COM : titre bizarre. Pourquoi pas "Candidat vainqueur par état?"
            author = "Auteur: L. Herse",
            sources = "Source : J. Migozzi, 2018", #C'est gentil, mais c'est la BBC. J'aurai du le préciser en effet.
            tabtitle = T,
            frame = TRUE)                 
                                                                                                       
dev.off()                                                                                                    





# CARTE N5
#une carte montrant le nombre de votes obtenus par Biden dans chaque état 

#step 1: creation d'un tableau elementaire
us_elections_Biden <- us_elections %>% #choisir l'objet premier du pipeline
  mutate(Ratio_Biden = (100* Biden/ (Biden + Trump))) %>% #calculer le pourcentage de votes pour Biden parmi les votants (votes blancs non-comptés)
  mutate(Ratio_Trump = (100* Trump/ (Biden + Trump))) %>% #faire de même pour Trump
  mutate(Votes_pro_Biden = case_when(    #créer des catégories pour la cartographie
    Ratio_Biden < 20 ~ "0-20%",
    Ratio_Biden > 20 & Ratio_Biden < 40 ~ "20-40%",
    Ratio_Biden > 40 & Ratio_Biden < 60 ~ "40-60%",
    Ratio_Biden > 60 & Ratio_Biden < 80 ~ "60-80%",
    Ratio_Biden > 80 ~ "80-100%",
  )) %>% 
  mutate(Votes_pro_Trump = case_when(     #créer des catégories pour la cartographie
    Ratio_Trump < 20 ~ "0-20%",
    Ratio_Trump > 20 & Ratio_Trump < 40 ~ "20-40%",
    Ratio_Trump > 40 & Ratio_Trump < 60 ~ "40-60%",
    Ratio_Trump > 60 & Ratio_Trump < 80 ~ "60-80%",
    Ratio_Trump > 80 ~ "80-100%",
  ))

# COMMENTAIRE : tentative très appréciable pour utilier la fonction mutate, et la volonté de discrétiser une variablel continue. 
# Mais la fonction que vous devez utiliser ici est propSymbolLayer. Il faut représenter une variable quantitative en stock, et pas en proportion.
# Vous pouvez donc appliquer la fonction choroLayer pour réaliser la carte que vous avez en tête. 



#step 2: cartography
# fonction pdf
pdf("C:/Users/louka/Documents/Outils en geo/R/Exos_Confines_1/Confined_Project/Output/Vote_Biden_Etat.pdf",
    width=7, #largeur en pouces
    height=7, #hauteur en pouces
    useDingbats=FALSE)

#différentes recherches pour ajouter un fond de carte, sans succès... 
# COMMENTAIRE : est-il utile ce fond de carte ? 
#getTiles(x = ???,
#         type = "CartoDB.Positron", 
#         crop = TRUE)

#cartographier le vote pro-biden par Etat 
plot(st_geometry(us_elections_Biden),
     col="white", #couleur fond de carte 
     border="black", #coueur bordure 
     bg = "white", #couleur du fond
     lwd = 1, 
     add = F) #largeur bordure

propSymbolsTypoLayer(us_elections_Biden,
                     var = "Map",
                     var2 = "Votes_pro_Biden",
                     inches = 0.05,
                     col = rainbow(5),
                     legend.var.pos = "none",  
                     add = T)
layoutLayer(title = "Le vote pro-Biden par Etat en pourcentage de votes", 
            author = "Auteur: L. Herse",
            sources = "Source : J. Migozzi, 2018",
            tabtitle = T,
            frame = TRUE)                 

dev.off()             






# une carte montrant le nombre de votes obtenus par Trump dans chaque état ;

pdf("C:/Users/louka/Documents/Outils en geo/R/Exos_Confines_1/Confined_Project/Output/Vote_Trump_Etat.pdf",
    width=7, #largeur en pouces
    height=7, #hauteur en pouces
    useDingbats=FALSE)

#cartographier le vote pro-Trump par Etat 
plot(st_geometry(us_elections_Biden),
     col="white", #couleur fond de carte 
     border="black", #coueur bordure 
     bg = "white", #couleur du fond
     lwd = 1, 
     add = F) #largeur bordure

propSymbolsTypoLayer(us_elections_Biden,
                     var = "Map",
                     var2 = "Votes_pro_Trump",
                     inches = 0.05,
                     col = rainbow(5),
                     legend.var.pos = "none",  
                     add = T)
layoutLayer(title = "Le vote pro-Trump par Etat en pourcentage de votes", 
            author = "Auteur: L. Herse",
            sources = "Source : J. Migozzi, 2018",
            tabtitle = T,
            frame = TRUE)                 



dev.off()  

# COMMENTAIRE : une volonté de tester des choses, d'aller plus loin dans l'utilisation des fonctions = c'est de très bon augure 
# et c'est comme cela qu'on apprend à programmer. 
# Mais vous vous mélangez les pinceaux dans le choix des variables visuelles et le type de variables à cartographier, ce qui 
# vous mets en difficulté technique pour la 2e partie de l'exercice. 