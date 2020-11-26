

# Je vous ai fourni un script annoté : pourquoi ne pas l'utiliser ? 

library(sf)
library(tidyverse)
library(dplyr)
library(mapview) #chargement des différents packages necessaires au vu de l'enonce

#####Exercice question 1#####

murders <- read.csv("data/murders.csv", stringsAsFactors = F) #ouverture du fichier 


murders <- read.csv(file = "statsmappingwithR/01_Introduction/data/murders.csv",
        header = TRUE,
        sep = ",")

murders_reg <- murders %>% 
  group_by(region) %>% #regroupement des lignes par region 
  summarise(total = sum(total))#somme des meurtres par région

murders_pop <- murders %>% 
  group_by(region) %>% #regroupement des lignes par region 
  summarise(population = sum(population))#somme de la population par region

murders_reg <- left_join(murders_reg, #jointure de la colonne population sur le dataframe murders_reg
                    murders_pop, 
                    by = "region")

## COMMENTAIRE : c'est complexe tout cela ! La jointure n'est pas nécessaire, car vous n'avez pas besoin 
# de faire deux objets différents. Vous pouvez tout faire en un mouvement comme suit : 

# SOLUTION
murders_reg <- murders %>% #objet de départ
  group_by(region) %>% # on regroupe nos lignes par région
  summarise(TotalMurders = sum(total), #calcul du total de meurtres par région
            TotalPopulation = sum(population)) %>%         #calcul du total de population par région
  mutate(Ratio = TotalMurders / (TotalPopulation / 100000)) #calcul taux pour 100 000


# Le reste est ok

murders_reg #visualisation du nombre de meutres total par region 


murders_reg$pop_tx <- murders_reg$population/100000 
murders_reg$Ratio <- murders_reg$total/murders_reg$pop_tx #calcul du ratio meurtres/population

murders_reg # visualisation du ratio par region 



######Exercice question 2#####

idf <- st_read(dsn = "data/parispc_com.shp", 
                  crs = 2154, 
                  stringsAsFactors = F) #Ouverture du fichier

dpt_sf <- idf %>%
  group_by(NOM_DEPT) %>% #regroupe communes par departement
  summarise(POPULATION = sum(POPULATION),
            SUPERFICIE = sum(SUPERFICIE)) #additionne population et superficie par departement
        
dpt_sf <- mutate(dpt_sf, DENSITE = POPULATION*1000/SUPERFICIE*100) #calcul de la densite de population par km2. On multiplie la population par 1000 car elle est exprimee en milliers et la superficie par 100 car elle est exprimee en hectares

mapview(dpt_sf, zcol="DENSITE") #Visualisation cartographique de la densite de population par departement



#####DM#####

##QUESTION 1 Ouverture du fichier

movies_paris <- st_read(dsn = "data/lieux-de-tournage-a-paris.shp", 
                        crs = 4326, 
                        stringsAsFactors = F) #Lecture du fichier



##QUESTION 2 Reprojection dans une projection adaptee

st_transform(movies_paris, 2154) #Passage du CRS en Lambert-93



##QUESTION 3 Description du tableau de donnees

nrow(movies_paris) #Nombre d'individus correspond ici au nombre de lignes soit 7742

class(movies_paris) #Types de variable : décricvez les ! 
# Rappel : la fonction glimpse est plus pratique. 


##QUESTION 4 Combien de lieux de tournage sont associés à une série Web

x <- filter(movies_paris, type_tourna == "Série Web") #on ne filtre que les tournages associes a des series web. 
nrow(x) # il y a 282 lieux de tournages associes à une serie web

# # SOLUTION : une fonction table, qui calcule fréquence de chaque modalité. Bien plus rapide !
table(movies_paris$type_tourna)
# COMMENTAIRE : il y'en a 283.



##QUESTION 5 Créez un data.frame qui renseigne sur le nombre de tournages realises par boite de production

movies_produc <- movies_paris %>%
  group_by(nom_product) %>% #groupe les lignes par nom de la boite de production 
  summarise(Nonbre_tournage = n()) #compte le nombre de tournage par boite de production 

# COMMENTAIRE : c'est compliqué ! 
# SOLUTION  : un table suffit !
movies_produc <- data.frame(table(movies_paris$nom_product))

##QUESTION 6 Divisez l'objet movies_paris en deux objets distincts = OK

tvshowsparis <- filter(movies_paris, type_tourna == "Série TV") #Création d'un dataframe pour les series
longparis <- filter(movies_paris, type_tourna == "Long métrage") #Création d'un dataframe pour les longs metrages



##QUESTION 7 Quel est l'arrondissement de Paris qui accueille le plus de tournages en 2016 ? 

movies_annee <- movies_paris %>%
  group_by(annee_tourn, ardt_lieu) %>%
  summarise(Nombre = n()) #Creation d'un dataframe avec le nombre de tournages par arrondissement et par annee

movies_2016 <- filter(movies_annee, annee_tourn == "2016") #Creation d'un dataframe pour l'annee 2016
max.nb.tournage <- max(movies_2016$Nombre) #Creation d'un objet max de l'année 2016


filter(movies_2016, Nombre == max.nb.tournage) %>% #filtrage permettant de determiner l'arrondissement accueullant le plus de tournage en 2016 
  select(ardt_lieu) %>% 
  st_set_geometry(NULL) #suppression de la geometry pour n'afficher que le resultat souhaite


# COMMENTAIRE : ok, mais c'est se compliquer la vie en créant bcp d'objets intermédiaires. 
# SOLUTION 
tournages_2016 <- movies_paris %>% 
  filter(annee_tourn == 2016) %>% # on filtre sur l'année 2016
  group_by(ardt_lieu) %>% # On regroupe par arrondissement
  summarise(total = n())  %>% # Calcul de l'occurence
  arrange(desc(total)) #classer par ordre décroissant
tournages_2016

#En 2019 ?

movies_2019 <- filter(movies_annee, annee_tourn == "2019") #meme demarche avec 2019
max.nb.tournage9 <- max(movies_2019$Nombre)

filter(movies_2016, Nombre == max.nb.tournage) %>%
  select(ardt_lieu) %>% 
  st_set_geometry(NULL)

# COMMENTAIE : voir plus haut


##QUESTION 8 Quel est l'arrondissement de Paris qui accueille le plus de long métrage en 2018 ?

longparis_2018 <- filter(longparis, annee_tourn == "2018") #creation d'un dataframe avec les tournages de long metrages en 2018

long_metrage_paris_ard <- longparis_2018 %>%
  group_by(ardt_lieu, nom_tournag) %>% #Cree une ligne par film par occurence d'un arrondissement dans un film
  # COMMENTAIRE : cette ligne au dessus n'est pas nécessaire. 
  group_by(ardt_lieu)%>% #compter le nombre de film dans l'arrondissement pour compter le nombre de ligne
  summarize(nombre_films = n()) #compter le nombre de lignes


max_LM_ard <- max(long_metrage_paris_ard$nombre_films) #Creation d'un objet max de long metrage par arrondissement

filter(long_metrage_paris_ard, nombre_films == max_LM_ard) %>% #filtrage permettant de determiner l'arrondissement accueullant le plus de long-métrage en 2018
  select(ardt_lieu) %>% #selection, de l'arrondissement
  st_set_geometry(NULL) #suppression de la geometrie pour ne voir que le resultat 

# COMMENTAIRE : c'est de nouveau compliqué
# SOLUTION :

long_metrage_2018 <- movies_paris %>%
  filter(annee_tourn == 2018 & type_tourna == "Long métrage") %>% #on ne conserve que les long métrages tournés en 2018
  group_by(ardt_lieu) %>% #regrouper les arrondissements
  summarise(total = n()) %>% # on compte
  arrange(desc(total)) # ranger tableau
long_metrage_2018


#QUESTION 9 Dans quels arrondissements se déroulent la série "Emily in Paris" ?

EmilyinP <- filter(movies_paris, nom_tournag == "Emily in Paris") #creation d'un dataframe composé uniquement des tournages de la serie "Emily in Paris"

EmilyinP.ard <- EmilyinP %>%
  group_by(ardt_lieu) %>%
  summarise(NombreT = n()) #permet d'avoir la liste des tournages agrégés par arrondissement 

EmilyinP.ard <- select(EmilyinP.ard, -NombreT) #supprime la ligne avec le nombre de tournage

EmilyinP.ard %>% st_set_geometry(NULL) #Permet de ne voir que la liste des arrondissements dans lesquels la serie a été tournee

#COMMENTAIRE : c'est comliqué !
# SOLUTION 
EmilyinP <- movies_paris %>%
  filter(nom_tournag == "Emily in Paris") %>%
  group_by(ardt_lieu) %>%
  summarise(nombre = n())

##QUESTION 10  Proposez une visualisation des lieux de tournage de 8e saison de la série Engrenages. Quels types de localisations et quels arrondissements sont le plus associés à cette série ?

Engrenages <- filter(tvshowsparis, nom_tournag == "ENGRENAGES SAISON 8") # creation d'un dataframe composé uniquement des tournages d'Engrenages

mapview(Engrenages) #Visualisation des lieux de tournages de la série Engrenages

#On remaraque que les lieux de tournages sont concentrés dans les 17, 18, 19, et 20e arrondissement. 
#On peut expliquer la présence du 17e arrondissement par le tournage dans le nouveau Tribunal de Paris achevé à l'occasion du tournage de la derniere saison. Cette qui porte sur un migrant adolescent retrouvé mort dans une laverie du 18e arrondissement est donc logiquement tourné principalement dans ces arrondissement et plus largement dans ceux du nord est parisien. 




##QUESTION 11 Proposez une visualisation des lieux de tournages du film "120 battements par minute"

BPM120 <- filter(longparis, nom_tournag == "120 BATTEMENTS PAR MINUTE")

mapview(BPM120) # visualisation des lieux de tournages du film "120 battements par minute"
