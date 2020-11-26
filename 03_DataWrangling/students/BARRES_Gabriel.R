
#DM Barrès

#Exercices d'"échauffement"

# Je vous ai fourni un script annoté : pourquoi ne pas l'utiliser ? 

##Chargement des plugins de base
#   NB. Il s'agit de packages
##(tous ne sont pas utiles pour l'exercice)
library(sf)
library(tidyverse)
library(mapview) 
library(dplyr) # inutile : dplyr est compris dans le Tidyverse

## Calculs autour des meurtres
###Chargement des données

murders_exo <- read.csv("Data/murders.csv")
###Calcul du total de meurtres par région


murders_exo_region <- select(murders_exo, region, total) %>% ###
  ####Bizarrement R refusait de sauvegarder murders_exo_regions
  ####quand tout ce bout de code était lancé d'un coup et que la
  ####1ère ligne n'avait pas les parenthèses supplémentaires
  group_by(region) %>%
  summarise(total_regional = sum(total))

# COMMENTAIRE 1
# Oui, car la syntaxe est défaillante. Vous devez d'abord appeler votre objet avant d'enchaîner les pipes.
# Ce qui idonne 
murders_exo_region <- murders_exo %>% # on sélectionne l'objet
  select(region, total) %>%
  group_by(region) %>%
  summarise(total_regional = sum(total))

# COMMENTAIRE 2 : vous n'avez pas calculé le ratio pour 100 000 habitants !


##Calculs autour des départements

data_idf <-st_read("Data/parispc_com.shp") ###Chargement des données


data_idf <- data_idf %>% st_set_geometry(NULL) ###Pourquoi s'encombrer?

# COMMENTAIRE : pour répondre à la consigne. Je vous demande de visualiser la densité de population par département/
# Vous ne faites ici que la calculez. C'est incomplet?

paris_exo <- select(data_idf, CODE_DEPT, SUPERFICIE, POPULATION) %>% ###Extraction de celles qui nous intéressent
  #Attention à la syntace : paris_exo <- data_idf %>% select(etc)
  group_by(CODE_DEPT) %>% ###Regroupement des données par communes selon le département
  summarise(POPULATION = sum(POPULATION), SUPERFICIE = sum(SUPERFICIE), .groups = "keep") %>% ###Addition des données par commune pour obtenir des données par département
  mutate(DENSITE = (POPULATION*1000)/(SUPERFICIE/100)) ###NB: dans la table originale, la population est en milliers d'habitants et la superficie en hectares



movies_paris <- st_read("data/lieux-de-tournage-a-paris.shp")


##2.Reprojection en Lambert-93 = OK
movies_paris <- st_transform(movies_paris, 2154)

##3.Le tableau de données présente 13 variables pour 7742 individus; sur les 13, 
## on a surtout des variables qualitatives, principalement nominales, avec qques
##variables qualitatives ordonnées (dates), plus les variables de géométrie
plot(movies_paris)

# SOLUTION : la formule plot est trop lourdes, et ne renseigne pas sur la classe des variales.
# Mieux vaut utilier la fonction glimpse
glimpse(movies_paris)

##4.Exploration des données = OK
table(movies_paris$type_tourna) ## 283 tournages sont associés à des séries web

##5.Création d'un df résumant le nombre de tournages par producteur = OK
producteurs <- data.frame(table(movies_paris$nom_product))

##6. Division du df original en deux objets distincts 
tvshowsparis <- data.frame(filter(movies_paris, type_tourna == 'Série TV' | type_tourna == "Série Web" ))
long_paris <- data.frame(filter(movies_paris, type_tourna == 'Long métrage' | type_tourna == "Téléfilm" ))

# COMMENTAIRE : il fallait diviser l'objet "movies_paris", qui n'est pas un dataframe. 
# SOLUTION 
tvshowsparis <- movies_paris %>% filter(movies_paris, type_tourna == 'Série TV' | type_tourna == "Série Web" )

##7.1. Arrondissement accueillant le plus de tournages en 2016:
# COMMENTAIRE : à nouveau, petite erreur de syntaxe, toujours la même. 

movies_paris %>% group_by(ardt_lieu, annee_tourn) %>%
  filter(annee_tourn == 2016) %>%
  summarise(effectifs = n()) -> tournages_2016  # SYNTAXE : l 'objet à créer se met au début de la ligne, pas à la ifn. 
which.max(tournages_2016$effectifs) ##C'est le 16e arrondissement

# SOLUTION 
tournages_2016 <- movies_paris %>% 
  filter(annee_tourn == 2016) %>%
  group_by(ardt_lieu) %>%
  summarise(effectifs = n())  %>%
  arrange(desc(effectifs)) #classer par ordre décroissant
tournages_2016
##C'est le 16e arrondissement avec 240 tournages

##7.2. Arrondissement accueillant le plus de tournages en 2019:
group_by(movies_paris, ardt_lieu, annee_tourn) %>%
  filter(annee_tourn == 2019) %>%
  summarise(effectifs = n()) -> tournages_2019  
which.max(tournages_2019$effectifs) ##C'est le 18e arrondissement

# SOLUTION : voir plus haut

##8.Arrondissement accueillant le plus de longs métrages en 2018:
group_by(long_paris, ardt_lieu) %>%
  filter(annee_tourn == 2018) %>%
  summarise(effectifs = n()) -> tourn_long_2018 # SYNTAXE : même commentaire
which.max(tourn_long_2018$effectifs) ##Le 19e arrondissement


# SOLUTION : 

movies_paris %>% 
  filter(type_tourna == 'Long métrage') %>% # ON a joute une étape filter ici
  group_by(ardt_lieu, annee_tourn) %>%
  filter(annee_tourn == 2018) %>%
  summarise(effectifs = n())  %>%
  arrange(desc(effectifs)) #classer par ordre décroissant


##9.Arrondissements dans lesquels se déroulent la série Emily in Paris

filter(tvshowsparis, nom_tournag == 'Emily in Paris') %>%
  group_by(ardt_lieu) %>%
  summarise(nb_tournages = n()) -> 'EmilyTournages' # IDEM, problème de syntaxe

#COMMENTAIRE 1 : problème de syntaxe
# COMMENTAIRE 2 : pour créer un objet, on utilie pas de ''

#SOLUTION 
EmilyTournages <- movies_paris%>%
  filter(nom_tournag == 'Emily in Paris') %>%
  group_by(ardt_lieu) %>%
  summarise(nb_tournages = n())

###Le Df EmilyTournages contient toute l'information. 
# COmmentaire : et donc ? Commentez brièvement les résultats. 

##10.Visualisation basique des lieux de tournage de la S8 d'"Engrenages"
Engrenages <- filter(movies_paris, nom_tournag == 'ENGRENAGES SAISON 8')
mapview(Engrenages$geometry) 

# COMMENTAIRE 
# 1. Syntaxe, de nouvea
# 2. La fonction mapview ne nécessite pas de désigner la colonne de géométrie.
mapview(Engrenages)
table(Engrenages$ardt_lieu)


###Les arrondissements les plus concernés sont les arrondissements périphériques
###de la rive droite, avec une surreprésentation des quartiers les plus populaires
###de la capitale mais aussi des quartiers bourgeois
#Réponse vague : que sont les quartiers bourgeois ? 
#Par ailleurs, en appliquant la fonction table, on constate que ce sont les arrondissements du 20e, du 19e et du 18e
# Sont les plus représentés, et qy(il n'y a qu'un seul tournage rive gauche)
# Conclusion : il n'y a donc pas de sureprésentation des quartiers bourgeois. Attention à vos argumennts. 
sort(table(Engrenages$ardt_lieu))

##11.Visualisation des lieux de tournages du film "120 battements par minute"
BpM <- filter(movies_paris, nom_tournag == '120 BATTEMENTS PAR MINUTE')
mapview(BpM$geometry) #SYNTAXE : même remarque, pas utile. 
