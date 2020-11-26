

#lancer les packages utiles
library(tidyverse)
library(sf)
library(mapview)

#ECHAUFFEMENT

#QUESTION NUMERO UNE

#ouvrir le fichier
murders <- read.csv("data/murders.csv", stringsAsFactors = F)

#calcul de la somme totale des meurtres
murders_T <- sum(murders$total)

#calcul d'un taux pour 100.000 habitant.es
murders$Population_th <- murders$population/100000
murders$Ratio <- murders$total/murders$Population_th

#calcul de la somme totale des meurtres par grande region
region_sf <- murders %>%
  group_by(region) %>% #regroupe lignes par region
  summarise(total_region = sum(total), # total de meurtres par region
            population_region = sum(population), # population totale par region
            state = n()) %>% # compte le nombre d'Etats
  mutate(population_th = population_region/100.000) %>% #creation variable population en centaine de milliers d'habitant.es
  # Commentaire : Attention, 100000 n'est pas 100.000. 
  # 100 000 : Cent mille
  # 100.000 = 100
  mutate(taux = total_region/population_th) #calcul ratio meurtres pour 100.000 hbts 
  

#QUESTION NUMERO DEUX
#ouvrir données
idf_sf <- st_read(dsn= "Data/parispc_com.shp", 
                  crs = 2154,
                  stringsAsFactors = F)
soc_eco <- read.csv(file="Data/paris_soc_eco.csv", header=TRUE, sep=",")

#travail de jointure
#creation d'un identifiant commun
soc_eco$INSEE_COM <- as.character(soc_eco$CODGEO)

#jointure avec la fonction leftjoin
idf_sf <- left_join(idf_sf, #objet 1 : l'objet sf
                    soc_eco, #objet 2 : le data.frame
                    by = "INSEE_COM") # l'identifiant commun


#calcul de la densité de population par departement
paris_sf <- idf_sf %>% # choix objet de départ du pipeline
  group_by(CODE_DEPT) %>% # regrouper lignes par departement
  summarise(population_dept = sum(POPULATION), # population totale par dept
            superficie_dept = sum(SUPERFICIE), # superficie du departement en m2
            NOM_COMM = n()) %>% # compte le nombre de communes dans chaque département
  mutate(superficie_dept_km = as.numeric(superficie_dept/10^6)) %>% # En km2
  mutate(Density = population_dept/superficie_dept_km) # calculer densité

#commentaire:
#la densité est dix fois trop élevée, mais je ne trouve pas l'erreur que j'ai commise.
# COMMENTAIRE : la superfice est donnée en hectare dans le fichier !Pensez à regarder les méta données, ou
# à calculer la superficie avec un st_area()

#visualisation de la densité de population par dep
mapview(paris_sf, zcol = "Density")




#Devoir maison sur les lieux de tournage

#1 et 2/  ouvrir le shapefile et transformer son système de projection
movies_paris <- st_read(dsn= "Data/lieux de tournage/lieux-de-tournage-a-paris.shp", 
                        crs = 4326,
                        stringsAsFactors = F) %>%
  st_transform(2154)

#3/A  Trouver le nombre d'individus : trouver le nombre de lignes
nb_ind <- nrow(movies_paris) # Pas la peine de créer un objet, le résultat s'affiche dans la console. 

#3/B  Décrire les variables (nombre et classe des variables) 
type_var <- class(col(movies_paris))
nb_var <- ncol(movies_paris)

# COMMENTAIRE : pensez à utilisez la fonction glimpse, qui fait du tout en un. 
glimpse(movies_paris)

#4/ Nombre de tournages associés à des séries TV
nb_series_paris <- movies_paris %>% #choix objet du pipeline
  filter(type_tourna == "Série TV") %>% #filtrer uniquement les tournages associés à des séries TV
  nrow(.)

# SOLUTION : une fonction table(), qui calcule fréquence de chaque modalité. Bien plus rapide !
table(movies_paris$type_tourna)

#5/  Création d'un data.frame qui renseigne sur le nombre de tournages réalisés par boite de production
BP_df <- movies_paris %>%
  group_by(nom_product) %>%
  summarize(nb_tournage_Paris = n())

# SOLUTION : 
# 1. Soit la fonction table
BP_df <- data.frame(table(movies_paris$nom_product))
# 2. Soit avec dplyr
BP_df <- movies_paris %>%
  st_set_geometry(NULL) %>% # enlève les données géométrique pour alléger le calcul de l'ordi
  group_by(nom_product) %>% 
  summarise(n= n())

#6/  Divisez l'objet movies_paris en deux objets distincts = OK

#A/  tvshowsparis pour les séries TV
tvshowsparis <- movies_paris %>% #choix objet du pipeline
  filter(type_tourna == "Série TV") 

#B/ long_paris pour les longs métrages
long_paris <- movies_paris %>% #choix objet du pipeline
  filter(type_tourna == "Long métrage") 



#DEVOIR MAISON SUITE
#1/ Quel est l'arrondissement de Paris qui accueille le plus de tournages en 2016 ? 
movies_arrond_2016 <- movies_paris %>%
  group_by(annee_tourn,ardt_lieu) %>%
  summarize(n()) %>%
  filter(annee_tourn=="2016" ) 

max(movies_arrond_2016$`n()`)
summary(movies_arrond_2016)
movies_arrond_2016 [which.max(movies_arrond_2016$`n()`),2]

# COMMENTAIRE ; 
# 1. OK, mais le code est lourd
# 2. Il y a beaucoup d'objets intermédiaires créés. 
# SOLUTION pour 2016
tournages_2016 <- tournage_proj %>% 
  filter(annee_tourn == 2016) %>% # on filtre sur l'année 2016
  group_by(ardt_lieu) %>% # On regroupe par arrondissement
  summarise(total = n())  %>% # Calcul de la fréquence
  arrange(desc(total)) #classer le dataframe par ordre décroissant
tournages_2016 # on affiche l'objet pour avoir toutes les infos dans la console. 


#En 2019 ?  
movies_arrond_2019 <- movies_paris %>%
  group_by(annee_tourn,ardt_lieu) %>%
  summarize(n()) %>%
  filter(annee_tourn=="2019" ) 

max(movies_arrond_2019$`n()`)
summary(movies_arrond_2019)
movies_arrond_2019 [which.max(movies_arrond_2019$`n()`),2]


#2/  Quel est l'arrondissement de Paris qui accueille le plus de long métrage en 2018 ?
movies_ALM_18 <- long_paris %>%
  group_by(annee_tourn,ardt_lieu) %>%
  summarize(n()) %>%
  filter(annee_tourn=="2018" ) # Mettez les filtres au début pour alléger le calcul

max(movies_ALM_18$`n()`)
summary(movies_ALM_18)
movies_ALM_18 [which.max(movies_ALM_18$`n()`),2]      

# COMMENTAIRE ; 
# 1. OK, mais le code est lourd
# 2. Il y a beaucoup d'objets intermédiaires créés. 
# SOLUTION
movies_paris %>% 
  filter(annee_tourn == 2018 & type_tourna == "Long métrage") %>% # on filtre sur l'année 2018 et les tournagges
  group_by(ardt_lieu) %>% # On regroupe par arrondissement
  summarise(total = n())  %>% # Calcul de la fréquence
  arrange(desc(total)) #classer le dataframe par ordre décroissant
 # on affiche l'objet pour avoir toutes les infos dans la console. 

#3/  Dans quels arrondissements se déroulent la série "Emily in Paris" ? = OK
Emily_arrond <- tvshowsparis %>%
  filter(nom_tournag=="Emily in Paris") %>%
  group_by(annee_tourn,ardt_lieu) %>%
  summarize(n())

max(Emily_arrond$`n()`)
summary(Emily_arrond)
Emily_arrond [which.max(Emily_arrond$`n()`),2]


#4/ Proposez une visualisation des lieux de tournage de 8e saison de la série Engrenages. 

Engrenages_arrond <- tvshowsparis %>%
  filter(nom_tournag=="ENGRENAGES SAISON 8") %>%
  group_by(annee_tourn,ardt_lieu) %>%
  summarize(n())

max(Engrenages_arrond$`n()`)
summary(Engrenages_arrond)
Engrenages_arrond [which.max(Engrenages_arrond$`n()`),2]
mapview(Engrenages_arrond)

#/  Quels types de localisations et quels arrondissements sont le plus associés à cette série ?
#Les localisations des tournages de la série "Engrenages" sont majoritairement périphériques. 
#Ces tournages se déroulent la plupart du temps sur la rive droite et dans les arrondissements périphériques de la capitale
#(le 20e, le 18e, puis, avec un même nombre de tournages, le 9e, le 17e et le 19e).
#Paris-Centre ne constitue pas un lieu de tournage contrairement aux Portes de Paris.


#5 /  Proposez une visualisation des lieux de tournages du film "120 battements par minute".
CVBPM_arrond <- long_paris %>%
  filter(nom_tournag == "120 BATTEMENTS PAR MINUTE") %>%
  group_by(annee_tourn,ardt_lieu) %>% # Pas nécessaire ici
  summarize(n())

# max(CVBPM_arrond$`n()`) # IDEM, pas nécessaire
# summary(CVBPM_arrond)
# CVBPM_arrond [which.max(CVBPM_arrond$`n()`),2]
mapview(CVBPM_arrond)


# COMMENTAIRE : 
# C'est bien ! Le code est propre, et la logique du pipe est comprise. Vous semblez manipuler les fichiers Sf avec aisance,
# et vous repérez dans les enchainements.


