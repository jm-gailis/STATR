

#----------------------------#
#       BAGES Thibaud
#------------------------

# Ici, précisez les packages dont vous avez besoin
library(sf)
library(dplyr)
library(mapview)


#--------------------------#
#     RAPPEL
#------------------------

# Avec le signe #, je peux "désactiver" une ligne de code. Ce que j'écris n'est donc pas exécutable.
# Je peux donc m'en servir pour prendre des notes, ou structurer mon script. 
# A l'inverse, quant je n'utilise pas de signe #, le code est donc exécutable : 

1+1
mean(c(1, 8, 9)) #moyenne d'un vecteur de 3 valeurs : 1, 8, 9

#----------------------------#
#          Exercice 1 -----
#----------------------------#
#Complétez les lignes suivantes 

#1. Ouvrez le fichier csv
murders <- read.csv("Data/DM/murders.csv")

#2. Calculer le nombre de meurtres par grande région
total_murd <- murders %>%
              group_by(region) %>%
              summarise(Total = sum(total),
                        Population = sum(population))

#3. ratio
total_murd <- total_murd %>% 
              mutate(ratio = Total/100000)


#----------------------------#
#          Exercice 2 -----
#----------------------------#
# Calculez et visualisez la densité de  population par département avec le fichier `parispc_com.shp`

#1. Ouvrez le shapefile parispc_com et 

idf_sf <- st_read("Data/DM/parispc_com.shp",
                  crs = 2154,
                  stringsAsFactors = F)

#2. Ouvrez le fichier csv paris_soc_eco
  
soc_eco <- read.csv("Data/DM/paris_soc_eco.csv")
  
#3. Préparez les fichiers pour une jointure avec la fonction mutate

soc_eco <- soc_eco %>%
  mutate(INSEE_COM = as.character(CODGEO))

#4. Réalisez une jointure

idf_sf <- left_join(idf_sf, soc_eco, by = "INSEE_COM")

#5. Calculez la densité par département
dpt_sf <- idf_sf %>%
          group_by(NOM_DEPT) %>% # regrouper les communes
          summarise(POPD2008 = sum(POP2008)) %>% #calculer somme de population en 2008
          ungroup() %>% #pour dégrouper. Ne pas toucher !
          mutate(Superficie = st_area(.)) %>% #calculer surface en M2
          mutate(Superficie_km = as.numeric(Superficie/10^6)) %>% #surface en km2
          mutate(Density = POPD2008/Superficie_km) #calculer densité de population

#6. Visualiser la densité par département

# Avec la fonction plot

plot(dpt_sf["Density"])

# Avec la fonction mapview

mapview(dpt_sf, zcol = "Density")

#----------------------------#
#          Exercice 3 -----
#----------------------------#

#1 Ouvrir le shapefile

tournages <- st_read("Data/DM/lieux-de-tournage-a-paris/lieux-de-tournage-a-paris.shp",
                     crs = 4326,
                     stringsAsFactors = F)

#2 Changer la projection

movies_paris <- st_transform(tournages, 2154)
plot(st_geometry(movies_paris)) #visualiser la nouvelle projection

# COMMENTAIRE : pourquoi créer deux objets différents ? 
# SOLUTION / 
movies_paris <- st_read("Data/DM/lieux-de-tournage-a-paris/lieux-de-tournage-a-paris.shp",
                     crs = 4326,
                     stringsAsFactors = F)
movies_paris <- movies_paris(movies_paris, 2154)


#3 Décrire le tableau de données 

nrow(movies_paris)#Le tableau de données regroupe des tournages, chacun correspondant donc à un individu, il y en 7742
class(movies_paris)#Il s'agit d'un data.frame ET D'UN OBJET SF

# COmmentaire : décrire un tableau de données consiste à catégoriser brièvement chaque variable. 

#4 Compter le nombre de lieux de tournage associés à une série Web

web <- filter(movies_paris, type_tourna == "Série Web") #créer l'objet web
nrow(web) #il y a 283 lieux de tournages associés à des séries Web

# SOLUTION : une fonction table, qui calcule fréquence de chaque modalité. Bien plus rapide !
table(movies_paris$type_tourna)

#5 créer un data.frame

df <- data.frame(movies_paris$nom_product,
             stringsAsFactors = F) #creation du data.fram

df <- df %>%
  group_by(movies_paris.nom_product) %>% #regrouper les lieux producteurs
  summarise(n()) #compter les lieux de tournages

# COMMENTAIRE : c'est compliqué ! 
# SOLUTION  : un table suffit !
df <- data.frame(table(movies_paris$nom_product))


#6 Diviser l'objet movies_paris = OK

tvshowsparis <- filter(movies_paris, type_tourna == "Série TV") #création de l'objet des séries TV
long_paris <- filter(movies_paris, type_tourna == "Long métrage") #création de l'objet des longs métrages


#----------------------------#
#          Exercice 4 -----
#----------------------------#



#1 Chercher l'arrondissement accueillant le plus de tournages

tournages_2016 <- movies_paris %>%
  filter(annee_tourn == 2016) %>% #Filtrer l'années 2016
  group_by(ardt_lieu) %>% #regrouper par arrondissement
  summarise(n()) #compter le nombre de tournage par ardt

max(tournages_2016$`n()`) #le 16eme arrondissement est donc celui qui accueille le plus de tournages en 2016

# COMMENTAIRE : la dernière ligne de code est un peu obscure. 
# SOLUTION 
tournages_2016 <- movies_paris %>% 
  filter(annee_tourn == 2016) %>%
  group_by(ardt_lieu) %>%
  summarise(total = n())  %>%
  arrange(desc(total)) #classer par ordre décroissant
tournages_2016


tournages_2019 <- movies_paris %>%
  filter(annee_tourn == 2019) %>%
  group_by(ardt_lieu) %>%
  summarise(n())

max(tournages_2019$`n()`) #le 18eme arrondissement est donc celui qui accueille le plus de tournages en 2019

#2 Chercher l'arrondissement accueillant le plus de long métrage en 2018

long_metrage_2018 <- movies_paris %>%
  filter(annee_tourn == 2018 & type_tourna == "Long métrage") %>% #on ne conserve que les long métrages tournés en 2018
  group_by(ardt_lieu) %>% #regrouper les arrondissements
  summarise(n())

max(long_metrage_2018$`n()`) #le 18eme arrondissement est donc celui qui accueille le plus de tournages en 2019

# COMMENTAIRE : voir rematque précédente

#3 Arrondissements de Emily in Paris

emily <- movies_paris %>%
  filter(nom_tournag == "Emily in Paris") %>%
  group_by(ardt_lieu) %>%
  summarise(nombre = n()) #on trouve la liste des arrondissements dans lesquels la série a été tournée

# La série "Emily in Paris" été tournée dans les 1er, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 16, et 18èmes arrondissements de Paris

# COMMENTAIRE : certes, mais quels sont ceux qui sont les plus représentés ? 

#4 Représenter les lieux de tournage de Engrenages 8

engrenages <- movies_paris %>%
  filter(nom_tournag == "ENGRENAGES SAISON 8")
  
mapview(engrenages) #visualiser les lieux des tournage

tournages_engrenages <- engrenages %>%
  group_by(ardt_lieu) %>%
  summarise(nombre = n()) #compter le nombre de tournages par arrondissements

# COMMENTAIRE :  Quels types de localisations et quels arrondissements sont le plus associés à cette série ? 
# Vous ne répondez pas à la question. 

#5 Représenter les lieux de tournage de 120 battements par minute 

battements <- movies_paris %>%
  filter(nom_tournag == "120 BATTEMENTS PAR MINUTE") #filtrer les tournages du film

mapview(battements) #visualiser les lieux de tournage du film 


