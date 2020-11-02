#----------------------------#
#       DEXTREIT Natalia
#------------------------

# Ici, précisez les packages dont vous avez besoin

library(sf)
library(tidyverse)
library(mapview)
library(dplyr)

#----------------------------#
#          Exercice 1 -----
#----------------------------#
#Complétez les lignes suivantes 

#1. Ouvrez le fichier csv

murders <- read.csv("data/Exercice 1/murders.csv.xls") # COMMENTAIRE : un fichier ne peut pas être à la fois CSV et XLS !

#2. Calculer le nombre de meurtres par grande région

total_murd <- murders %>%
              group_by(region) %>%
              summarise(Pop=sum(population),Total_murders=sum(total))

#3. Calcul du ratio pour 100 000 habitants
total_murd <- total_murd %>%
              mutate(Pop_th=Pop/100000) %>%
              mutate(Total_murders_Ratio = Total_murders/Pop_th)
              
#----------------------------#
#          Exercice 2 -----
#----------------------------#
# Calculez et visualisez la densité de  population par département avec le fichier `parispc_com.shp`

#1. Ouvrez le shapefile parispc_com et 

idf_sf <- st_read(dsn="data/Exercice 2/idf/parispc_com.shp",crs = 2154, stringsAsFactors=F)


#2. Ouvrez le fichier csv paris_soc_eco

soc_eco <- read.csv("data/Exercice 2/paris_soc_eco.csv.xls")
  

#3. Préparez les fichiers pour une jointure avec la fonction mutate

  soc_eco <- soc_eco %>% 
            mutate(INSEE_COM = as.character(CODGEO))

#4. Réalisez une jointure

idf_sf <- left_join(idf_sf,soc_eco, by = "INSEE_COM")

#5. Calculez la densité par département

dpt_sf <- idf_sf %>%
          group_by(NOM_DEPT) %>% # regrouper les communes
          summarise(Pop = sum(POP2008)) %>% #calculer somme de population en 2008
          ungroup() %>% #pour dégrouper. Ne pas toucher !
          mutate(Sup = st_area(.)) %>% #calculer surface en M2
          mutate(Sup_km2= as.numeric(Sup/10^6)) %>% #surface en km2
          mutate(Densité = Pop/Sup_km2) #calculer densité de population

#6. Visualiser la densité par département

# Avec la fonction plot
plot(dpt_sf["Densité"])

# Avec la fonction mapview
mapview(dpt_sf, zcol="Densité")

#----------------------------#
#          Exercice 3 -----
#----------------------------#

#1. Ouvrez le shapefile `lieux-de-tournage-a-paris.shp` dans RStudio et stockez le dans un objet qui s'appelle _movies_paris_. NB Le système de projection est WGS 84, soit epsg = 4326. 

movies_paris <- st_read(dsn="data/Exercice 3/lieux-de-tournage-a-paris.shp",crs = 4326, stringsAsFactors=F)

#2. Reprojetez l'objet dans une projection adaptée, en utilisant la fonction [st_tranform](https://rcarto.github.io/carto_avec_r/chapitre1.html#les-syst%C3%A8mes-de-projections)

movies_paris <- st_transform(movies_paris, 2154)

#3. Décrivez le tableau de données : combien d'individus ? Quels types de variables ?

nrow(movies_paris) # 7742 individus dans le tableau de données

ncol(movies_paris) # 13 variables
head(movies_paris) # on constate que les 13 variables sont de type qualitatif nominal, la première faisant fonction d'identifiant

#4. Combien de lieux de tournage sont associés à une série Web ?

Serie_web_paris <- filter(movies_paris,type_tourna == "Série Web")
nrow(Serie_web_paris) # Ce sont donc 283 lieux de tournage associés à une série Web. 

# SOLUTION : une fonction table(), qui calcule fréquence de chaque modalité. Bien plus rapide !
table(movies_paris$type_tourna)

#5. Créez un data.frame qui renseigne sur le nombre de tournages réalisés par boite de production. 

freq_tournage_production <- as.data.frame(table(movies_paris$nom_product))
freq_tournage_production # affiche le data.frame réalisé

#6. Divisez l'objet _movies_paris_ en deux objets distincts : 

#    - _tvshowsparis_ pour les séries TV ; 
tvshowsparis <- filter(movies_paris,type_tourna == "Série TV")
#   - _long_paris_ pour les longs métrages.
long_paris <- filter(movies_paris,type_tourna == "Long métrage")


#7. Quel est l'arrondissement de Paris qui accueille le plus de tournages en 2016 ? En 2019 ? _NB. Il vous faut utiliser la fonction group_by en utilisant 2 variables pour grouper_...

# en 2016
movies_paris_2016 <- movies_paris %>%
                  group_by(annee_tourn,ardt_lieu) %>%
                  summarize(n()) %>%
                  filter(annee_tourn=="2016" ) 
                  
max(movies_paris_2016$`n()`) # indique que le plus de tournages tournés en 2016 dans un arrondissement est de 240, mais ne précise pas lequel
movies_paris_2016 [which.max(movies_paris_2016$`n()`),2] # précise que c'est le 16 ème arrondissement (75016)
 
# COMMENTAIRE : OK, mais quelques remarques. 
# 1. Il vaut mieux faire une opération de filter() avant de faire un group_by() : le calcul pour l'ordi est plus léger.
# 2. La fonction arrange permet de trier le tableau
# SOLUTION 
tournages_2016 <- movies_paris %>% 
  filter(annee_tourn == 2016) %>% # on filtre sur l'année 2016
  group_by(ardt_lieu) %>% # On regroupe par arrondissement
  summarise(total = n())  %>% # Calcul de la fréquence
  arrange(desc(total)) #classer le dataframe par ordre décroissant
tournages_2016 # on affiche l'objet pour avoir toutes les infos dans la console. 

# en 2019
movies_paris_2019 <- movies_paris %>%
  group_by(annee_tourn,ardt_lieu) %>%
  summarize(n()) %>%
  filter(annee_tourn=="2019" ) 

max(movies_paris_2019$`n()`) # indique que le plus de tournages tournés en 2016 dans un arrondissement est de 144, mais ne précise pas lequel
movies_paris_2019 [which.max(movies_paris_2019$`n()`),2] # précise que c'est le 18 ème arrondissement (75018)


#8. Quel est l'arrondissement de Paris qui accueille le plus de long métrage en 2018 ? 

long_paris_2018 <- long_paris %>% # nous nous plaçons dans l'objet indiquant seulement les longs métrages
                   group_by(annee_tourn, ardt_lieu) %>%
                   summarize(n()) %>%
                   filter(annee_tourn=="2018" ) 

summary(long_paris_2018) # fonction qui résume la distribution statistique et indique donc la valeur maximale (108.0)
long_paris_2018 [which.max(long_paris_2018$`n()`),2] # indique que cela correspond au 19ème arrondissement (75019)
  
## UN peu confus ici. La fonction summary() donne la distribution d'une variable statistique, et non d'un objet. 
# SOLUTION 

long_paris_2018 <- movies_paris %>%
  filter(annee_tourn == 2018 & type_tourna == "Long métrage") %>% #on ne conserve que les long métrages tournés en 2018
  group_by(ardt_lieu) %>% #regrouper les arrondissements
  summarise(total = n()) %>% #calcul de la fréquence
  arrange(desc(total)) #ranger le tableau par ordre décroissant

#9. Dans quels arrondissements se déroulent la série "Emily in Paris" ?

# COMMENTAIRE : idem pour la fonction filter(), à faire au début. Le calcul est bcp, bcp plus rapide. 

movies_paris_Emily <- movies_paris %>%
                      group_by(nom_tournag, ardt_lieu) %>%
                      summarize(n()) %>%
                      filter(nom_tournag=="Emily in Paris") # on obtient comme résultat que cette série a été tournée dans la plupart des arrondissements de Paris (du 1er au 12ème arrondissement inclus, ainsi que le 16ème et 18ème arrondissement)
                      # avec un nombre important de tournages notamment dans le 5ème, 6 ème et 1er arrondissement (cf colonne n())

#10. Proposez une visualisation des lieux de tournage de 8e saison de la série Engrenages. Quels types de localisations et quels arrondissements sont le plus associés à cette série ? 

# COMMENTAIRE : idem pour la fonction filter, à faire au début. 

movies_paris_engrenages_8 <- movies_paris %>%
                             group_by(nom_tournag, ardt_lieu, adresse_lie) %>%
                             summarize(n()) %>%
                             filter(nom_tournag=="ENGRENAGES SAISON 8")
View(movies_paris_engrenages_8) # affiche le tableau obtenu
mapview(movies_paris_engrenages_8) # visualisation 
# Cette série est surtout associée à des localisations sur la rive droite (à l'exception d'un seul lieu de tournage), 
# principalement dans les arrondissements 8 à 20 (à l'exception du 14ème et 15ème). 

#11. Proposez une visualisation des lieux de tournages du film "120 battements par minute". 

movies_paris_120b <- movies_paris %>%
                     group_by(nom_tournag, ardt_lieu, adresse_lie) %>%
                     summarize(n()) %>%
                    filter(nom_tournag=="120 BATTEMENTS PAR MINUTE")
mapview(movies_paris_120b) # Les lieux de tournage sont assez dispersés dans Paris avec néanmoins une concentration apparente d'un certain nombre de lieux de tournage près de la Seine sur la rive droite (4ème et 12ème arrondissement).


# COMMENTAIRE ----

# Très bon travail, vous pouvez alléger encore l'écriture du code, en éliminant les étapes intermédiaires : 
#la fonction filter peut combiner plusieurs critères. On peut donc partir du même objet "movies_paris" pour réaliser toutes
# les opérations !
