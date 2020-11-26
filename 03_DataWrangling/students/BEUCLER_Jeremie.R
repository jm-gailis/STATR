#chargement des library - #packages

# Je vous ai fourni un script annoté : pourquoi ne pas l'utiliser ? 


library(mapview)
library(sf)
library(tidyverse)

# premier exo : calculer le taux  hab de meurtres aux USA / 100000 = OK

murders <- read.csv("Data/murders.csv")

# on groupe par région et on calcule le taux
murders %>% group_by(region) %>% 
  summarise(tot_by_region = sum(total),
            taux_cent_mille = tot_by_region / (population / 100000))

# deuxième exo : calculer la densité de popu par département et la
# visualiser


# tout d'abord : on joint les données INSEE et les données géo de l'IDF

idf_sf <- st_read(dsn = "statsmappingwithR/03_DataWrangling/data/parispc_com.shp",
                  crs = 2154,
                  stringsAsFactors = F)

data_insee <- read.csv("statsmappingwithR/03_DataWrangling/data/paris_soc_eco.csv")

idf_sf <- idf_sf %>% rename(CODGEO = INSEE_COM)
idf_sf$CODGEO <- as.character(idf_sf$CODGEO)

new_idf <- merge(idf_sf,data_insee,by="CODGEO")
# SOLUTION ----
# Vous pouvez aussi écraser le fichier idf_sf pour éviter un fichier supplémentaire
#  idf_sf <- merge(idf_sf,data_insee,by="CODGEO")


# puis on calcule la densite de popu par dpt

idf_dpt <- new_idf %>% select(CODGEO, NOM_DEPT, POP2008) %>%
  mutate(Superficie = st_area(.)) %>% 
  mutate(Superficie_km = as.numeric(Superficie/10^6)) %>% 
  group_by(NOM_DEPT) %>% 
  summarise(Superficie_dpt = sum(Superficie_km),
            Popu_dpt = sum(POP2008),
            Densite_dpt = Popu_dpt/Superficie_dpt)

# et on la visualise sur la carte = OK 

mapview(idf_dpt, zcol = "Densite_dpt")


#-----------------------#
#---- EXERCICE 3-------
#-----------------------#

# on ouvre le fichier shapefile et on le stocke dans un objet

movies_paris <- st_read(dsn = "statsmappingwithR/03_DataWrangling/data/lieux-de-tournage-a-paris/lieux-de-tournage-a-paris.shp",
                        crs = 4326,
                        stringsAsFactors = F)

# on utilise le bon système de projection = OK

movies_paris <- st_transform(movies_paris, 2154)

# Décrivez le tableau de données : combien d'indivius ? 

# SOLUTION = Où sont vos réponses ???? ---- 
# Quels types de variables ?

# SOLUTION = IDEM, sont vos réponses ???? ---- 

# on regarde ce qui se trouve dans l'objet movies_paris

glimpse(movies_paris)

# on compte le nombre de lieux de tournages associés à une série web

movies_paris %>% filter(type_tourna == "Série Web") %>% 
 select(adresse_lie) %>% count()

# SOLUTION PLUS SIMPLE ---- 

movies_paris %>% filter(type_tourna == "Série Web") %>% count()

# on compte le nombre de tournages par boite de production et on le met dans 
# un objet = OK

tournage_by_prod <- movies_paris %>% group_by(nom_product) %>% 
  summarise(nb = n())


# on divise l'objet :
# - tvshowsparis pour les séries TV ;
# - long_paris pour les longs métrages.

tvshowparis <- movies_paris %>%
  filter(type_tourna == "Série TV"| type_tourna == "Série Web")

longparis <- movies_paris %>%
  filter(type_tourna == "Long métrage" | type_tourna == "Téléfilm")

# Quel est l'arrondissement de Paris qui accueille le
#plus de tournages en 2016 ? En 2019 ?

#on filtre les années qui nous intéressent, on regroupe par année,
#puis on obtient les noms des ardt les plus fréquents

movies_paris %>% filter(annee_tourn == "2016"
                        | annee_tourn == "2019") %>% 
                        group_by(annee_tourn) %>%
          summarise(max_ardt_years = names(which.max(table(ardt_lieu))))

# SOLUTION----
# Cela fonctionne. On peut aussi faire ça pour avoir toutees les infos
# Exemple 
movies_paris %>% 
  filter(annee_tourn %in% c("2016", "2019")) %>% # sélectionner année en créant un vecteur c()
  group_by(ardt_lieu, annee_tourn) %>% #regrouper par quartiers
  summarise(Total = n()) %>%
  group_by(annee_tourn) %>%
  arrange(desc(Total), .by_group = TRUE) %>% # classer par ordre décroisant
  slice(1) # ligne avec la plus forte valeur de Total



#Quel est l'arrondissement de Paris qui accueille
#le plus de long métrage en 2018 ?
# on recommence la même opération sur le df longparis

longparis %>% filter(annee_tourn == "2018") %>% 
   summarise(names(which.max(table(ardt_lieu))))

## SOLUTION ----
longparis %>% filter(annee_tourn == "2018") %>%
  group_by(ardt_lieu) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))

# Dans quels arrondissements se déroulent la série "Emily in Paris" ?
# on créé un tableau avec la fréquence des tournages de la série
# par arrondissement : on voit que le 5ème, le 6ème et le 1er sont en tête
# ce qui est cohérent avec une vision fantasmée et américano-centrée
# de Paris

movies_paris %>% filter(nom_tournag == "Emily in Paris") %>%
  group_by(ardt_lieu) %>% 
  summarise(table(ardt_lieu))

# Ok, est ce pour autant une vision "américano-centrée" ? Est-elle propre aux "américains" ?  

#Proposez une visualisation des lieux de tournage de 8e saison
#de la série Engrenages. Quels types de localisations et quels 
#arrondissements sont le plus associés à cette série ? 

Lieux_engrenages_saison_8 <- tvshowparis %>% 
  filter(nom_tournag == "ENGRENAGES SAISON 8")

mapview(Lieux_engrenages_saison_8, zcol = "adresse_lie", legend = FALSE)

## SOLUTION ##--------------------------
# On peut aussi faire ces opérations sans créer d'objet intermédiaire 
movies_paris %>% filter(nom_tournag == "120 BATTEMENTS PAR MINUTE") %>% mapview(., zcol = "ardt_lieu")

# on voit bien que la saison 8 de la série Engrenages se déroule 
# presque exclusivement dans Paris intra-muros, et plus
# particulièrement rive droite, au nord de Paris (9ème, 10ème, 18ème, 19ème)
# dans les quartiers les plus populaires de Paris
# ce qui correspond bien au genre de la série
# Egalement des solutions périphériques. 

# COMMENTAIRE ----
# certes, mais comme il s'agit de données fournies par la municipalité de Paris, 
# nous n'avons pas les lieux de tournage dans les autres communes !

# Proposez une visualisation des lieux de tournages du film
#"120 battements par minute". 

Lieux_120_BPM <- longparis %>%
  filter(nom_tournag == "120 BATTEMENTS PAR MINUTE")
  
mapview(Lieux_120_BPM, zcol = "adresse_lie", legend = FALSE)


