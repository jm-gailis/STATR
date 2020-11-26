#------------#
# GLASER Raphaël
#------------
# Je vous ai fourni un script annoté : pourquoi ne pas l'utiliser ? 



#### Exercice 1 ####

# Importation des donnees
tab_meurtres <- read.csv(file = "statsmappingwithR/01_Introduction/data/murders.csv",
                         header = TRUE,
                         sep = ",")

# Explorer le tableau
names(tab_meurtres)
head(tab_meurtres)
str(tab_meurtres)

# On cree un nouveau tableau avec les operations demandees grace au pipe
meurtres_par_reg <- tab_meurtres %>%
  group_by(region) %>% # on regroupe les lignes individus selon les modalites de la var region
  summarise(total_meurtres = sum(total), # on somme le total des meurtres par region
            ratio_meurtres = total_meurtres / (sum(population) / 100000)) # meurtres pour 100000 hab par region

# On explore ce nouveau tableau pour voir le resultat
names(meurtres_par_reg)
head(meurtres_par_reg)
str(meurtres_par_reg)


#### Exercice 2 ####

# Chargement des packages
library(mapview)

# Importation des donnees
paris_sf <- st_read (dsn = "statsmappingwithR/03_DataWrangling/data/parispc_com.shp",
                     crs = 2154,
                     stringsAsFactors = F)

# Exploration des donnees
names(paris_sf)
head(paris_sf)
str(paris_sf)

## On constate que la population est indiqué en milliers...
## ... et la superficie en hectares.

dptmt_sf <- paris_sf %>%
  mutate(SUPERFICIE = SUPERFICIE / 100) %>% # l unite sont desormais les km2
  mutate(POPULATION = POPULATION * 1000) %>% # unite = hab et non milliers d hab
  group_by(NOM_DEPT) %>%
  summarise(DENSITE = sum(POPULATION) / sum(SUPERFICIE))

mapview(dptmt_sf, zcol = "DENSITE")

# Chargement des packages
library(sf)
library(tidyverse)
library(mapview)

# Importation des donnees
movies_paris <- st_read(dsn = "statsmappingwithR/03_DataWrangling/data/lieux-de-tournage-a-paris/lieux-de-tournage-a-paris.shp",
                  crs = 4326,
                  stringsAsFactors = F)

# Reprojeter dans le systeme Lambert
movies_paris <- st_transform(movies_paris, 2154)

# Decrire le tableau
glimpse(movies_paris)
## Nombre d individus : 7742
## Types de variables : character, date, double class (decimaux), POINT

# Nombres de lieux associes au tournage d une serie Web
names(movies_paris) # pour rappel du nom des colonnes 
series_web <- movies_paris %>%
  select(id_lieu, type_tourna) %>% # les deux colonnes qui nous interessent
  filter(type_tourna == "Série Web") # ne garder que les series Web
glimpse(series_web) # voir combien de lieux il reste dans le .shp

# Nombre de tournages par boite (dans un data.frame)
boites_df <- as.data.frame(table(movies_paris$nom_product))

# Diviser en series et longs-metrages
tvshowparis <- filter(movies_paris, type_tourna == "Série TV")
long_paris <- filter(movies_paris, type_tourna == "Long métrage")

# Arrondissement qui accueille le plus de tournages
movies_2016 <- filter(movies_paris, annee_tourn == "2016")
sort(table(movies_2016$ardt_lieu))
movies_2019 <- filter(movies_paris, annee_tourn == "2019")
table(movies_2019$ardt_lieu)

## SOLUTION ##--------------------------
# On peut aussi faire ces calculs sans créer d'objet intermédiaire 
# Exemple 
movies_paris %>% 
  filter(annee_tourn == "2016") %>% # sélectionner année
  group_by(ardt_lieu) %>% #regrouper par quartiers
  summarise(Total = n()) %>% #calculer total
  arrange(desc(Total)) #ranger par ordre décroissant
#---------------------------------------------------------#

# Arrondissement qui accueille le plus de LM en 2018
long_2018 <- filter(long_paris, annee_tourn == "2018")
table(long_2018$ardt_lieu)

## SOLUTION ##--------------------------
# utiliser l'argument sort avec table
long_2018 <- filter(long_paris, annee_tourn == "2018")
sort(table(long_2018$ardt_lieu))

# Arrondissements de Emily in Paris
emily_paris <- filter(movies_paris, nom_tournag == "Emily in Paris") %>% st_set_geometry(NULL)
table(emily_paris$ardt_lieu)

# Visualisation des lieux de tournage de ENGRENAGES S7 (par arrondissement)
engrenagesS8_sf <- filter(movies_paris, nom_tournag == "ENGRENAGES S7")
mapview(engrenagesS8_sf, zcol = "ardt_lieu")

# Visualisation des lieux de tournage de 120 BPM
bpm_sf <- filter(movies_paris, nom_tournag == "120 BATTEMENTS PAR MINUTE")
mapview(bpm_sf, zcol = "ardt_lieu")

## SOLUTION ##--------------------------
# On peut aussi faire ces opérations sans créer d'objet intermédiaire 
movies_paris %>% filter(nom_tournag == "120 BATTEMENTS PAR MINUTE") %>% mapview(., zcol = "ardt_lieu")

