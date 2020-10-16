library(tidyverse)
library(mapview)
library(sf)























movies_paris <- st_read(dsn = "statsmappingwithR/02_SpatialData/data/lieux-de-tournage-a-paris/lieux-de-tournage-a-paris.shp", 
                        crs = 4326, 
                        stringsAsFactors = F)

head(movies_paris)

mapview(movies_paris)

movies_paris %>% filter(nom_tournag == "ENGRENAGES SAISON 8") %>% mapview()

test <-  movies_paris %>% filter(type_tourna == "Long métrage")%>% filter(annee_tourn == "2016")
mapview(test)
head(test)

table(movies_paris$type_tourna)

test2 <-  movies_paris %>% filter(nom_tournag == "120 BATTEMENTS PAR MINUTE") 
mapview(test2)
table(test2$ardt_lieu)
table(movies_paris$nom_tournag)
