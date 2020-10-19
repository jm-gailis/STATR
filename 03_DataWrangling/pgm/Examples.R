
##-------------------------##
#         Packages
##-------------------------##


library(tidyverse)
library(sf)


# Open data
idf_sf <- st_read("data/parispc_com.shp")


#Count frequencies for Department
table(idf_sf$NOM_DEPT)








##-------------------------##
#         DATA PREP
##-------------------------##








movies_paris <- st_read(dsn = "data/lieux-de-tournage-a-paris/lieux-de-tournage-a-paris.shp", 
                        crs = 4326, 
                        stringsAsFactors = F)

head(movies_paris)

a <- as.data.frame(table(movies_paris$nom_tournag))

movies_paris %>% filter(nom_tournag == "ENGRENAGES SAISON 8") %>% mapview()

test <-  movies_paris %>% filter(type_tourna == "Long métrage")%>% filter(annee_tourn == "2016")
mapview(test)
head(test)

table(movies_paris$type_tourna)

test2 <-  movies_paris %>% filter(nom_tournag == "Emily in Paris") 
mapview(test2)
table(test2$ardt_lieu)
table(movies_paris$nom_tournag)

