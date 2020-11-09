library(tidyverse)
library(mapview)
library(sf)
library(socviz)
library(urbnmapr) # For map
library(cartography)
library(viridis)
library(tmap)

# library(raster)

setwd("c:/Users/Julien M/Dropbox/ENS/Cours/FormationR/")


###################################-
 #           US ELECTIONS ----
###################################-

#Open data from 2020 elections ----
results_df <- read.csv("Datasets/US_Elections_2020/results_x2020_11_08_11_25_25.csv", 
                       colClasses=c("fips5"="character", "fips_char" = "character"))
state_fips <- read.csv("Datasets/US_Elections_2020/fips.csv")

glimpse(results_df)

# spatial data ----
 # county_geo <- st_read(dsn = "Datasets/US_Elections_2016/counties_Elections.shp")
# glimpse(county_geo)
states_sf <- get_urbn_map(map = "states", sf = TRUE) %>% rename(fips_char = state_fips) #from urbanmap
counties_sf <- get_urbn_map(map = "counties", sf = TRUE) %>% rename(fips5 = county_fips)



## Kieran Healy code ----

# National Result
results_df %>% 
  # filter(race == "President", id != "0", fips_char == "02", mpc == "1") %>% 
  filter(race == "President", id != "0", mpc == "1") %>% 
  
  select(race, id, fips_char, fips5,  place, lname, votes) %>% 
  group_by(fips5, lname) %>%  # Group by county fips
  summarize(votes = sum(votes)) %>% 
  group_by(lname) %>% 
  summarize(state_total = sum(votes))

# Data from KH package
county_map <- county_map


# County

u <- results_df %>% 
  # filter(race == "President", id != "0", fips_char == "25", mpc == "1") %>% 
  filter(race == "President", id != "0", mpc == "1") %>% 
  
  select(race, id, fips_char, fips5,  place, lname, votes) %>% 
  group_by(fips5, lname) %>%  # Group by county fips
  summarize(votes = sum(votes)) %>% 
  # left_join(tmp, by = "fips5") %>%
  # mutate(state = as.character(state)) %>%
  as_tibble() %>%
  pivot_wider(names_from = lname, values_from = votes) %>%
  left_join(counties_sf, ., by = "fips5") %>%
  mutate(Winner = ifelse(Biden > Trump, "Biden", "Trump"))

mapview(u, zcol = "Winner")

# State results

u <- results_df %>% 
  # filter(race == "President", id != "0", fips_char == "25", mpc == "1") %>% 
  filter(race == "President", id != "0", mpc == "1") %>% 
  select(race, id, fips_char, fips5,  place, lname, votes) %>% 
  group_by(fips_char, lname) %>%  # Group by county fips
  summarize(votes = sum(votes)) %>% 
  # left_join(tmp, by = "fips5") %>%
  # mutate(state = as.character(state)) %>%
  as_tibble() %>%
  pivot_wider(names_from = lname, values_from = votes) %>%
  left_join(states_sf, ., by = "fips_char") %>%
  mutate(Winner = ifelse(Biden > Trump, "Biden", "Trump"))

mapview(u, zcol = "Winner")

# Save geopackage

st_write(obj = u, dsn = "us_elections.gpkg")

###################################-
             # IDF R et Espace ----
###################################-


idf_sf <- st_read(dsn = "statsmappingwithR/statsmappingwithR/02_SpatialData/data/parispc_com.shp", 
                  crs = 2154, 
                  stringsAsFactors = F) %>% st_transform(4326)
# soc_eco <- read.csv("02_SpatialData/data/paris_soc_eco.csv")

paris <- st_read(dsn = "Datasets/RetEspace_Donnees/arrondissements/arrondissements.shp", 
                 crs = 4326, 
                 stringsAsFactors = F) %>% st_transform(crs = 2154)
  
  idf_sf %>% filter(NOM_DEPT == "PARIS")
mapview(paris)  

plot(st_geometry(paris), add=TRUE)
txt <- paste0("\u00A9 OpenStreetMap contributors.",
              " Tiles style under CC BY-SA, www.openstreetmap.org/copyright")
mtext(text = txt, side = 1, adj = 0, cex = 0.7, font = 3)
library(raster)
mtqOSM <- getTiles(x = paris, type = "CartoDB.Positron", crop = TRUE)
CHM_HARV_Cropped <- crop(x = mtqOSM, y = paris)

r3 <- mask(CHM_HARV_Cropped, paris)
tilesLayer(r3)

soc_eco$INSEE_COM <- as.character(soc_eco$CODGEO)

idf_sf <- left_join(idf_sf, #objet 1 : l'objet sf
                    soc_eco, #objet 2 : le data.frame
                    by = "INSEE_COM") # l'identifiant commun


###################################-
#        Movies Data Set -----
###################################-

movies_paris <- st_read(dsn = "statsmappingwithR/statsmappingwithR/03_DataWrangling/data/lieux-de-tournage-a-paris/lieux-de-tournage-a-paris.shp", 
                        crs = 4326, 
                        stringsAsFactors = F)

glimpse(movies_paris)

# Data wrangling

cases <- movies_paris %>% 
  filter(str_detect(nom_tournag, "Emily|ENGRENAGES|DIX POUR CENT|BUREAU DES LÉGENDES")) %>%
  st_transform(2154) %>% 
  mutate(Map = 1) %>%
  mutate(Tournage = ifelse(str_detect(nom_tournag, "ENGREN"), "Engrenages", 
  ifelse(str_detect(nom_tournag, "BUREAU DES"), "Bureau des Légendes",     
  ifelse(str_detect(nom_tournag, "DIX POUR CENT"), "Dix pour Cent",     
  "Emily in Paris"))))


# Mapping

par(mfrow = c(2, 1))

plot(st_geometry(u), col="white",
     border="darkseagreen4",  
     bg = "white", lwd = 2)

propSymbolsLayer(u, var = "Biden", col = "blue")

plot(st_geometry(u), col="white",
     border="darkseagreen4",  
     bg = "white", lwd = 2)

propSymbolsLayer(u, var = "Trump", col = "red")

propSymbolsTypoLayer(cases, var = "Map", var2 = "Tournage", inches = 0.05,
                     add = T, col = viridis(4), legend.var.pos = "none")

layoutLayer("Paris des flics, paris glamour", 
            tabtitle = TRUE, frame = TRUE, author = "Author", sources = "Sources",  north = TRUE,  scale = 5)



movies_paris %>% filter(nom_tournag == "ENGRENAGES SAISON 8") %>% mapview()
