library(tidyverse)
library(mapview)
library(sf)
library(socviz)
library(urbnmapr) # For map
library(cartography)
library(viridis)
library(tmap)
library(hrbrthemes)
library(ggrepel)

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
  left_join(states_sf, ., by = "fips_char") 

# National Result
results_df %>% 
  # filter(race == "President", id != "0", fips_char == "02", mpc == "1") %>% 
  filter(race == "President", id != "0", mpc == "1") %>% 
  
  dplyr::select(race, id, fips_char, fips5,  place, lname, votes) %>% 
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




u <- u %>% mutate(Winner = ifelse(Biden > Trump, "Biden", "Trump")) 
# mapview(u, zcol = "Winner")






#Calculer mes variables
u

u <- u  %>%
          mutate(TrumpPct = Trump/(Biden+Trump)*100, 
                  BidenPct = Biden/(Biden+Trump)*100) %>%
          mutate(Winner = ifelse(Biden > Trump, "Biden", "Trump"))


pdf("Elections.pdf")

# Barplot
barplot(table(u$Winner), col = c("blue", "red"))

# Histogramme


par(mfrow = c(1, 1)) #diviser écran en 2 colonnes
hist(u$BidenPct)
hist(u$TrumpPct)


# Boxplot

boxplot(u$TrumpPct)
boxplot(u$BidenPct)

# Boxplot à deux variables
par(mfrow = c(1, 1)) #revenir à écran unique
boxplot(u$BidenPct, u$TrumpPct, names = c("Biden", "Trump"))

dev.off()











# Save geopackage

st_write(obj = u, dsn = "us_elections.gpkg")

u <- st_read("Datasets/US_Elections_2020/us_elections.gpkg")

###################################-
             # IDF R et Espace ----
###################################-


idf_sf <- st_read(dsn = "statsmappingwithR/statsmappingwithR/02_SpatialData/data/parispc_com.shp", 
                  crs = 2154, 
                  stringsAsFactors = F) 

# soc_eco <- read.csv("02_SpatialData/data/paris_soc_eco.csv")

paris <- st_read(dsn = "Datasets/RetEspace_Donnees/arrondissements/arrondissements.shp", 
                 crs = 4326, 
                 stringsAsFactors = F) %>% st_transform(crs = 2154)
  
paris <-  idf_sf %>% filter(NOM_DEPT == "PARIS")
  
  paris <- idf_sf %>% filter(NOM_DEPT == "PARIS") %>% mutate(ardt_lieu = as.character(c_arinsee - 100))

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
                        stringsAsFactors = F) %>% st_transform(2154)

glimpse(movies_paris)
table(movies_paris$type_tourna)

movies_paris <- movies_paris %>%
  mutate(Type = case_when(type_tourna != "Long métrage" ~ "Série", TRUE ~ "Film")) %>%
  table(movies_paris$Type)

# " Tournage par année"
tournages_year <-  movies_paris %>%
  filter(!(ardt_lieu %in% c(93200, 93500, 94320))) %>%
  st_set_geometry(NULL) %>%
  mutate(ardt_lieu = as.numeric(ardt_lieu),
         ardt_lieu = case_when(ardt_lieu != 75116 ~ ardt_lieu - 75000, # Pourquoi cette manip ? Parce qu'il y a deux codes pour le 16 : 75016 et 75116. J'enlève le 75... pour que le graphe suivant soit plus lisible
                               ardt_lieu == 75116 ~ ardt_lieu - 75100)) %>%
  mutate(Type = case_when(type_tourna != "Long métrage" ~ "Série", TRUE ~ "Film")) %>%
  group_by(ardt_lieu, annee_tourn, Type) %>%
  summarise(TotalType = n()) %>%
  ungroup() %>%
  # group_by(ardt_lieu, annee_tourn) %>%
  # mutate(TotalAll = sum(TotalType)) %>%
  relocate(ardt_lieu, TotalType, everything()) %>%
  pivot_wider(names_from = "Type", values_from = "TotalType")


ggplot(tournages_year, aes(x = Film, y = Série, label = ardt_lieu)) + 
  geom_point() +
  geom_text_repel(size = 4, segment.alpha = 0.5) +
  
  labs(x="Nombre de tournages de film", y="Nombre de tournages de série",
       title="Les tournages à Paris",
       subtitle="Paris des films, Paris des séries",
       caption="Sources: Paris Open data") +

  theme_ipsum_rc() +
  geom_smooth(method = "lm") +
  # theme_ft_rc() +
facet_wrap(.~ annee_tourn) 


plot(idf_sf$TXCHOMA07, idf_sf$POUV07)
# Tous les tournages


tournages_all <-  movies_paris %>%
  filter(!(ardt_lieu %in% c(93200, 93500, 94320))) %>%
  st_set_geometry(NULL) %>%
  mutate(ardt_lieu = as.numeric(ardt_lieu),
         ardt_lieu = case_when(ardt_lieu != 75116 ~ ardt_lieu, # Pourquoi cette manip ? Parce qu'il y a deux codes pour le 16 : 75016 et 75116. J'enlève le 75... pour que le graphe suivant soit plus lisible
                               ardt_lieu == 75116 ~ 75016)) %>%
  mutate(Type = case_when(type_tourna != "Long métrage" ~ "Série", TRUE ~ "Film")) %>%
  group_by(ardt_lieu, Type) %>%
  summarise(TotalType = n()) %>%
  ungroup() %>%
  relocate(ardt_lieu, TotalType, everything()) %>%
  pivot_wider(names_from = "Type", values_from = "TotalType") %>%
  mutate(ardt_lieu = as.character(ardt_lieu)) %>%
  mutate(TotalAll = Film + Série) %>%
  mutate(ardt_label = as.numeric(ardt_lieu),
         ardt_label = case_when(ardt_label != 75116 ~ ardt_label - 75000, # Pourquoi cette manip ? Parce qu'il y a deux codes pour le 16 : 75016 et 75116. J'enlève le 75... pour que le graphe suivant soit plus lisible
                                ardt_label == 75116 ~ ardt_label - 75100))

# FIchier SF
tournages_all <- merge(paris, tournages_all)

# Plot

ggplot(tournages_all, aes(x = Film, y = Série, label = ardt_label)) + 
  geom_point(size = 5, alpha = 0.9) +
  scale_x_continuous(expand=c(0,0), limits=c(10, 450)) +
  scale_y_continuous(expand=c(0,0), limits=c(10, 350)) +
    geom_text_repel(size = 4, segment.alpha = 0.5) +
  labs(x="\nNombre de tournages pour les films", y="Nombre de tournages pour les séries\n",
       title="Paris des films, Paris des séries",
       subtitle="Tournages publics dans les arrondissements de Paris (2016-2019)",
       caption="J. Migozzi, 2020.\nSources : Open Data Paris, 'Lieux de tournage à Paris'.") +

  geom_smooth(method = "lm", se = F, ) +
theme_ipsum_rc(grid="XY", axis="xy")


table(movies_paris$annee_tourn)
# Stats
ggplot(tournages_all, aes(x = ardt_lieu, y = TotalAll)) +
  geom_col()

## Mapping Symbols Prop

plot(st_geometry(tournages_all))
propSymbolsLayer(tournages_all, var = "TotalAll", add = "T", inches = 0.2, legend.style = "e")

propSymbolsLayer(tournages_all, var = "Film", add = "T", inches = 0.2, fixmax = 600)
propSymbolsLayer(tournages_all, var = "Série", add = "T", inches = 0.2, fixmax = 600,
                 col = "blue", legend.pos = "topleft")


### Regression linénaire #####  -----
library(kableExtra)
reg <- lm(Série ~ Film, data = tournages_all) 

#Extract residuals
tournages_all$residuals <- reg$residuals
hist(tournages_all$residuals, breaks = 10)
library(jtools)
plot_summs(reg)

#Carto residus
choroLayer(tournages_all, var = "residuals", method = "sd", nclass = 5,
           col =  carto.pal(pal1 = "blue.pal", n1 =2, 
                            pal2 = "green.pal", n2 = 3), 
           legend.pos = "topleft")


#Pourquoi le premier est si représenté ? Que s'est-il passé ? 

first <- movies_paris %>% filter(ardt_lieu == "75001")
head(freq(first$nom_tournag, cum = TRUE, sort = "dec", total = TRUE))

# Ajouter Emily à la carte

choroLayer(tournages_all, var = "residuals", method = "sd", nclass = 5,
           col =  carto.pal(pal1 = "blue.pal", n1 =2, 
                            pal2 = "green.pal", n2 = 3), 
           legend.pos = "topleft")

propSymbolsTypoLayer(movies_paris %>% 
            filter(str_detect(nom_tournag, "Emily")) %>%
            mutate(Map = 1), 
          var = "Map", 
          var2 = "nom_tournag", 
          inches = 0.05, 
          col = "red", 
          add = T,)


# Data wranggling -----

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
