# Chargement des packages----
library(tidyverse)
library(ggrepel)
library(questionr)
library(sf)
library(mapview)
library(cartography)

# Import des données----
movies_paris <- st_read("data/lieux-de-tournage-a-paris.shp", stringsAsFactors = FALSE) %>%
  st_transform(crs = 2154)

str(movies_paris) # 7742 lieux de tournages, 13 variables (nominales, catégorielles, dates, géométries)

# Combien de lieux de tournages sont associés à une série web ?
freq(movies_paris$type_tourna) # 283 lieux associés à une Série Web soit 3.7% des lieux de tournage

# Dataframe contenant le nombre de tournages réalisés par boîte de production
prod <- movies_paris %>%
  # st_set_geometry(NULL) %>%
  group_by(nom_product) %>%
  count() 
# On constate qu'il y aurait à recoder les noms de boîtes de production. Les données ne sont pas "propres".
# Exact = ON peut aussi rajouter un argument st_set_geometry(NULL), pour éviter d'avoir un calcul trop lourd. 

# Distinguons séries TV et longs métrages
tvshowsparis <- movies_paris %>%
  filter(type_tourna == "Série TV")

long_paris <- movies_paris %>%
  filter(type_tourna == "Long métrage")

# Quel est l'arrondissement de Paris qui accueille le plus de tournages en 2016 ? En 2019 ?
ardt_tournages <- movies_paris %>%
  filter(!(ardt_lieu %in% c(93200, 93500, 94320))) %>%
  mutate(ardt_lieu = as.numeric(ardt_lieu),
         ardt_lieu = case_when(ardt_lieu != 75116 ~ ardt_lieu - 75000, # Pourquoi cette manip ? Parce qu'il y a deux codes pour le 16 : 75016 et 75116. J'enlève le 75... pour que le graphe suivant soit plus lisible
                        ardt_lieu == 75116 ~ ardt_lieu - 75100)) %>%
  group_by(ardt_lieu, annee_tourn) %>%
  summarise(nb_tournages = n()) %>%
  st_set_geometry(NULL)
  
ardt_long <- long_paris %>%
    filter(!(ardt_lieu %in% c(93200, 93500, 94320))) %>%
    mutate(ardt_lieu = as.numeric(ardt_lieu),
           ardt_lieu = case_when(ardt_lieu != 75116 ~ ardt_lieu - 75000,
                          ardt_lieu == 75116 ~ ardt_lieu - 75100)) %>%
  group_by(ardt_lieu, annee_tourn) %>%
    summarise(nb_long = n()) %>%
    st_set_geometry(NULL)

ardt <- left_join(ardt_tournages, ardt_long)

# COMMENTAIRE : très beau ! On aimerait presque ajouter un 'e' à chaque ligne pour avoir 19e. Mais ça ne sert à rien.

ggplot(ardt) + 
  geom_text_repel(aes(x = nb_tournages, y = nb_long, label = ardt_lieu), size = 4, segment.alpha = 0.5) +
  facet_wrap(.~ annee_tourn) +
  xlab("Nombre de tournages") +
  ylab("Nombre de longs métrages")

# On constate facilement que l'arrondissement de Paris qui accueille le plus de tournages
# en 2016 et en 2019 est le 16e. En revanche, l'arrondissement qui accueille le plus de longs métrages 
# en 2018 est le 19e

# Dans quels arrondissements se déroule la série "Emily in Paris" ?
emilie <- movies_paris %>%
  filter(nom_tournag == "Emily in Paris") %>%
  group_by(ardt_lieu) %>%
  count()
# Se déroule dans tous les arrondissements sauf le 13, 14, 15, 19, 20.

# Lieux de tournage de la huitième saison de "Engrenages" = BIEN
engrenages <- movies_paris %>%
  filter(str_detect(nom_tournag, "ENGRENAGES SAISON 8") == TRUE ) %>%
  group_by(ardt_lieu) %>%
  count()

mapview(engrenages, zcol = "n")

# La série est principalement associée à la rive droite et aux zones les plus populaires de Paris,
# avec des incursions dans l'Ouest parisien
# Préférence pour le 18e et le 20e arrondissement

# Lieux de tournage de "120 battements par minute"
battements <- movies_paris %>%
  filter(str_detect(nom_tournag, "120") == TRUE ) %>%
  group_by(ardt_lieu) %>%
  count()

mapview(battements, zcol = "n")

# Forte concentration dans les arrondissements centraux, à bien des égards l'inverse de Engrenages