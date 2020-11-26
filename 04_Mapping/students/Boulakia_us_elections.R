#### Carte des résultats des élections américaines de 2020, par État

# Chargement des packages----
library(tidyverse)
library(sf)
library(cartography)
library(rgdal)
library(raster)

# Import et recodage des données----
us_elections <- st_read("data/us_elections.gpkg") %>%
  mutate(winner = if_else(Biden > Trump, "Biden", "Trump"))

# Cartographie des résultats----
pdf("graphiques/Boulakia_us_elections.pdf",
    width = 7,
    height = 7,
    useDingbats = FALSE
    )

## États remportés par Trump et Biden
plot(st_geometry(us_elections))

typoLayer(x = us_elections,
          var = "winner",
          col = c("red", "blue"),
          legend.pos = "topleft",
          legend.title.txt = "Vainqueur",
          add = TRUE)

layoutLayer(title = "États remportés par Trump et Biden en 2020",
            sources = "Sources: Associated Press, 2020",
            author = "Auteur: Théo Boulakia, 2020")

## Nombre de voix obtenues par Biden et Trump dans chaque État
map_nbvotes <- function(x, y) {
  
  if(x == "Biden") {
    y = "blue"
  }
  else {
    y = "red"
  }
  
  
  plot(st_geometry(us_elections))
  
  propSymbolsLayer(x = us_elections,
                   var = x,
                   inches = 0.15,
                   col = y,
                   legend.pos = "topleft",
                   legend.title.txt = paste("Votes", x),
                   add = TRUE)
  
  layoutLayer(title = paste("Nombre de voix obtenues par", i ,"dans chaque État"),
              sources = " Sources: Associated Press, 2020",
              author = "Auteur: Théo Boulakia, 2020")
  
}   


par(mfrow = c(1, 2)) #éventuel

for (i in c("Biden", "Trump")) {
  map_nbvotes(i)
}

dev.off() # The end

# Excellent travail, et jolie boucle. Je vous encourage donc à lire le chapitre 6 du Manuel de Lambert et Zanin. 
# La rubrique d'aide de la fonction propSymbolsLayer détaille comment rendre deux cartes en symboles proportionnels comparables. 
# Même si de fait, les cartes représentées ici ne sont finalement que des cartes qui reflètent la population de chaque état !
# Pensez aussi éventuellement à :
# 1. Changer le format du pdf pour passer en mode paysage en paramétrant hauteur et largeur
# 2. mettre les deux cartes côtes à côte avec un par(mfrow = c(1, 2))