#chargement des differentes library

library(mapview)
library(sf)
library(tidyverse)
library(cartography)
library(raster)
library(viridis)


# on ouvre les fichier shapefile et on les stocke dans deux objets

movies_paris <- st_read(dsn = "data/lieux-de-tournage-a-paris.shp",
                        crs = 4326,
                        stringsAsFactors = F)


idf_sf <- st_read(dsn = "Data/parispc_com.shp",
                  crs = 2154,
                  stringsAsFactors = F)

# on utilise le bon systeme de projection
movies_paris <- st_transform(movies_paris, 2154)

# on cree deux objets avec les tournages par ardt pour 2018 et 2019
# mais cette partie gagnerait a etre effectuee avec une boucle
# plutot qu'avec du copier-coller

tourn_2018 <- movies_paris %>% filter(annee_tourn == "2018") %>% 
                 group_by(ardt_lieu) %>% count()


tourn_2019 <- movies_paris %>% filter(annee_tourn == "2019") %>% 
  group_by(ardt_lieu) %>% count()

# COMMENTAIRE : Oui, c'est bien vrai ! Et je ne vous interdis pas du tout de tester cela !
# Un for i in Years fonctionnerait très bien ici. Sinon, on peut faire du tmap en utilisant
# les facets

# premiere carte : tournages en 2018 par ardt

png("Output/Carte_tournages_2018.png")# Emplacement et nom du fichier

plot(st_geometry(idf_sf %>% filter(NOM_DEPT == "PARIS")),
     col="ivory", #couleur fond de carte 
     border="darkseagreen4", #couleur bordure 
     bg = "white", #couleur du fond
     lwd = 2) #largeur bordure

propSymbolsLayer(tourn_2018, 
                 var = "n", #nom de la variable 
                 legend.pos = "topleft", #position de la légende
                 inches = 0.1, #taille du plus gros symbole
                 legend.style = "e", #légende "étendue"
                 legend.title.txt = "Nombre de tournages", #titre de la légende
                 add = T) #ajouter au fond de carte

layoutLayer(title = "Nombre de tournages par arrondissements en 2018", 
            author = "Auteur: J. Beucler",
            sources = "Source : Open Data - Mairie de Paris",
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)


dev.off() # on enregistre

# deuxieme carte (2019)

png("Output/Carte_tournages_2019.png")# Emplacement et nom du fichier

plot(st_geometry(idf_sf %>% filter(NOM_DEPT == "PARIS")),
     col="ivory", #couleur fond de carte 
     border="darkseagreen4", #couleur bordure 
     bg = "white", #couleur du fond
     lwd = 2) #largeur bordure

propSymbolsLayer(tourn_2019, 
                 var = "n", #nom de la variable 
                 legend.pos = "topleft", #position de la légende
                 inches = 0.1, #taille du plus gros symbole
                 legend.style = "e", #légende "étendue"
                 legend.title.txt = "Nombre de tournages", #titre de la légende
                 add = T) #ajouter au fond de carte

layoutLayer(title = "Nombre de tournages par arrondissements en 2019", 
            author = "Auteur: J. Beucler",
            sources = "Source : Open Data - Mairie de Paris",
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)


dev.off() # on enregistre

# troisieme carte : lieux tournage
# engrenage / emily in Paris sur une meme carte

pdf("Output/Emily_engrenage.pdf", # Emplacement et nom du fichier
    width=7, #largeur en pouces
    height=7 , #hauteur en oouces
    useDingbats=FALSE)

# on cree deux variables, une pour placer les points, une pour la legende
emily_engrenage <- movies_paris %>%
  filter(nom_tournag == "ENGRENAGES SAISON 8" | nom_tournag == "Emily in Paris") %>%
  mutate(Lieux_de_tournage = if_else(nom_tournag == "ENGRENAGES SAISON 8",
                          "Engenages S8", # PETITE ERREUR DE FRAPPE
                          "Emily in Paris")) %>% 
  mutate(tourn_1 = 1)

plot(st_geometry(idf_sf %>% filter(NOM_DEPT == "PARIS")),
     col="white", #couleur fond de carte 
     border="darkseagreen4", #couleur bordure 
     bg = "white", #couleur du fond
     lwd = 2) #largeur bordure


propSymbolsTypoLayer(emily_engrenage,
                     var = "tourn_1", #place les points
                     var2 = "Lieux_de_tournage", #légende
                     inches = 0.045,
                     col = viridis(2), 
                     legend.var.pos = "none",
                     legend.var2.title.txt = "Série", #titre de la légende
                     add = T)


layoutLayer(title = "Lieux de tournages d'Emily in Paris et d'Engrenages
            S8 à Paris", 
            author = "Auteur: J. Beucler",
            sources = "Source : Open Data - Mairie de Paris",
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)
  
dev.off()

# on cree un objet en chargeant les données sur l'élection us

elections_us <- st_read(dsn = "data/us_elections.gpkg",
                        stringsAsFactors = F)

# on cree une variable qualitative (pour le vainqueur) 
# a partir de deux variables quantitatives en stock (Trump et Biden)

elections_us <- elections_us %>% mutate(winner_takes_it_all =
                      if_else(Biden > Trump, "Biden", "Trump"))

# on utilise le bon systeme de projection
#elections_us <- st_transform(elections_us, 4326)

# 1) faire une carte typologique qui montre le vainqueur dans chaque état

png("Output/Carte_us_binaire.png")# Emplacement et nom du fichier

typoLayer(elections_us,
          var = "winner_takes_it_all", 
          col = viridis(2),
          legend.title.txt = "Vainqueur par Etat", 
          legend.pos = "topleft")

layoutLayer(title = "Résultat des élections américaines de 2020", 
  author = "J. Beucler, 2020.", 
  sources = "Source inconnue") #BBC, autant pour moi


dev.off()

# on code le nbr de votants en milliers

elections_us <- elections_us %>% mutate(Trump_mill = Trump / 1000,
                                        Biden_mill = Biden / 1000)



# carte avec nombre de votes pour trump par Etat

png("Output/Carte_us_stock_trump.png")# Emplacement et nom du fichier

plot(st_geometry(elections_us), 
     col="ivory",#fond de carte
     border = "ivory3") #bordures

propSymbolsLayer(elections_us, 
                 var = "Trump_mill", #nom de la variable 
                 legend.pos = "topleft", #position de la légende
                 inches = 0.13, #taille du plus gros symbole
                 legend.style = "e", #légende "étendue"
                 legend.title.txt = "Nombre de votes (en milliers)", #titre de la légende
                 add = T) #ajouter au fond de carte

layoutLayer(title = "Nombre d'électeurs ayant voté Trump", 
            author = "Auteur: J. Beucler",
            sources = "Source inconnue",
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)

dev.off()

#idem pour Biden

png("Output/Carte_us_stock_biden.png")# Emplacement et nom du fichier

plot(st_geometry(elections_us), 
     col="ivory",#fond de carte
     border = "ivory3") #bordures

propSymbolsLayer(elections_us, 
                 var = "Biden_mill", #nom de la variable 
                 legend.pos = "topleft", #position de la légende
                 inches = 0.13, #taille du plus gros symbole
                 legend.style = "e", #légende "étendue"
                 legend.title.txt = "Nombre de votes (en milliers)", #titre de la légende
                 add = T) #ajouter au fond de carte

layoutLayer(title = "Nombre d'électeurs ayant voté Biden", 
            author = "Auteur: J. Beucler",
            sources = "Source inconnue",
            tabtitle = T,
            frame = TRUE, 
            scale = NULL)

dev.off()

# Très bonne maîtrise. Vous avez déjà des acquis en programmation, donc tout cela est facile pour vous. 
# Néanmoins, vous n'avez pas repéré le doublon du 16e arrondissement, et donc la nécessité de recoder la variable. 
# Une remarque cependant : pensez à regarder les détails des fonctions. Ainsi, la rubrique d'aide de 
# propSymbolsLayer précise comment comparer deux cartes de stock, ce qui permettrait de visualiser les votes 
# pour Trump et Biden avec une légende similaire. 