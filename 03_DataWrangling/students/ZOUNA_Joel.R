#Joel ZOUNA
#Script sur les lieux de tournages 


# Je vous ai fourni un script annoté : pourquoi ne pas l'utiliser ? 



library(sf)
library(tidyverse)
library(mapview)



tournage_sf <- st_read(dsn = "data/lieux-de-tournage-a-paris.shp", crs = 4326, stringsAsFactors = F)
plot(st_geometry(tournage_sf))

tournage_sf <- movies_paris
# Pensez à davantage annoter votre script
tournage_proj <- st_transform(tournage_sf, 2154)

plot(st_geometry(tournage_proj))
head (tournage_proj) #attention aux espaces. 
summary(tournage_proj$id_lieu) 
# COMMENTAIRE : 
#1 la fonction summary s'applique à une variable quantitative
#2. penser à utiliser la fonction glimpse, plus simple pour décrire et se renseigner sur un objet
glimpse(tournage_proj)

#Le tableau offre une distribution présentant 7742 individus
# SOLUION Le tableau "comporte 7742 individus", ou unités spatiales. 
#avec des variables quantitatives notamment des identifiants et des coordonnees et des variables qualitatives comme les adresses et les noms des arrondissements

tourna_web <- filter(tournage_proj,type_tourna =="Série Web")
nrow(tourna_web)

# SOLUTION : une fonction table(), qui calcule fréquence de chaque modalité. Bien plus rapide !
table(movies_paris$type_tourna)

#Il y a 282 series web qui ont ete tournees

boite_tournage <- tournage_proj %>% group_by(nom_product) %>% summarise(n= n())
# COMMENTAIRE : ok, mais vous appliquez la fonction summarise à des données géométriques.
# Le calcul est donc lourd : l'ordinateur doit regrouper 7742 points. 
# Mieux vaut donc enlever lees données géoométriques pour faire un tel calcul, puisqu'il s'agit d'obtenir un data.frale
# SOLUTION
boite_tournage <- tournage_proj %>% 
  st_set_geometry(NULL) %>% # enlève les données géométrique
  group_by(nom_product) %>% 
  summarise(n= n())



movies_paris <- c("tvshowsparis","long_paris" ) # A quoi sert ce vecteur ? 

tvshowsparis <- filter(tournage_proj, type_tourna == "Série TV") # OK
long_paris <- filter(tournage_proj, type_tourna == "Long métrage") #OK

# Pensez SVP à annoter votre script avec les numéros des exercices : c'est difficilement lisible. 

tournage_numb <- tournage_proj  %>%  group_by(ardt_lieu,annee_tourn) %>% summarise(n = n())

tournage_numbmax2016 <- filter(tournage_numb, annee_tourn==2016)
#en 2016  c'est le 75016 qui a accueilli le plus de tournage avec 240 
tournage_numbmax2019 <- filter(tournage_numb, annee_tourn==2019)
##en 2019  c'est le 75018 qui a accueilli le plus de tournage avec 144


# COMMENTAIRE ; 
# 1. OK, mais le code en tant que tel ne montre pas les résultats. 
# 2. Il y a beaucoup d'objets intermédiaires créés. 
# SOLUTION pour 2016
tournages_2016 <- tournage_proj %>% 
  filter(annee_tourn == 2016) %>% # on filtre sur l'année 2016
  group_by(ardt_lieu) %>% # On regroupe par arrondissement
  summarise(total = n())  %>% # Calcul de la fréquence
  arrange(desc(total)) #classer le dataframe par ordre décroissant
tournages_2016 # on affiche l'objet pour avoir toutes les infos dans la console. 

# Vous pouvez faire pareil pour 2019 

# INCOMPLET : Je ne trouve pas la question : Quel est l'arrondissement de Paris qui accueille le plus de long métrage en 2018 ? 



Emily <- tournage_proj %>% 
  select(ardt_lieu,adresse_lie,nom_tournag) %>%  #COMMENTAIRE : OK, mais pas utile. 
  filter(nom_tournag=="Emily in Paris")

arrond_emily <- Emily %>% group_by(ardt_lieu) %>% summarise (n=n())
table(arrond_emily$ardt_lieu)

# Ok, mais vou pouvez enchainer les opérations : 
# SOLUTION PLUS SIMPLE : 
tournage_proj %>% 
  filter(nom_tournag=="Emily in Paris") %>%
  group_by(ardt_lieu) %>%
  summarise (n=n())

#La serie Emily in Paris est tournee dans le 75001 75002 75003 75004 75005 
#75006 75007 75008 75009 75010 75011 75012 75016 75018 et le 75116
# COMMETAIRE Certes, mais où est-elle concentrée ? 



Engre_tourn <- filter(tournage_proj, nom_tournag=="ENGRENAGES SAISON 8")

mapview(Engre_tourn,Zcol="adresse_lie")

table(Engre_tourn$ardt_lieu)

#Les lieux de tournages de la saison 8 de la serie engrenages sont quasiment tous situee dans 
#les arrondissements qui sont sur la rive droite de la seine  sauf celui qui a ete tourne au 9 rue de sainte-hélène dans le 75013 paris.
# Plus encore, les scenes sont toutes tournees sur les rues a l'interieur de la grande couronne. 
# COMMENTAIRE : ce sont en effet en majorité des localisations périphériques, dans des quartiers plutôt populaires,
# et donc plus adaptées pour cette série policière, que je vous recommande.
#Les arrondissements concernes sont le 75008 75009 75010 75011 75012 75013 75016 75017 75018 75019 75020 et le 75116

beatspermin_tourn <- filter(tournage_proj, nom_tournag == "120 BATTEMENTS PAR MINUTE")
# Attetion, il faut l'écire en majuscules

mapview(beatspermin_tourn,zcol="adresse_lie") #attention l'argument zcol ne contient pas de majuscule.

# COMMENTAIRE
# Bon travail ! Vous pouvez davantage alléger votre code, en essyant de garder le moins possible d'objets intermédiaires. 
# un net progrès dans la syntaxe et la manipulation des objets. 
