

#----------------------------#
#       NOM Prénom
#------------------------

# Ici, précisez les packages dont vous avez besoin
library()


#--------------------------#
#     RAPPEL
#------------------------

# Avec le signe #, je peux "désactiver" une ligne de code. Ce que j'écris n'est donc pas exécutable.
# Je peux donc m'en servir pour prendre des notes, ou structurer mon script. 
# A l'inverse, quant je n'utilise pas de signe #, le code est donc exécutable : 

1+1
mean(c(1, 8, 9)) #moyenne d'un vecteur de 3 valeurs : 1, 8, 9

#----------------------------#
#          Exercice 1 -----
#----------------------------#
#Complétez les lignes suivantes 

#1. Ouvrez le fichier csv
murders <- read.csv("")

#2. Calculer le nombre de meurtres par grande région
total_murd <- murders %>%
              group_by(...) %>%
              summarise(Total = sum(...))

#3. ratio
total_murd <- total_murd %>% 
              mutate(....)


#----------------------------#
#          Exercice 2 -----
#----------------------------#
# Calculez et visualisez la densité de  population par département avec le fichier `parispc_com.shp`

#1. Ouvrez le shapefile parispc_com et 

idf_sf <- #....


#2. Ouvrez le fichier csv paris_soc_eco
  
soc_eco <- #....
  
#3. Préparez les fichiers pour une jointure avec la fonction mutate

  soc_eco <- soc_eco %>% 
            mutate(INSEE_COM = as.character(...))

#4. Réalisez une jointure

idf_sf <- left_join(...)

#5. Calculez la densité par département
dpt_sf <- idf_sf %>%
          group_by(...) %>% # regrouper les communes
          summarise(... = sum(...)) %>% #calculer somme de population en 2008
          ungroup() %>% #pour dégrouper. Ne pas toucher !
          mutate(... = st_area(.)) %>% #calculer surface en M2
          mutate(... = as.numeric(.../10^6)) %>% #surface en km2
          mutate(... = .../...) #calculer densité de population

#6. Visualiser la densité par département

# Avec la fonction plot

...

# Avec la fonction mapview

...


#----------------------------#
#          Exercice 3 -----
#----------------------------#

# A vous de jouer.

# Conseil : prenez le temps, crayon à la main, de décomposer la réflexion et les étapes pour répondre aux questions.


  