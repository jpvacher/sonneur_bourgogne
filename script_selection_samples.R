###################################################################################
#Préparation des grilles SIG pour le protocole de suivi du Sonneur à ventre jaune
#Script rédigé par Jean-Pierre Vacher
#Mise à jour 7 février 2022
###################################################################################

#Charger les packages nécessaires
x=c("here","dismo","ggmap", "rgdal", "rgeos", "maptools", "plyr","dplyr", "tidyr", "tmap", "raster","mapdata","sp","spdep","colorRamps","ggplot2","gridExtra")
lapply(x, library, character.only=TRUE)

#Vérifier quels formats on peut lire avec rgdal
ogrDrivers()$name

#########################
#PREPARATION DES DONNEES
#########################

#Sources des couches utilisées
#data_bombina : données SHNA
#n_zsc_s_r27.shp : couche des ZSC téléchargée sur le site https://geo.data.gouv.fr/en/datasets/9cd412f2e09b111c428bc93954b6aa7904fe19fe
#bâtiments : couche BDTOPO 2020 téléchargée sur le site de l'IGN professionnel

#Lire la couche shape des données de points précis du sonneur
bombina=readOGR(dsn="data_bombina.shp", layer="data_bombina")
bombina=spTransform(bombina, CRS("+init=epsg:2154")) #on assigne le système de projection Lambert 93 à la couche bombina

#lire la couche des sites Natura 2000
n2000=readOGR(dsn="n_zsc_s_r27.shp", layer="n_zsc_s_r27")
n2000=spTransform(n2000, CRS("+init=epsg:2154"))
citeaux=n2000[n2000@data$CODE_FR=="FR2601013",]
sud.morvan=n2000[n2000@data$CODE_FR=="FR2601015",]
clunisois=n2000[n2000@data$CODE_FR=="FR2601016",]
amognes=n2000[n2000@data$CODE_FR=="FR2601014",]

#lire les couches des zones avec bâtiments
#placer les dossiers qui contiennent les couches dans le dossier de travail (= le dossier où se trouve le présent script)
bati58=readOGR(dsn="BDTOPO_3-0_TOUSTHEMES_SHP_LAMB93_D058_2020-12-15/BDTOPO/1_DONNEES_LIVRAISON_2021-01-00019/BDT_3-0_SHP_LAMB93_D058-ED2020-12-15/BATI/BATIMENT.shp", layer="BATIMENT")
bati58=spTransform(bati58, CRS("+init=epsg:2154"))
bati21=readOGR(dsn="BDTOPO_3-0_TOUSTHEMES_SHP_LAMB93_D021_2020-12-15/BDTOPO/1_DONNEES_LIVRAISON_2021-01-00019/BDT_3-0_SHP_LAMB93_D021-ED2020-12-15/BATI/BATIMENT.shp", layer="BATIMENT")
bati21=spTransform(bati21, CRS("+init=epsg:2154"))
bati71=readOGR(dsn="BDTOPO_3-0_TOUSTHEMES_SHP_LAMB93_D071_2020-12-15/BDTOPO/1_DONNEES_LIVRAISON_2021-01-00019/BDT_3-0_SHP_LAMB93_D071-ED2020-12-15/BATI/BATIMENT.shp", layer="BATIMENT")
bati71=spTransform(bati21, CRS("+init=epsg:2154"))


#créer une grille avec des mailles de 300x300m sur les quatre sites d'étude
#Citeaux
citeaux.prive=readOGR(dsn="foret_privee_citeaux.shp",layer="foret_privee_citeaux")
citeaux.prive=spTransform(citeaux.prive, CRS("+init=epsg:2154"))
citeaux2=gDifference(citeaux,citeaux.prive) #on retire les forêts privées
grid.citeaux=raster(citeaux2) #on créé une grille en raster par dessus la couche citeaux2
res(grid.citeaux)=300 #résolution de la maille en mètres
grid.citeaux=rasterToPolygons(grid.citeaux) #on transforme la grille raster en couche vectorielle de polygones
grid.citeaux=crop(grid.citeaux, citeaux2) #on ajuste les limites de la grille au site Natura 2000
grid.citeaux@data$ID_plot=c(1:nrow(grid.citeaux@data)) #on assigne à chaque maille un indentifiant unique
grid.citeaux@data$layer=NULL #on retire la colonne layer
grid.citeaux@data$area=raster::area(grid.citeaux) #on ajoute une colonne qui calcule la surface de chaque polygone
grid.citeaux2=grid.citeaux[grid.citeaux@data$area==90000,] #on exlut toutes les mailles tronquées qui se trouvent en marge
intersect=raster::intersect(bati21,grid.citeaux2) #on calcule l'intersection entre le bâti et la maille du site Natura 2000 pour retirer les surfaces en bâti
grid.citeaux2=grid.citeaux[grid.citeaux@data$area>=(90000*20)/100,] #on sélectionne que les mailles qui contiennent jusqu'à 20% de surfaces baties
raster::shapefile(grid.citeaux2,file="grid_citeaux_trim.shp")
#On ouvre dans QGIS et on applique la fonction difference (en metant la couche grid_citeaux_trim en premier) dans le menu vector entre la couche grid_sudMorvan_trim et la couche bati58 (équivaut à gDifference de rgeos)
#Puis on sauvegarde la couche avec le nom grille_globale_citeaux en epsg:2154
grid.citeaux=readOGR(dsn="grille_globale_citeaux.shp", layer="grille_globale_citeaux")
#grid.citeaux=grid.citeaux[grid.citeaux@data$area==90000,] #on sélectionne que les mailles qui sont dépourvues de surfaces baties
grid.citeaux@data$area=NULL #on retire la colonne area
grid.citeaux@data$area2=NULL #on retire la colonne area2
grid.citeaux@data$area=raster::area(grid.citeaux) #on recalcule la surface des polygones
grid.citeaux1=grid.citeaux[grid.citeaux@data$area>=(90000-((90000*5)/100)),] #on sélectionne que les mailles qui contiennent jusqu'à 5% de surfaces baties
grid.citeaux2=grid.citeaux[grid.citeaux@data$area==90000,] #on sélectionne que les mailles qui sont dépourvues de surfaces baties
grid.citeaux=rbind(grid.citeaux1, grid.citeaux2) #on combine les deux couches


#Sud Morvan
grid.sud.morvan=raster(sud.morvan)
res(grid.sud.morvan)=300
grid.sud.morvan=rasterToPolygons(grid.sud.morvan)
grid.sud.morvan=crop(grid.sud.morvan, sud.morvan)
grid.sud.morvan@data$ID_plot=c(1:nrow(grid.sud.morvan@data))
grid.sud.morvan@data$layer=NULL
proj4string(grid.sud.morvan)=CRS("+init=epsg:2154")
grid.sud.morvan@data$area=raster::area(grid.sud.morvan)
grid.sud.morvan2=grid.sud.morvan[grid.sud.morvan@data$area==90000,] #on exlut toutes les mailles tronquées qui se trouvent en marge
raster::shapefile(grid.sud.morvan2, file="grid_sudMorvan_trim.shp")
#On ouvre dans QGIS et on applique la fonction difference (en metant la couche grid_sudMorvan_trim en premier) dans le menu vector entre la couche grid_sudMorvan_trim et la couche bati58
#Puis on sauvegarde la couche avec le nom grille_globale_sudMorvan (équivaut à gDifference de rgeos)
grid.sud.morvan=readOGR(dsn="grille_globale_sudMorvan.shp", layer="grille_globale_sudMorvan")
grid.sud.morvan@data$area=NULL
grid.sud.morvan@data$area2=NULL
grid.sud.morvan@data$area=raster::area(grid.sud.morvan)
grid.sud.morvan1=grid.sud.morvan[grid.sud.morvan@data$area>=(90000-((90000*5)/100)),] #on sélectionne que les mailles qui contiennent jusqu'à 5% de surfaces baties
grid.sud.morvan2=grid.sud.morvan[grid.sud.morvan@data$area==90000,] #on sélectionne que les mailles qui sont dépourvues de surfaces baties
grid.sud.morvan=rbind(grid.sud.morvan1, grid.sud.morvan2)


#Clunisois
grid.clunisois=raster(clunisois)
res(grid.clunisois)=300
grid.clunisois=rasterToPolygons(grid.clunisois)
grid.clunisois=crop(grid.clunisois, clunisois)
grid.clunisois@data$ID_plot=c(1:nrow(grid.clunisois@data))
grid.clunisois@data$layer=NULL
proj4string(grid.clunisois)=CRS("+init=epsg:2154")
grid.clunisois@data$area=raster::area(grid.clunisois)
grid.clunisois=grid.clunisois[grid.clunisois@data$area==90000,] #on exlut toutes les mailles tronquées qui se trouvent en marge
raster::shapefile(grid.clunisois, file="grille_clunisois_trim.shp")
#On ouvre dans QGIS et on applique la fonction difference (en metant la couche grid_clunisois_trim en premier) dans le menu vector entre la couche grid_clunisois_trim et la couche bati71 (équivaut à gDifference de rgeos)
#Puis on sauvegarde la couche avec le nom grille_globale_clunisois
grid.clunisois=readOGR(dsn="grille_globale_clunisois.shp", layer="grille_globale_clunisois")
grid.clunisois@data$area=NULL
grid.clunisois@data$area2=NULL
grid.clunisois@data$area=raster::area(grid.clunisois)
grid.clunisois1= grid.clunisois[grid.clunisois@data$area>=(90000-((90000*5)/100)),] #on sélectionne que les mailles qui contiennent jusqu'à 5% de surfaces baties
grid.clunisois2= grid.clunisois[grid.clunisois@data$area==90000,] #on sélectionne que les mailles qui sont dépourvues de surfaces baties
grid.clunisois=rbind(grid.clunisois1, grid.clunisois2)


#Amognes
grid.amognes=raster(amognes)
res(grid.amognes)=300
grid.amognes=rasterToPolygons(grid.amognes)
grid.amognes=crop(grid.amognes, amognes)
grid.amognes@data$ID_plot=c(1:nrow(grid.amognes@data))
grid.amognes@data$layer=NULL
proj4string(grid.amognes)=CRS("+init=epsg:2154")
grid.amognes@data$area=raster::area(grid.amognes)
grid.amognes=grid.amognes[grid.amognes@data$area==90000,] #on exlut toutes les mailles tronquées qui se trouvent en marge
raster::shapefile(grid.amognes, file="grille_amognes_trim.shp")
#On ouvre dans QGIS et on applique la fonction difference (en metant la couche grid_amognes_trim en premier) dans le menu vector entre la couche grid_amognes_trim et la couche bati58 (équivaut à gDifference de rgeos)
#Puis on sauvegarde la couche avec le nom grille_globale_amognes
grid.amognes=readOGR(dsn="grille_globale_amognes.shp", layer="grille_globale_amognes")
grid.amognes@data$area=NULL
grid.amognes@data$area2=NULL
grid.amognes@data$area=raster::area(grid.amognes)
grid.amognes1= grid.amognes[grid.amognes@data$area>=(90000-((90000*5)/100)),] #on sélectionne que les mailles qui contiennent jusqu'à 5% de surfaces baties
grid.amognes2= grid.amognes[grid.amognes@data$area==90000,] #on sélectionne que les mailles qui sont dépourvues de surfaces baties
grid.amognes=rbind(grid.amognes1, grid.amognes2)


########################################
#GENERER L'ECHANTILLON POUR CHAQUE SITE
########################################

##########
#Citeaux
##########

#Première phase : on créé une sous-grille qui ne contient que des mailles positives, puis on sélectionne au hasard 50 mailles au sein de ce sous-ensemble
intersect.positive=raster::intersect(grid.citeaux3, bombina) #on créé un objet qui contient les mailles dans lesquelles se trouvent au moins un point de sonneur
intersect.positive=unionSpatialPolygons(intersect.positive, intersect.positive@data$ID_plot) #on regroupe les polygones par ID_plot
id=sapply(slot(intersect.positive,"polygons"), function(x)slot(x,"ID")) #on créé un slot polygon et un slot ID
intersect.positive.df=data.frame(ID=1:length(intersect.positive),row.names=id) #on créé une data frame qui sera la table attributaire de l'élément intersect.positive, et qui contient les éléments de id
intersect.positive=SpatialPolygonsDataFrame(intersect.positive, intersect.positive.df) #on ajoute la table attributaire à l'élément intersect.positive
class(intersect.positive) #on vérifie la classe de l'objet intersect positive
sum(gIsValid(intersect.positive, byid=T)==FALSE) #permet de vérifier que la géométrie est correcte, c'est-à-dire qu'il n'y a pas de polygones mal placés ou avec des erreurs, le résultat de cette formule doit être égal à zéro, sinon c'est qu'il y a un problème de géométrie
intersect.positive #on regarde à quoi ça ressemble
# 50 polygones

sample1=sample(intersect.positive@data$ID, 50, replace=F) #on sélectionne au hasard 50 polygones au sein de intersect.positive
sample1=sort(sample1) #c'est juste une suite de nombre
sample1=intersect.positive[intersect.positive@data$ID %in% sample1,] #on sélectionne les mailles qui ont le même ID que sample1

#Seconde phase : on sélectionne au hasard 150 mailles au sein de la grille globale de la forêt de Citeaux (en plus des 50 sélectionnées précédemment), mais en retirant celles qui ont déjà été sélectionnées
intersect.positive=raster::intersect(grid.citeaux, bombina) #on créé un nouvel objet intersect.positive qui contient les mailles avec les dpoints de sonneur
grid.citeaux2=grid.citeaux[!grid.citeaux@data$ID_plot %in% intersect.positive@data$ID_plot,] #on retire les mailles avec les points de sonneur de la grille globale
sample2=sample(grid.citeaux2@data$ID_plot, 150, replace=F) #On sélectionne au hasard 150 cellules de cette grille
sample2=sort(sample2) #on classe le vecteur obtenu
sample2=grid.citeaux2[unique(grid.citeaux2@data$ID) %in%sample2,] #on sélectionne les cellules de la grille dont l'ID correspond aux nombres contenus dans sample2
colnames(sample2@data)[which(names(sample2@data)=="ID_plot")]="ID" #on change le nom de la colonne
sample2@data$area=NULL #on retire la colonne area
#Troisième phase : joindre les deux résultats
sample.citeaux=rbind(sample1, sample2) #on joint les résultats
sample.citeaux@data$ID=NULL #on retire la colonne ID
sample.citeaux@data$ID=c(1:nrow(sample.citeaux@data)) #on insère une nouvelle colonne ID qui contient une suite de chiffre séquentielle qui correspond au nombre de lignes de la table attributaire
sample.citeaux #on regarde à quoi ça ressemble

raster::shapefile(sample.citeaux,file="sample_citeaux.shp") #on enregistre la couche obtenue en couche shape

#Calculer le pourcentage de mailles qui possèdent au moins une donnée de sonneur au sein de l'échantillon
percentage=(nrow(sample1)*100)/nrow(sample.citeaux)
percentage
#25

#Générer les réplicats spatiaux, c'est-à-dire les sous-parcelles au sein des pacelles.
grid.replicates=raster(sample.citeaux) #bien s'assurer que sample.citeaux est en epsg:2154
res(grid.replicates)=100 #100 correspond à 100 m
grid.replicates=rasterToPolygons(grid.replicates) #génère une grille sur l'ensemble de la surface de l'aire d'étude
grid.replicates=raster::intersect(grid.replicates, sample.citeaux) #On réduit uniquement aux mailles de l'échantillon
grid.replicates$layer=NULL #on retire la colonne layer
grid.replicates$ID_plot=grid.replicates$ID #on créé une nouvelle colonne ID_plot qui correspond à la colonne ID
grid.replicates$ID_subplot=seq.int(nrow(grid.replicates)) #on attribue un ID de 1 à n à chaque carré. Ici comme la grille fait 100m de large et insérée dans une maille de 300 m de large, ça fait un total de 9. On doit donc obtenir une séquence de 1 à 9.
#raster::shapefile(grid.replicates, file="grid_subsamples_citeaux_globale.shp") #on enregistre la couche avec toutes les sous-mailles (pas obligatoire)
spac.rep=ddply(as.data.frame(grid.replicates),.(ID), function(x) x[sample(nrow(x),4),]) #ici on met 4 si on veut 4 réplicats spatiaux
spac.rep=grid.replicates[match(spac.rep$ID_subplot, grid.replicates@data$ID_subplot, nomatch=0),]
spac.rep@data$ID_rep=ave(spac.rep@data$ID_subplot, spac.rep@data$ID_plot, FUN=seq_along) #on ajoute un champ ID_rep qui numérote les réplicats spatiaux de 1 à 4 au sein des plots
spac.rep@data$layer=NULL #on retire la colonne layer
spac.rep@data$ID=NULL #on retire la colonne ID
spac.rep #on regarde à quoi ça ressemble
raster::shapefile(spac.rep, file="replicats_spatiaux_citeaux.shp") #on enregistre la couche au format shape


#############
#Sud Morvan
#############

#Première phase : on créé une sous-grille qui ne contient que des mailles positives, puis on sélectionne au hasard 90 mailles au sein de ce sous-ensemble
intersect.positive=raster::intersect(grid.sud.morvan3, bombina)
intersect.positive=unionSpatialPolygons(intersect.positive, intersect.positive@data$ID_plot)
id=sapply(slot(intersect.positive,"polygons"), function(x)slot(x,"ID"))
intersect.positive.df=data.frame(ID=1:length(intersect.positive),row.names=id)
intersect.positive=SpatialPolygonsDataFrame(intersect.positive, intersect.positive.df)
class(intersect.positive)
sum(gIsValid(intersect.positive, byid=T)==FALSE) #permet de vérifier que la géométrie est correcte, c'est-à-dire qu'il n'y a pas de polygones mal placés ou avec des erreurs, le résultat de cette formule doit être égal à zéro, sinon c'est qu'il y a un problème de géométrie
intersect.positive
# 103 polygones

sample1=sample(intersect.positive@data$ID, 80, replace=F)
sample1=sort(sample1) #c'est juste une suite de nombre
sample1=intersect.positive[intersect.positive@data$ID %in% sample1,] #on sélectionne les mailles qui ont le même ID que sample1

#Seconde phase : on sélectionne au hasard 120 mailles au sein de la grille globale du bocage sud Morvan (en plus des 80 sélectionnées précédemment), mais en retirant celles qui ont déjà été sélectionnées
intersect.positive=raster::intersect(grid.sud.morvan, bombina)
grid.sud.morvan2=grid.sud.morvan[!grid.sud.morvan@data$ID_plot %in% intersect.positive@data$ID_plot,]
sample2=sample(grid.sud.morvan2@data$ID_plot, 120, replace=F)
sample2=sort(sample2)
sample2=grid.sud.morvan2[unique(grid.sud.morvan2@data$ID) %in%sample2,]
colnames(sample2@data)[which(names(sample2@data)=="ID_plot")]="ID"
sample2@data$area=NULL

#Troisième phase : joindre les deux résultats
sample.sud.morvan=rbind(sample1, sample2)
sample.sud.morvan@data$ID=NULL
sample.sud.morvan@data$ID=c(1:nrow(sample.sud.morvan@data))
sample.sud.morvan

#Enregistrer la couche obtenue en couche shape
raster::shapefile(sample.sud.morvan,file="sample_sudMorvan.shp")

#Calculer le pourcentage de mailles qui possèdent au moins une donnée de sonneur au sein de l'échantillon
percentage=(nrow(sample1)*100)/nrow(sample.sud.morvan)
percentage
#40

#Générer les réplicats spatiaux
grid.replicates=raster(sample.sud.morvan)
res(grid.replicates)=100
grid.replicates=rasterToPolygons(grid.replicates)
grid.replicates=raster::intersect(grid.replicates, sample.sud.morvan)
grid.replicates$layer=NULL
grid.replicates$ID_plot=grid.replicates$ID
grid.replicates$ID_subplot=seq.int(nrow(grid.replicates))
raster::shapefile(grid.replicates, file="grid_subsamples_sud_morvan_globale.shp")
spac.rep=ddply(as.data.frame(grid.replicates),.(ID), function(x) x[sample(nrow(x),4),]) #ici on met 4 si on veut 4 réplicats spatiaux
spac.rep=grid.replicates[match(spac.rep$ID_subplot, grid.replicates@data$ID_subplot, nomatch=0),]
spac.rep@data$ID_rep=ave(spac.rep@data$ID_subplot, spac.rep@data$ID_plot, FUN=seq_along) #on ajoute un champ ID_rep qui numérote les réplicats spatiaux de 1 à 4 au sein des plots
spac.rep@data$layer=NULL
spac.rep@data$ID=NULL
spac.rep
raster::shapefile(spac.rep, file="replicats_spatiaux_sud_morvan.shp")


############
#Amognes
############

#Première phase : on créé une sous-grille qui ne contient que des mailles positives, puis on sélectionne au hasard 80 mailles au sein de ce sous-ensemble
intersect.positive=raster::intersect(grid.amognes, bombina)
intersect.positive=unionSpatialPolygons(intersect.positive, intersect.positive@data$ID_plot)
id=sapply(slot(intersect.positive,"polygons"), function(x)slot(x,"ID"))
intersect.positive.df=data.frame(ID=1:length(intersect.positive),row.names=id)
intersect.positive=SpatialPolygonsDataFrame(intersect.positive, intersect.positive.df)
class(intersect.positive)
sum(gIsValid(intersect.positive, byid=T)==FALSE) #permet de vérifier que la géométrie est correcte, c'est-à-dire qu'il n'y a pas de polygones mal placés ou avec des erreurs, le résultat de cette formule doit être égal à zéro, sinon c'est qu'il y a un problème de géométrie
intersect.positive
#91 éléments

sample1=sample(intersect.positive@data$ID, 91, replace=F)
sample1=sort(sample1) #c'est juste une suite de nombre
sample1=intersect.positive[intersect.positive@data$ID %in% sample1,] #on sélectionne les mailles qui ont le même ID que sample1

#Seconde phase : on sélectionne au hasard 110 mailles au sein de la grille globale du bocages des Amognes (en plus des 90 sélectionnées précédemment), mais en retirant celles qui ont déjà été sélectionnées
intersect.positive=raster::intersect(grid.amognes, bombina)
grid.amognes2=grid.amognes[!grid.amognes@data$ID_plot %in% intersect.positive@data$ID_plot,]
sample2=sample(grid.amognes2@data$ID_plot, 110, replace=F)
sample2=sort(sample2)
sample2=grid.amognes2[unique(grid.amognes2@data$ID_plot) %in% sample2,]
colnames(sample2@data)[which(names(sample2@data)=="ID_plot")]="ID"
sample2@data$area=NULL

#Troisième phase : joindre les deux résultats
sample.amognes=rbind(sample1, sample2)
sample.amognes@data$ID=NULL
sample.amognes@data$ID=c(1:nrow(sample.amognes@data))
sample.amognes

#Enregistrer la couche obtenue en couche shape
raster::shapefile(sample.amognes,file="sample_amognes.shp")

#Calculer le pourcentage de mailles qui possèdent au moins une donnée de sonneur au sein de l'échantillon
percentage=(nrow(sample1)*100)/nrow(sample.amognes)
percentage
#45.7

#Générer les réplicats spatiaux
grid.replicates=raster(sample.amognes)
res(grid.replicates)=100
grid.replicates=rasterToPolygons(grid.replicates)
grid.replicates=raster::intersect(grid.replicates, sample.amognes)
grid.replicates$layer=NULL
grid.replicates$ID_plot=grid.replicates$ID
grid.replicates$ID_subplot=seq.int(nrow(grid.replicates))
#raster::shapefile(grid.replicates, file="grid_subsamples_amognes_globale.shp")
spac.rep=ddply(as.data.frame(grid.replicates),.(ID), function(x) x[sample(nrow(x),4),]) #ici on met 4 si on veut 4 réplicats spatiaux
spac.rep=grid.replicates[match(spac.rep$ID_subplot, grid.replicates@data$ID_subplot, nomatch=0),]
spac.rep@data$ID_rep=ave(spac.rep@data$ID_subplot, spac.rep@data$ID_plot, FUN=seq_along) #on ajoute un champ ID_rep qui numérote les réplicats spatiaux de 1 à 4 au sein des plots
spac.rep@data$layer=NULL
spac.rep@data$ID=NULL
spac.rep
raster::shapefile(spac.rep, file="replicats_spatiaux_amognes.shp")


############
#Clunisois
############

#Première phase : on créé une sous-grille qui ne contient que des mailles positives, puis on sélectionne au hasard 100 mailles au sein de ce sous-ensemble
intersect.positive=raster::intersect(grid.clunisois, bombina)
intersect.positive=unionSpatialPolygons(intersect.positive, intersect.positive@data$ID_plot)
id=sapply(slot(intersect.positive,"polygons"), function(x)slot(x,"ID"))
intersect.positive.df=data.frame(ID=1:length(intersect.positive),row.names=id)
intersect.positive=SpatialPolygonsDataFrame(intersect.positive, intersect.positive.df)
class(intersect.positive)
sum(gIsValid(intersect.positive, byid=T)==FALSE) #permet de vérifier que la géométrie est correcte, c'est-à-dire qu'il n'y a pas de polygones mal placés ou avec des erreurs, le résultat de cette formule doit être égal à zéro, sinon c'est qu'il y a un problème de géométrie
intersect.positive

sample1=sample(intersect.positive@data$ID, 100, replace=F)
sample1=sort(sample1) #c'est juste une suite de nombre
sample1=intersect.positive[intersect.positive@data$ID %in% sample1,] #on sélectionne les mailles qui ont le même ID que sample1

#Seconde phase : on sélectionne au hasard 100 mailles au sein de la grille globale du bocages des Amognes (en plus des 90 sélectionnées précédemment), mais en retirant celles qui ont déjà été sélectionnées
intersect.positive=raster::intersect(grid.clunisois, bombina)
grid.clunisois2=grid.clunisois[!grid.clunisois@data$ID_plot %in% intersect.positive@data$ID_plot,]
sample2=sample(grid.clunisois2@data$ID_plot, 100, replace=F)
sample2=sort(sample2)
sample2= grid.clunisois2[unique(grid.clunisois2@data$ID) %in% sample2,]
colnames(sample2@data)[which(names(sample2@data)=="ID_plot")]="ID"
sample2@data$area=NULL

#Troisième phase : joindre les deux résultats
sample.clunisois=rbind(sample1, sample2)
sample.clunisois@data$ID=NULL
sample.clunisois@data$ID=c(1:nrow(sample.clunisois@data))
sample.clunisois

#Enregistrer la couche obtenue en couche shape
raster::shapefile(sample.clunisois,file="sample_clunisois.shp")

#Calculer le pourcentage de mailles qui possèdent au moins une donnée de sonneur au sein de l'échantillon
percentage=(nrow(sample1)*100)/nrow(sample.clunisois)
percentage
#50

#Générer les réplicats spatiaux
grid.replicates=raster(sample.clunisois)
res(grid.replicates)=100
grid.replicates=rasterToPolygons(grid.replicates)
grid.replicates=raster::intersect(grid.replicates, sample.clunisois)
grid.replicates$layer=NULL
grid.replicates$ID_plot=grid.replicates$ID
grid.replicates$ID_subplot=seq.int(nrow(grid.replicates))
#raster::shapefile(grid.replicates, file="grid_subsamples_clunisois_globale.shp")
spac.rep=ddply(as.data.frame(grid.replicates),.(ID), function(x) x[sample(nrow(x),4),]) #ici on met 4 si on veut 4 réplicats spatiaux
spac.rep=grid.replicates[match(spac.rep$ID_subplot, grid.replicates@data$ID_subplot, nomatch=0),]
spac.rep@data$ID_rep=ave(spac.rep@data$ID_subplot, spac.rep@data$ID_plot, FUN=seq_along) #on ajoute un champ ID_rep qui numérote les réplicats spatiaux de 1 à 4 au sein des plots
spac.rep@data$layer=NULL
spac.rep@data$ID=NULL
spac.rep
raster::shapefile(spac.rep, file="replicats_spatiaux_clunisois.shp")

