##############################################################
############ SINGLE-SEASON OCCUPANCY MODEL #################
##############################################################
library(here)
library(unmarked)
data<-read.table("data_2021.txt",h=T) # lecture du fichier format long
head(data) #pour voir à quoi ça ressemble
str(data) #regarder la classe des colonnes, et dans quelle colonne se trouvent les données

##### les fonctions utiles pour retransformer les paramètres estimés rapidement#######
#`expit` <-
#function(x){
#exp(x)/(1+exp(x))
#}

#`logit` <-
#function(x){
#log(x/(1-x))
#}
################

######## création du tableau avec les observations
datay<-matrix(NA,nrow=nrow(data)/length(unique(data$visite)),length(unique(data$visite))) #nrow = nombre d'échantillons (mailles), ncol = nombre d'occasion de détection, visite = nom de la colonne qui contient les identifiants des visites (1, 2, 3,… n)
compteur<-1
for (z in 1:as.numeric(length(unique(data$visite)))) #colonnes
{
for (i in 1:as.numeric(nrow(data)/length(unique(data$visite)))) #lignes
{
datay[i,z]<-data[compteur,2] #le 2 correspond au nombre de la colonne où se trouvent les données dans la table data. À ajuster selon la structure de la table data
compteur<-compteur+1
}
}

####### lecture des fichiers des covariables

covarsite=read.table("covar_site.txt",h=T) #covariables occupation, correspond à la table de covariables d'occurence, mesurées sur SIG (par exemple densité de chemins forestiers, densité de sources, distance à la lisière la plus proche, % de recouvrement par les prairies)
names(covarsite)

#lectures des covariables visite (détection), les variables mesurées lors des passages, exportées depuis le tableau Excel
temp=read.table("temp.txt",h=T) #température
templog=read.table("templog.txt",h=T) #log de la température
orn=read.table("orn.txt",h=T) #nombre d'ornières/points d'eau dans la maille
ornlog=read.table("ornlog.txt",h=T) #log du nombre d'ornières/points d'eau dans la maille

obscov<-list(temp = temp, orn = orn, templog=templog, ornlog=ornlog)# on créé une liste avec ces covariables de session
year<-matrix(data="2021",nrow=(nrow(datay)),ncol=2,byrow=TRUE) #rajouter l'année supplémentaire chaque nouvelle année

###########INTRA ANNEE

data1<-unmarkedFrameOccu(datay, siteCovs = covarsite, obsCovs=obscov) #création de la table lisible par unmarked
data1 #pour vérifier que la table est au bon format et que les données sont ok

#modele avec occupation et détection constantes, c'est-à-dire même probabilité d'être présent dans chacune des mares, et même probabilité de détection à chaque visite
m0<-occu(~1~1,data=data1)
summary(m0) 
ranef(m0)

#transformation des valeurs de l'estimate du modèle pour avoir les "vraies" valeurs de détection et d'occupation, en pourcentage
det1<-backTransform(m0,'det') #détection
det1
confint(det1)

occ1<-backTransform(m0,'state') #occupation
occ1
confint(occ1)

#on peut aussi faire avec la fonction inv.logit du package boot, seulement pour le modèle occupancy. 
#library(boot)
#occ=inv.logit(coef(m0))[[1]]
#det=inv.logit(coef(m0))[[2]]

###################################

#modele avec les variables d'occupation et de détection
m1=occu(~temp+meteo ~forest+roads, data1) #On met les variables de detection en premier, puis les variables d'occupation, puis les données
summary(m1)
#ranef(m1)

#transformation des valeurs 

det1<-backTransform(m1,'det') #détection
det1
confint(det1)

occ1<-backTransform(m1,'state') #occupation
occ1
confint(occ1)

######################################

#Plot de la prediction de l'effet sur les nouvelles données pour voir comment l'occupation varie avec la variable forets par exemple

#on créé une table qui contient les valeurs prédites et les intervales (min et max) pour deux covariables, forest et roads par exemple
predict_m1_roads=cbind(predict(m1,
	newdata=data.frame(roads=seq(min(covarsite$roads, 
	na.rm=T),
	max(covarsite$roads,
	na.rm=T),
	by=0.01),
	forest=mean(covarsite$forest)),
	type="state"),
	data.frame(roads=seq(min(covarsite$roads,
	na.rm=T),
	max(covarsite$roads,
	na.rm=T),
	by=0.01),
	forest=mean(covarsite$forest)))

#jpeg(file="proba_densite_route.jpg", width=17, height=17, units="cm", res=300) #si on veut sauvegarder le graphique comme une image au format jpg dans le dossier de travail
ggplot(data=predict_m1_roads, aes(x=roads, y=Predicted))+
	geom_ribbon(aes(ymin=lower,ymax=upper),fill="gray")+
	stat_smooth(method="loess", col="black", se=F)+
	labs(x="Densité de routes (données standardisées)",y="Probabilité d'occupation prédite")+
	theme_classic()
#dev.off()



