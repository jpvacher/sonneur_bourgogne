#script rédigé par Jean-Pierre Vacher, Mai 2022#

#on appelle les packages####
library("here")
library("ggplot2")
library("gridExtra")

#Lecture des données####
data1=read.csv("tabscenarii_AN10_1000boot.csv", dec=",", sep=";") #lire la source de données
data1 #on regarde à quoi ça ressemble
summary(data1) #on regarde le summary pour ajuster les valeurs des bornes de l'axe des y

#Construction des graphiques####
p1=ggplot(data1[data1$col==0.01 & data1$ext==0.012,], aes(x=N, y=lambda2))+ #on appelle le graphique
	geom_point()+ #on plote les valeurs de lambda
	geom_errorbar(aes(ymin=l_low, ymax=l_high),width=0.4)+ #barres d'erreur
	ylim(0.9,1.05)+ #on fixe les limites de l'axe des y
	labs(x="Nombre de sites", y="Lambda", title="Col. = 0.01 ; Ext. = 0.012")+ #on ajoute les titres des axes et du graphique
	theme(plot.title = element_text(size=9), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8),axis.text.x=element_text(size=7),axis.text.y=element_text(size=7)) #on ajuste la taille des titres

p2=ggplot(data1[data1$col==0.01 & data1$ext==0.012,], aes(x=N, y=lambda2.1))+
	geom_point()+
	geom_errorbar(aes(ymin=l2_low, ymax=l2_high),width=0.4)+
	ylim(0.9,1.05)+
	labs(x="Nombre de sites", y="Lambda2", title=" ")+
	theme(plot.title = element_text(size=9), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8),axis.text.x=element_text(size=7),axis.text.y=element_text(size=7))

p3=ggplot(data1[data1$col==0.02 & data1$ext==0.012,], aes(x=N, y=lambda2))+
	geom_point()+
	geom_errorbar(aes(ymin=l_low, ymax=l_high),width=0.4)+
	ylim(0.9,1.05)+
	labs(x="Nombre de sites", y="Lambda", title="Col. = 0.02 ; Ext. = 0.012")+
	theme(plot.title = element_text(size=9), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8),axis.text.x=element_text(size=7),axis.text.y=element_text(size=7))

p4=ggplot(data1[data1$col==0.02 & data1$ext==0.012,], aes(x=N, y=lambda2.1))+
	geom_point()+
	geom_errorbar(aes(ymin=l2_low, ymax=l2_high),width=0.4)+
	ylim(0.9,1.05)+
	labs(x="Nombre de sites", y="Lambda2", title=" ")+
	theme(plot.title = element_text(size=9), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8),axis.text.x=element_text(size=7),axis.text.y=element_text(size=7))

p5=ggplot(data1[data1$col==0.01 & data1$ext==0.015,], aes(x=N, y=lambda2))+
	geom_point()+
	geom_errorbar(aes(ymin=l_low, ymax=l_high),width=0.4)+
	ylim(0.9,1.05)+
	labs(x="Nombre de sites", y="Lambda", title="Col. = 0.01 ; Ext. = 0.015")+
	theme(plot.title = element_text(size=9), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8),axis.text.x=element_text(size=7),axis.text.y=element_text(size=7))

p6=ggplot(data1[data1$col==0.01 & data1$ext==0.015,], aes(x=N, y=lambda2.1))+
	geom_point()+
	geom_errorbar(aes(ymin=l2_low, ymax=l2_high),width=0.4)+
	ylim(0.9,1.05)+
	labs(x="Nombre de sites", y="Lambda2", title=" ")+
	theme(plot.title = element_text(size=9), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8),axis.text.x=element_text(size=7),axis.text.y=element_text(size=7))

p7=ggplot(data1[data1$col==0.02 & data1$ext==0.015,], aes(x=N, y=lambda2))+
	geom_point()+
	geom_errorbar(aes(ymin=l_low, ymax=l_high),width=0.4)+
	ylim(0.9,1.05)+
	labs(x="Nombre de sites", y="Lambda", title="Col. = 0.02 ; Ext. = 0.015")+
	theme(plot.title = element_text(size=9), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8),axis.text.x=element_text(size=7),axis.text.y=element_text(size=7))

p8=ggplot(data1[data1$col==0.02 & data1$ext==0.015,], aes(x=N, y=lambda2.1))+
	geom_point()+
	geom_errorbar(aes(ymin=l2_low, ymax=l2_high),width=0.4)+
	ylim(0.9,1.05)+
	labs(x="Nombre de sites", y="Lambda2", title=" ")+
	theme(plot.title = element_text(size=9), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8),axis.text.x=element_text(size=7),axis.text.y=element_text(size=7))

#Enregistrement de la figure finale####
jpeg(file="figure2.jpg", height=17, width=17, units="cm", res=300) #on créé un fichier jpeg
grid.arrange(p1, p2, p3, p4,p5, p6, p7, p8, nrow=4, ncol=2) #on plotte les graphiques sur un seul panneau avec quatre lignes et deux colonnes
dev.off() #on enregistre le fichier jpeg