########## Importer les librairies
library(FactoMineR)
library(factoextra) # pour les graphes
library(tidyverse)
library(dplyr)
library(tidyr)

######### ######### ######### 
######### importer et transformer les données de base
######### ######### ######### 

############# Importer les données
path<-"F:/MIASHS/TER/Vegeta/data/data_sans_NA/Puechabon_2010_vpd.csv" 
donnees_base <- read.table(path,sep=",",dec=".",header=T,na.strings = c("-9999","NA"))
head(donnees_base)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")
summary(donnees_base)

donnees_base = donnees_base[,-1]# supprime premier colonne d'index
donnees_base = donnees_base[,-which(colnames(donnees_base)=="TS_2")]
donnees_base = donnees_base[,-which(colnames(donnees_base)=="TS_3")] # Enleve TS_2 et TS_3 car correles a TS

#############  Transforme les dates
donnees_base$dates = strptime(donnees_base$date,format = "%Y-%m-%d %H:%M:%S")
donnees_base$heure_solaire = strptime(donnees_base$heure_solaire,format = "%H:%M")

#############  Supprime colonnes inutiles pour le moment
donnees<- donnees_base[,-which(names(donnees_base) %in% c("TIMESTAMP_START","TIMESTAMP_END","DTime","dates","heure_solaire"))]
# On enlève PRI car on a pas de mesure la nuit! Pas possible de prédire avec les NA ensuite :(
donnees = donnees[,-which(colnames(donnees)=="PRI")]

############ ACP
res_pca<- PCA(donnees, scale.unit = TRUE, graph = F)

fviz_screeplot(res_pca)
res_pca$eig
fviz_pca_var(res_pca, axes=c(1,2), repel = TRUE)




######### ######### ######### 
######### importer et transformer les données modifiées
######### ######### ######### 

############# Importer les données
path<-"F:/MIASHS/TER/Vegeta/data/data_post_modelisation/Puechabon_2010_post_model.csv" 
donnees_base <- read.table(path,sep=",",dec=".",header=T,na.strings = c("-9999","NA"))
head(donnees_base)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")
summary(donnees_base)
donnees_base<-donnees_base[,-c(1,3,4)]

############ ACP
res_pca<- PCA(donnees_base, scale.unit = TRUE, graph = F,quanti.sup=1)

fviz_screeplot(res_pca)
res_pca$eig
fviz_pca_var(res_pca, axes=c(1,2), repel = TRUE)

res_pca$var$contrib
