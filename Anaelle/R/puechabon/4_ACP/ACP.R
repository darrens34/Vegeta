########## Importer les librairies
library(FactoMineR)
library(factoextra) # pour les graphes
library(tidyverse)
library(dplyr)
library(tidyr)
library(pls)

######### ######### ######### 
######### importer et transformer les données de base
######### ######### ######### 

############# Importer les données
path<-"F:/MIASHS/TER/Vegeta/data/data_train_test/Puechabon_train.csv" 
donnes_train<-read.table(path,sep=",",dec=".",header=T,na.strings = c("-9999","NA"))
donnes_train$type<-"train"
path<-"F:/MIASHS/TER/Vegeta/data/data_train_test/Puechabon_test.csv" 
donnes_test<-read.table(path,sep=",",dec=".",header=T,na.strings = c("-9999","NA"))
donnes_test$type<-"test"

donnees_base <-rbind(donnes_train,donnes_test)
head(donnees_base)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")
summary(donnees_base)
donnees_base = donnees_base[,-1]# supprime premier colonne d'index

#############  Transforme les dates
donnees_base$dates = strptime(donnees_base$date,format = "%Y-%m-%d %H:%M:%S")
donnees_base$heure_solaire = strptime(donnees_base$heure_solaire,format = "%H:%M")

#############  Supprime colonnes inutiles pour le moment
donnees<- donnees_base[,-which(names(donnees_base) %in% c("TIMESTAMP_START","TIMESTAMP_END","DTime","dates","heure_solaire"))]
summary(donnees)

############# Remplacement des valeurs manquantes par valeur moyenne
for(i in colnames(donnees)){
  if(is.numeric(donnees[,i])==TRUE){
    donnees[which(is.na(donnees[,i])),i] <- mean(donnees[,i],na.rm=TRUE)
  }
}
summary(donnees)

############ Mise en place des donnees test/train et Y
# convert to dataframe
train <- as.data.frame(donnees[which(donnees$type=="train"),])
test <- as.data.frame(donnees[which(donnees$type=="test"),])

############ ACP
res_pca<- PCA(train[,-which(names(train) %in% c("SAP_FLOW","type"))], scale.unit = TRUE, graph = F)

fviz_screeplot(res_pca)
res_pca$eig
fviz_pca_var(res_pca, axes=c(1,2), repel = TRUE)

############ PCR
mod_pcr <- pcr(SAP_FLOW ~ ., data = train[,-which(names(train) %in% c("type"))], scale = TRUE)
pred <- predict(mod_pcr, ncomp = 4, newdata = test[,-which(names(test) %in% c("SAP_FLOW","type"))]) %>% as.data.frame %>% pull()
rmse <- sqrt(mean((test$SAP_FLOW - pred)^2)) ; rmse
