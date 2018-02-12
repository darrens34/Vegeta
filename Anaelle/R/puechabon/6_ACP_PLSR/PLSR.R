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
donnees_base$dates = strptime(donnees_base$dates,format = "%Y-%m-%d %H:%M:%S")
donnees_base$heure_solaire = strptime(donnees_base$heure_solaire,format = "%H:%M")


#############  Supprime colonnes inutiles pour le moment
donnees<- donnees_base[,-which(names(donnees_base) %in% c("dates","heure_solaire"))]
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

## 1
plsr_fit <- plsr(SAP_FLOW ~ ., data = train[,-which(colnames(train)=="type")], scale = TRUE )
summary(plsr_fit)
# À partir de combien de composante(s) votre modèle peut expliquer plus de 75% de la variance ? # 6

## 2
# evolution RMSEP
plot(RMSEP(plsr_fit), legendpos = "topright") # 4 composantes

## 4
# Selectionner le nombre de composantes
plsr_fit <- plsr(SAP_FLOW ~ ., data = train[,-which(colnames(train)=="type")], scale = TRUE ,validation = "CV")
selectNcomp(plsr_fit,method = c("onesigma"), ncomp = plsr_fit$ncomp,plot=TRUE) # 14

## 5
# prediction jeu de données test
# Predict
plsr_fit <- plsr(SAP_FLOW ~ ., data = train[,-which(colnames(train)=="type")], scale = TRUE ,ncomp = 14 )
pred_plsr_fit <- predict(plsr_fit, test[,-which(colnames(train)=="type")],ncomp = 14 )
# RMSE
rmse <- round(sqrt(mean((test$SAP_FLOW-pred_plsr_fit)^2)),2)
rmse #  0.81
