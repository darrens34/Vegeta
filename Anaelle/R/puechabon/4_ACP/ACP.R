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

############ ACP
res_pca<- PCA(train[,-which(names(train) %in% c("SAP_FLOW","type"))], scale.unit = TRUE, graph = F,ncp =3)

fviz_screeplot(res_pca)
res_pca$eig
fviz_pca_var(res_pca, axes=c(1,2), repel = TRUE)

train_proj <- res_pca$ind$coord
train_proj <- as.data.frame(train_proj)

# add target column
train_proj$SAP_FLOW <- train$SAP_FLOW


############ PCR
model_pcr <- lm(SAP_FLOW~Dim.1+Dim.2+Dim.3,data = train_proj)
# summary
summary(model_pcr)


# projeter données test 
# standardisation par echantillon train (on dit que variables distribuées pareil dans echantillons train et test)
test_standard <- t(apply(test[,-which(names(test) %in% c("SAP_FLOW","type"))], MARGIN = 1, FUN = function(x) { (x-res_pca$call$centre)/res_pca$call$ecart.type } ))
# projection (matrix multiplication between individuals and dimensions coordinates)
test_proj <- as.matrix(test_standard) %*% res_pca$svd$V
test_proj <- as.data.frame(test_proj) 
colnames(test_proj) <- colnames(train_proj)[-length(colnames(train_proj))]
test_proj$SAP_FLOW <- test$SAP_FLOW

# prediction
pred <- predict.lm(model_pcr,test_proj[,1:3])
rmse <- sqrt(mean((test$SAP_FLOW - pred)^2)) ; rmse # 0.955463




########### Enregistrer fichiers nécessaires pour la prédiction avec ACP
data_stand <- data.frame("centre"=res_pca$call$centre,"reduire"=res_pca$call$ecart.type)
rownames(data_stand)<-colnames(test[,-which(names(test) %in% c("SAP_FLOW","type"))])
write.csv(data_stand,"F:/MIASHS/TER/Vegeta/data/data_pour_visu_nabil/Puechabon/acp/data_stand.csv")
vec_propre <-res_pca$svd$V
rownames(vec_propre)<-colnames(test[,-which(names(test) %in% c("SAP_FLOW","type"))])
write.csv(vec_propre,"F:/MIASHS/TER/Vegeta/data/data_pour_visu_nabil/Puechabon/acp/vec_propre.csv")

