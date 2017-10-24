########## Importer les librairies
library(leaps)
?regsubsets

######### ######### ######### 
######### importer et transformer les données
######### ######### ######### 

############# Importer les données
path<-"F:/MIASHS/TER/Vegeta/data/data_sans_NA/Boreas_1996.csv" 
donnees_base <- read.table(path,sep=",",dec=".",header=T,na.strings = c("-9999","NA"))
head(donnees_base)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")
summary(donnees_base)

donnees_base = donnees_base[,-1]# supprime premier colonne d'index

#############  Transforme les dates
donnees_base$DATE_OBS = strptime(donnees_base$DATE_OBS,format = "%Y-%m-%d %H:%M:%S")
donnees_base$heure_solaire = strptime(donnees_base$heure_solaire,format = "%H:%M")

#############  Supprime colonnes inutiles pour le moment
donnees<- donnees_base[,-which(names(donnees_base) %in% c("DATE_OBS","TIME_OBS","NUM_TREES","MEASUREMENT_DESCR","heure_solaire"))]

######### ######### ######### 
######### Regarde les correlations entre variables
######### ######### ######### 
core <- cor(donnees[,-c(1,2)],method="pearson",use = "pairwise.complete.obs")
write.csv(core,"correlations_entre_variables.csv")

#############  Supprime colonnes corrélées
donnees<- donnees[,-which(names(donnees) %in% c("MEAN_AIR_TEMP_1HR_25M","MEAN_DOWN_SHRTWAVE_RAD_1HR_18M"))]

#############  cONVERSION DES CAR EN NUMERIQUE
Populus<-(donnees$SPECIES=="Populus tremuloides")*1
PiceaG<-(donnees$SPECIES=="Picea glauca")*1
PiceaM<-(donnees$SPECIES=="Picea mariana")*1
donnees$Populus <-Populus
donnees$PiceaG <-PiceaG
donnees$PiceaM <-PiceaM

donnees<- donnees[,-which(names(donnees) %in% c("SPECIES","TREE_DESCR"))]

######### ######### ######### 
######### Recherche du meilleur modele
######### ######### ######### 
summary(donnees)

liste = c( "MEAN_TRANS_RATE", "MEAN_AIR_TEMP_1HR_150CM", "MEAN_REL_HUM_1HR_150CM" ,"MEAN_AIR_TEMP_3HR_240CM" ,"MEAN_REL_HUM_3HR_240CM",
           "MEAN_VAPOR_PRESS_1HR_25M", "MEAN_DOWN_SHRTWAVE_RAD_3HR_4M" ,"MEAN_WIND_VELOCITY_1HR_18M")


######### Selection du meilleur modele au temps t
mod1 = regsubsets(SAP_FLOW ~ MEAN_TRANS_RATE+ MEAN_AIR_TEMP_1HR_150CM +MEAN_REL_HUM_1HR_150CM +MEAN_AIR_TEMP_3HR_240CM +
                    MEAN_REL_HUM_3HR_240CM + MEAN_WIND_VELOCITY_1HR_18M, data = donnees[donnees$Populus=="1",],method = "exhaustive",nvmax=ncol(donnees)-1,nbest=1)

mod1 = regsubsets(SAP_FLOW ~ MEAN_TRANS_RATE+ MEAN_AIR_TEMP_1HR_150CM +MEAN_REL_HUM_1HR_150CM +MEAN_AIR_TEMP_3HR_240CM +
                    MEAN_REL_HUM_3HR_240CM+ MEAN_VAPOR_PRESS_1HR_25M +MEAN_DOWN_SHRTWAVE_RAD_3HR_4M + MEAN_WIND_VELOCITY_1HR_18M , data = donnees[donnees$Populus=="1",],method = "seqrep",nvmax=ncol(donnees)-1,nbest=1)

#### Il y a bcp trop de NA. On fera juste des graphes des correlations observées
