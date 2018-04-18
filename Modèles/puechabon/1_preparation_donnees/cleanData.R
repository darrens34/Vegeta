

# Importer les données:
path<-"F:/MIASHS/TER/Vegeta/data/data_brut/Puechabon_2010_vpd.csv" 
donnees <- read.table(path,sep=",",dec=",",header=T,na.strings = c("-9999","NA"))
head(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")

# Supprime qd NA pour sap flow
donnees = donnees[-which(is.na(donnees$SAP_FLOW)),]
donnees = donnees[,-1]# supprime premier colonne d'index

# Transforme tout en numerique
for (i in 1:33){
  donnees[,i]<-as.numeric(as.character(donnees[,i]))}
donnees$VPD = as.numeric(as.character(donnees$VPD))

summary(donnees)

# enregistrer le CSV
write.csv(donnees,"Puechabon_2010_vpd.csv")
