# Importer les données:
path<-"F:/MIASHS/TER/Vegeta/data/data_brut/Puechabon_2010_vpd.csv" 
donnees <- read.table(path,sep=",",dec=",",header=T,na.strings = c("-9999","NA"))
head(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")

# Nettoyage valeurs bizarre de PA
donnees<-donnees[-which(as.numeric(donnees$PA) < 96),]

# Enleve lignes ou NA dans sap flow
donnees<-donnees[-which(is.na(donnees$SAP_FLOW)),]

# supprime colonnes inutiles
donnees = donnees[,-1]# supprime premier colonne d'index

# Pas PRI la nuit : On supprime la colonne
donnees = donnees[,-which(colnames(donnees)=="PRI")]
summary(donnees)

# Separer train et test au hasard
sample <- sample.int(n = nrow(donnees), size = round(nrow(donnees)*70/100,0), replace = F)
train <- donnees[sample, ]
test  <- donnees[-sample, ]

# Sauvegarde des données : 
write.csv(train,"F:/MIASHS/TER/Vegeta/data/data_train_test/Puechabon_train.csv")
write.csv(test,"F:/MIASHS/TER/Vegeta/data/data_train_test/Puechabon_test.csv")

