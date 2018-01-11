######### ######### #########
######### importer et transformer les données
######### ######### #########

############# Importer les données
path<-"F:/MIASHS/TER/Vegeta/data/data_train_test/Puechabon_train.csv" 
donnees_base <- read.table(path,sep=",",dec=".",header=T,na.strings = c("-9999","NA"))
head(donnees_base)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")
summary(donnees_base)

#############  Transforme les dates
donnees_base$dates = strptime(donnees_base$date,format = "%Y-%m-%d %H:%M:%S")
donnees_base$heure_solaire = strptime(donnees_base$heure_solaire,format = "%H:%M")

#############  Supprime colonnes inutiles pour le moment
donnees_base = donnees_base[,-1]# supprime premier colonne d'index
donnees<- donnees_base[,-which(names(donnees_base) %in% c("TIMESTAMP_START","TIMESTAMP_END","DTime","dates","heure_solaire"))]



######### ######### ######### 
######### Regarde les correlations entre variables
######### ######### ######### 
corDF = cor(donnees,use="complete.obs")
dissimilarity <- 1 - abs(corDF)
distance <- as.dist(dissimilarity)
hc <- hclust(distance)
clusterV = cutree(hc,h=0.05)
print(clusterV)

