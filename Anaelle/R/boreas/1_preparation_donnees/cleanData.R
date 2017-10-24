path<-"F:/MIASHS/TER/Vegeta/data/data_sans_NA/Boreas_1996.csv"
donnees <- read.table(path,sep=",",dec=".",header=T,na.strings = c("","-999.00"))
head(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle/")
summary(donnees)

# Transforme les dates
donnees$DATE_OBS = strptime(paste(donnees$DATE_OBS,donnees$TIME_OBS),format = "%Y-%m-%d %H")

# Colonne heure solaire:
donnees$heure_solaire = as.POSIXct(donnees$DATE_OBS, tz='Canada/Saskatchewan')
#attr(donnees$heure_solaire, "tzone") <- "UTC" 
#donnees$heure_solaire<-strftime(donnees$heure_solaire,format="%H:%M")
library(solaR)
donnees$heure_solaire = local2Solar(donnees$heure_solaire)
donnees$heure_solaire<-strftime(donnees$heure_solaire,format="%H:%M")

# transforme en numerique
for(i in c(7:17)){
  donnees[,i] = as.numeric(as.character(donnees[,i] ))
}
summary(donnees)

# outlier pour MEAN_AIR_TEMP_1HR_25M
donnees$MEAN_AIR_TEMP_1HR_25M[which(as.numeric(donnees$MEAN_AIR_TEMP_1HR_25M) >100)]<-NA
donnees$MEAN_DOWN_SHRTWAVE_RAD_3HR_4M[which(as.numeric(donnees$MEAN_DOWN_SHRTWAVE_RAD_3HR_4M) < - 500)]<-NA

# enregistrer le CSV
write.csv(donnees,"Boreas_1996_OK.csv")

