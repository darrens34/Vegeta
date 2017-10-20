

# Importer les données:
path<-"F:/MIASHS/TER/Vegeta/data/data_brut/Puechabon_2010.txt"
donnees <- read.table(path,sep=",",dec=",",header=T,na.strings = c("-9999"))
head(donnees)
setwd("F:/MIASHS/TER/Vegeta/data/data_brut/")

# Transforme tout en numerique
for (i in 1:ncol(donnees)){
  donnees[,i]<-as.numeric(as.character(donnees[,i]))}

# Colonne date normale:
library(solaR)
donnees$TIMESTAMP_END <- as.character(donnees$TIMESTAMP_END)
donnees$dates<-as.POSIXct(donnees$TIMESTAMP_END,format="%Y%m%d%H%M", tz='Europe/Paris',origin = "1960-01-01")
donnees$heure_solaire<-local2Solar(donnees$dates)
donnees$heure_solaire<-strftime(donnees$heure_solaire,format="%H:%M")
donnees$heure_solaire<-as.POSIXct(donnees$heure_solaire,format="%H:%M", tz='Europe/Paris')


# Calcul du VPD:

VPD = list()
for(i in 1:nrow(donnees)){
  if(is.na(donnees$TA[i])|is.na(donnees$RH[i])){VPD[i] = NA }
  else {
    es <- 0.6108 * exp((17.27 * donnees$TA[i]) / (donnees$TA[i] + 237.3))
    ea <- donnees$RH[i]/ 100 * es 
    VPD[i] <- es - ea} # en kPa
    }

VPD<-as.numeric(VPD)
donnees$VPD <-VPD

# enregistrer le CSV
write.csv(donnees,"Puechabon_2010_vpd.csv")
