

# Importation des données:
path<-"F:/MIASHS/TER/Vegeta/Anaelle/data/EFDC_L2_Flx_FRHes_2010_v010_30m.txt"
donnees <- read.table(path,sep=",",dec=",",header=T,na.strings = "-9999")
head(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")

# Supprime les données où -9999 dans sap flow
donnees <- donnees[-which(is.na(donnees$SAP_FLOW )),]

# selectionner les heures:
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
donnees$heure <- substrRight(donnees$TIMESTAMP_END,4)

# Transforme tout en numerique
for (i in 4:34){
donnees[,i]<-as.numeric(as.character(donnees[,i]))}

# Calcul du VPD:
es <- 0.6108 * exp((17.27 * donnees$TA) / (donnees$TA + 237.3))
ea <- donnees$RH/ 100 * es 
VPD <- es - ea # en kPa
donnees$VPD <-VPD

# enregistrer le CSV
write.csv(donnees,"data_ok.csv")

# PDF des correlations
pdf("scatterplots.pdf")
for (i in c(4:32,34,35)){
plot(donnees$SAP_FLOW~donnees[,i],ylab="Sap flow",xlab=colnames(donnees)[i])}
dev.off()