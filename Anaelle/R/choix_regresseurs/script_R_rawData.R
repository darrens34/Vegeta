

# Importer les données:
path<-"F:/MIASHS/TER/Vegeta/Anaelle/data/EFDC_L2_Flx_FRHes_2010_v010_30m.txt"
donnees <- read.table(path,sep=",",dec=",",header=T,na.strings = "-9999")
head(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")

# Supprimer les données où -9999 dans sap flow
donnees <- donnees[-which(is.na(donnees$SAP_FLOW )),]

# Transforme tout en numerique
for (i in 1:ncol(donnees)){
  donnees[,i]<-as.numeric(as.character(donnees[,i]))}

# Colonne date normale:
library(solaR)
donnees$date<-strptime(donnees$TIMESTAMP_END,format="%Y%m%d%H%M")
donnees$heure_solaire<-local2Solar(donnees$date)

# Calcul du VPD:
es <- 0.6108 * exp((17.27 * donnees$TA) / (donnees$TA + 237.3))
ea <- donnees$RH/ 100 * es 
VPD <- es - ea # en kPa
donnees$VPD <-VPD
donnees$VPD<-as.numeric(as.character(donnees$VPD))

# enregistrer le CSV
write.csv(donnees,"data_ok.csv")

# SAP_flow en fonction des heures de la jorunée:
plot(x=donnees$heure,y=donnees$SAP_FLOW)
# entre 18h et 8h le sap flow diminue car c'est la nuit. On essaie de separer jour et nuit
donnees_jour<-donnees[donnees$NETRAD>0,]
donnees_nuit<-donnees[donnees$NETRAD<0,]
  
# PDF des correlations
pdf("plot_lm.pdf")
for (i in c(4:32,34,35)){
plot(y=donnees$SAP_FLOW, x=donnees[,i],ylab="Sap flow",xlab=colnames(donnees)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees$SAP_FLOW~donnees[,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                      paste("r2adj = ",round(mod$adj.r.squared,2 )))))
  }
dev.off()
# PDF des correlations jour
pdf("plot_lm_jour.pdf")
for (i in c(4:32,34,35)){
  plot(y=donnees_jour$SAP_FLOW, x=donnees_jour[,i],ylab="Sap flow",xlab=colnames(donnees_jour)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees_jour$SAP_FLOW~donnees_jour[,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
}
dev.off()
# PDF des correlations nuit
pdf("plot_lm_nuit.pdf")
for (i in c(4:32,34,35)){
  plot(y=donnees_nuit$SAP_FLOW, x=donnees_nuit[,i],ylab="Sap flow",xlab=colnames(donnees_nuit)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees_nuit$SAP_FLOW~donnees_nuit[,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
}
dev.off()
# Regresseurs retenus:
# NETRAD,PPFD_IN,PPFD_OUT,RH,SW_IN,SW_OUT,TA,LE,VPD

plot(x=donnees$date,y=donnees$SAP_FLOW,type="l")
