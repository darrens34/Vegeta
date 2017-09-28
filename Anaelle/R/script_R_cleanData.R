

# Importer les données:
path<-"F:/MIASHS/TER/Vegeta/Anaelle/data/cleanData.csv"
donnees <- read.table(path,sep=",",dec=",",header=T,na.strings = "-9999")
head(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")


# Transforme tout en numerique
for (i in 4:ncol(donnees)){
  donnees[,i]<-as.numeric(as.character(donnees[,i]))}

# Colonne date normale:
library(solaR)
donnees$dates<-as.POSIXct(donnees$TIMESTAMP_END,format="%Y-%m-%d %H:%M:%S", tz='Europe/Paris')
donnees$heure_solaire<-local2Solar(donnees$dates)
donnees$heure_solaire<-strftime(donnees$heure_solaire,format="%H:%M")
donnees$heure_solaire<-as.POSIXct(donnees$heure_solaire,format="%H:%M", tz='Europe/Paris')

# Calcul du VPD:
es <- 0.6108 * exp((17.27 * donnees$TA) / (donnees$TA + 237.3))
ea <- donnees$RH/ 100 * es 
VPD <- es - ea # en kPa
donnees$VPD <-VPD
donnees$VPD<-as.numeric(as.character(donnees$VPD))

# enregistrer le CSV
write.csv(donnees,"data_clean_vpd.csv")

# SAP_flow en fonction des heures de la jorunée:
plot(x=donnees$heure,y=donnees$SAP_FLOW)
# entre 18h et 8h le sap flow diminue car c'est la nuit. On essaie de separer jour et nuit
donnees_jour<-donnees[donnees$NETRAD>0,]
donnees_nuit<-donnees[donnees$NETRAD<0,]
  
# PDF des correlations
pdf("plot_lm.pdf")
for (i in c(5:33,35:37)){
plot(y=donnees$SAP_FLOW, x=donnees[,i],ylab="Sap flow",xlab=colnames(donnees)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees$SAP_FLOW~donnees[,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                      paste("r2adj = ",round(mod$adj.r.squared,2 )))))
  }
dev.off()
# PDF des correlations jour
pdf("plot_lm_jour.pdf")
for (i in c(5:33,35:37)){
  plot(y=donnees_jour$SAP_FLOW, x=donnees_jour[,i],ylab="Sap flow",xlab=colnames(donnees_jour)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees_jour$SAP_FLOW~donnees_jour[,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
}
dev.off()
# PDF des correlations nuit
pdf("plot_lm_nuit.pdf")
for (i in c(5:33,35:37)){
  plot(y=donnees_nuit$SAP_FLOW, x=donnees_nuit[,i],ylab="Sap flow",xlab=colnames(donnees_nuit)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees_nuit$SAP_FLOW~donnees_nuit[,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
}
dev.off()
# Regresseurs retenus:
# NETRAD,PPFD_IN,PPFD_OUT,RH,SW_IN,SW_OUT,TA,LE,VPD

#################### ESSAI DELAI
pdf("plot_lm_delai_30min.pdf")
for (i in c(5:33,35:37)){
  j<-2:nrow(donnees)
  k<-1:(nrow(donnees)-1)
  plot(y=donnees$SAP_FLOW[j], x=donnees[k,i],ylab="Sap flow",xlab=colnames(donnees)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees$SAP_FLOW[j]~donnees[k,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
}
dev.off()

pdf("plot_lm_delai_1heure.pdf")
for (i in c(5:33,35:37)){
  j<-3:nrow(donnees)
  k<-1:(nrow(donnees)-2)
  plot(y=donnees$SAP_FLOW[j], x=donnees[k,i],ylab="Sap flow",xlab=colnames(donnees)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees$SAP_FLOW[j]~donnees[k,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
}
dev.off()

pdf("plot_lm_delai_2heure.pdf")
for (i in c(5:33,35:37)){
  j<-5:nrow(donnees)
  k<-1:(nrow(donnees)-4)
  plot(y=donnees$SAP_FLOW[j], x=donnees[k,i],ylab="Sap flow",xlab=colnames(donnees)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees$SAP_FLOW[j]~donnees[k,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
}
dev.off()

pdf("plot_lm_delai_3heure.pdf")
for (i in c(5:33,35:37)){
  j<-7:nrow(donnees)
  k<-1:(nrow(donnees)-6)
  plot(y=donnees$SAP_FLOW[j], x=donnees[k,i],ylab="Sap flow",xlab=colnames(donnees)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees$SAP_FLOW[j]~donnees[k,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
}
dev.off()