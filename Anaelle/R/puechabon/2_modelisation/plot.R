path<-"F:/MIASHS/TER/Vegeta/data/data_sans_NA/Puechabon_2010_vpd.csv" # Fichier de Darren
donnees <- read.table(path,sep=",",dec=",",header=T,na.strings = c("-9999","NA"))
head(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")
summary(donnees)

# Transforme tout en numerique
for (i in 1:33){
  donnees[,i]<-as.numeric(as.character(donnees[,i]))}
donnees$VPD = as.numeric(as.character(donnees$VPD))

donnees$dates = strptime(donnees$date,format = "%Y-%m-%d %H:%M:%S")
donnees$dates = strptime(donnees$date,format = "%Y-%m-%d %H:%M:%S")

summary(donnees)

# SAP_flow en fonction des heures de la jorunée:
plot(x=donnees$heure_solaire,y=donnees$SAP_FLOW)
# entre 18h et 8h le sap flow diminue car c'est la nuit. On essaie de separer jour et nuit
donnees_jour<-donnees[donnees$NETRAD>0,]
donnees_nuit<-donnees[donnees$NETRAD<0,]


# PDF des correlations
R<-data.frame()
pdf("plot_lm.pdf")
for (i in c(6:34,36:ncol(donnees))){
  plot(y=donnees$SAP_FLOW, x=donnees[,i],ylab="Sap flow",xlab=colnames(donnees)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees$SAP_FLOW~donnees[,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
  R<-rbind(R,round(mod$adj.r.squared,2 ))
}
dev.off()

# PDF des correlations jour
pdf("plot_lm_jour.pdf")
for (i in c(6:34,36:ncol(donnees))){
  plot(y=donnees_jour$SAP_FLOW, x=donnees_jour[,i],ylab="Sap flow",xlab=colnames(donnees_jour)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees_jour$SAP_FLOW~donnees_jour[,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
}
dev.off()
# PDF des correlations nuit
pdf("plot_lm_nuit.pdf")
for (i in c(6:34,36:ncol(donnees))){
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
R1<-data.frame()
pdf("plot_lm_delai_30min.pdf")
for (i in c(6:34,36:ncol(donnees))){
  j<-2:nrow(donnees)
  k<-1:(nrow(donnees)-1)
  plot(y=donnees$SAP_FLOW[j], x=donnees[k,i],ylab="Sap flow",xlab=colnames(donnees)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees$SAP_FLOW[j]~donnees[k,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
  R1<-rbind(R1,round(mod$adj.r.squared,2 ))
}
dev.off()

R2<-data.frame()
pdf("plot_lm_delai_1heure.pdf")
for (i in c(6:34,36:ncol(donnees))){
  j<-3:nrow(donnees)
  k<-1:(nrow(donnees)-2)
  plot(y=donnees$SAP_FLOW[j], x=donnees[k,i],ylab="Sap flow",xlab=colnames(donnees)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees$SAP_FLOW[j]~donnees[k,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
  R2<-rbind(R2,round(mod$adj.r.squared,2 ))
}
dev.off()

R3<-data.frame()
pdf("plot_lm_delai_2heure.pdf")
for (i in c(6:34,36:ncol(donnees))){
  j<-5:nrow(donnees)
  k<-1:(nrow(donnees)-4)
  plot(y=donnees$SAP_FLOW[j], x=donnees[k,i],ylab="Sap flow",xlab=colnames(donnees)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees$SAP_FLOW[j]~donnees[k,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
  R3<-rbind(R3,round(mod$adj.r.squared,2 ))
}
dev.off()

R4<-data.frame()
pdf("plot_lm_delai_3heure.pdf")
for (i in c(6:34,36:ncol(donnees))){
  j<-7:nrow(donnees)
  k<-1:(nrow(donnees)-6)
  plot(y=donnees$SAP_FLOW[j], x=donnees[k,i],ylab="Sap flow",xlab=colnames(donnees)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees$SAP_FLOW[j]~donnees[k,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
  R4<-rbind(R4,round(mod$adj.r.squared,2 ))
}
dev.off()