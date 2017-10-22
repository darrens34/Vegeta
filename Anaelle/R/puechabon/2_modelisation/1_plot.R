path<-"F:/MIASHS/TER/Vegeta/data/data_sans_NA/Puechabon_2010_vpd.csv" 
donnees <- read.table(path,sep=",",dec=".",header=T,na.strings = c("-9999","NA"))
head(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")
summary(donnees)

donnees = donnees[,-1]# supprime premier colonne d'index

# Transforme les dates
donnees$dates = strptime(donnees$date,format = "%Y-%m-%d %H:%M:%S")
donnees$heure_solaire = strptime(donnees$heure_solaire,format = "%Y-%m-%d %H:%M:%S")


# SAP_flow en fonction des heures de la jorunée:
plot(x=donnees$heure_solaire,y=donnees$SAP_FLOW)
# entre 18h et 8h le sap flow diminue car c'est la nuit. On essaie de separer jour et nuit
donnees_jour<-donnees[donnees$NETRAD>0,]
donnees_nuit<-donnees[donnees$NETRAD<0,]


# PDF des correlations
R<-data.frame()
pdf("plot_lm.pdf")
for (i in c(4:32,36:ncol(donnees))){
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
for (i in c(4:32,36:ncol(donnees))){
  plot(y=donnees_jour$SAP_FLOW, x=donnees_jour[,i],ylab="Sap flow",xlab=colnames(donnees_jour)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees_jour$SAP_FLOW~donnees_jour[,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
}
dev.off()
# PDF des correlations nuit
pdf("plot_lm_nuit.pdf")
for (i in c(4:32,36:ncol(donnees))){
  plot(y=donnees_nuit$SAP_FLOW, x=donnees_nuit[,i],ylab="Sap flow",xlab=colnames(donnees_nuit)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees_nuit$SAP_FLOW~donnees_nuit[,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
}
dev.off()
