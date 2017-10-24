path<-"F:/MIASHS/TER/Vegeta/data/data_sans_NA/Boreas_1996_OK.csv"
donnees <- read.table(path,sep=",",dec=".",header=T,na.strings = c("","NA"))
head(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle/")
summary(donnees)

donnees<- donnees[,-1]

# Transforme les dates
donnees$DATE_OBS = strptime(donnees$DATE_OBS,format = "%Y-%m-%d %H:%M:%S")
donnees$heure_solaire = strptime(donnees$heure_solaire,format = "%H:%M")


# SAP_flow en fonction des heures de la journée:
plot(x=donnees$heure_solaire,y=donnees$SAP_FLOW)


# PDF des correlations
R<-data.frame()
pdf("plot_lm.pdf")
for (i in c(7:16)){
  plot(y=donnees$SAP_FLOW, x=donnees[,i],ylab="Sap flow",xlab=colnames(donnees)[i])
  mod<-NULL
  try(mod<-summary(lm(donnees$SAP_FLOW~donnees[,i])))
  try(legend("topleft",c(paste("Y = ",round(mod$coefficients[2,1],2),"x + ",round(mod$coefficients[1,1],2)),
                         paste("r2adj = ",round(mod$adj.r.squared,2 )))))
  R<-rbind(R,round(mod$adj.r.squared,2 ))
}
dev.off()

