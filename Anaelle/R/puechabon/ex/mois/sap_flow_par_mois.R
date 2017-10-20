# Importer les données:
path<-"F:/MIASHS/TER/Vegeta/Anaelle/data/data_pour_model.csv"
donnees <- read.table(path,sep=",",dec=",",header=T)
head(donnees)
summary(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")

# Transforme tout en numerique
for (i in 4:ncol(donnees)){
  donnees[,i]<-as.numeric(as.character(donnees[,i]))}

# Rajout colonne mois:
donnees$mois = as.factor(as.character(strftime(strptime(donnees$dates,format = "%Y-%M-%d"),format = "%M")))

# Calcul des moyennes de sap flow par mois
data_month = aggregate(SAP_FLOW~mois + heure_solaire,data = donnees, FUN="mean")
data_month$heure_solaire = strptime(data_month$heure_solaire,format = "%H:%M:%S")

# plot
for(i in unique(data_month$mois)){
  plot(y=data_month$SAP_FLOW[data_month$mois == i], x=data_month$heure_solaire[data_month$mois == i],type = "l",
       xlab = "Heures",ylab="Sap Flow",main = i,ylim=c(0,10))
}