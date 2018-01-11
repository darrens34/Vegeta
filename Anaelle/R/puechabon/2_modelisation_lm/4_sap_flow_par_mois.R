# Importer les données:
path<-"F:/MIASHS/TER/Vegeta/data/data_post_modelisation/Puechabon_2010_post_model.csv"
donnees <- read.table(path,sep=",",dec=".",header=T)
head(donnees)
summary(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")

donnees = donnees[,-1]


# Transforme les dates
donnees$dates = strptime(donnees$date,format = "%Y-%m-%d %H:%M:%S")

# Rajout colonne mois:
donnees$mois = as.factor(as.character(strftime(strptime(donnees$dates,format = "%Y-%m-%d %H:%M:%S"),format = "%m")))

# Calcul des moyennes de sap flow par mois
data_month = aggregate(SAP_FLOW~ heure_solaire + mois,data = donnees, FUN="mean")
data_month$heure_solaire = strptime(data_month$heure_solaire,format = "%Y-%m-%d %H:%M:%S")


# plot
for(i in unique(data_month$mois)){
  plot(y=data_month$SAP_FLOW[data_month$mois == i], x=data_month$heure_solaire[data_month$mois == i],type = "l",
       xlab = "Heures",ylab="Sap Flow",main = i,ylim=c(0,10))
}

#save csv:
write.csv(data_month,"Puechabon_mean_per_month.csv")
