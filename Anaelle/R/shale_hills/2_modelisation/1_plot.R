path<-"F:/MIASHS/TER/Vegeta/data/data_sans_NA/Shale_Hills.csv" 
donnees <- read.table(path,sep=",",dec=".",header=T,na.strings = c("-9999","-6999","NA"))
head(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")
summary(donnees)

donnees = donnees[,-1]# supprime premier colonne d'index
donnees$espece[which(donnees$espece=="QUPR2")]<-"QUPR"

# Transforme les dates
donnees$date = strptime(donnees$date,format = "%d/%m/%Y %H:%M")
donnees$heure_solaire = strptime(donnees$heure_solaire,format = "%H:%M")
donnees$heure_solaire =as.factor(as.character(donnees$heure_solaire))

# Récupère les mois
donnees$mois = as.factor(as.character(strftime(donnees$date,format = "%m")))

# Calcul des moyennes de sap flow par mois
data_month = aggregate(sap_flow~ heure_solaire +orientation+espece+ mois,data = donnees, FUN="mean")

# PDF des orientations
pdf("plot_orientation.pdf")
les_couleurs = c("green","blue","purple","orange","red")
for (i in levels(data_month$espece)){
  for(j in unique(data_month$orientation[data_month$espece==i])){
    compt = 0
    for(k in unique(data_month$mois[data_month$espece==i & data_month$orientation==j])){
      compt = compt + 1
      if(compt == 1){
        plot(x=as.numeric(data_month$heure_solaire[data_month$espece==i & data_month$orientation==j & data_month$mois==k]),y=data_month$sap_flow[data_month$espece==i & data_month$orientation==j & data_month$mois==k],
           ylim=c(0,10),xlab="heure",ylab="Sap flow",main=paste("espece ",i," orientation ",j),type="l",col=les_couleurs[compt])}
      else
      {points(x=as.numeric(data_month$heure_solaire[data_month$espece==i & data_month$orientation==j & data_month$mois==k]),y=data_month$sap_flow[data_month$espece==i & data_month$orientation==j & data_month$mois==k],
          type="l",col=les_couleurs[compt])}
    }
    legend("bottomright",text.col=les_couleurs,legend=unique(data_month$mois[data_month$espece==i & data_month$orientation==j]))
}}
dev.off()

write.table(data_month,"Shale_hills_per_month.csv",sep=",")
