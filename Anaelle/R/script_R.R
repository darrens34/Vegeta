

# Importation des données:
path<-"F:/MIASHS/TER/data/EFDC_L2_Flx_FRHes_2010_v010_30m.txt"
donnees <- read.table(path,sep=",",dec=",",header=T)
head(donnees)

# Supprime les données où -9999 dans sap flow
donnees <- donnees[-which(donnees$SAP_FLOW =="-9999"),]

# Transforme tout en numerique
for (i in 4:33){
donnees[,i]<-as.numeric(as.character(donnees[,i]))}

# Calcul du VPD:
donnees$RH<-as.numeric(as.character(donnees$RH));donnees$TA<-as.numeric(as.character(donnees$TA))
VPD <- ((100-donnees$RH)/100) * (610.7 * 10^((7.5*donnees$TA)/(237.3+donnees$TA))) # VPD en Pa
VPD <- VPD * 10^-3 #VPD en kPa

# PDF des correlations
pdf("scatterplots.pdf")
for (i in 4:32){
plot(donnees[,33]~donnees[,i])}
dev.off()