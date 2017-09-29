# Importer les données:
path<-"F:/MIASHS/TER/Vegeta/Anaelle/data/data_pour_model.csv"
donnees <- read.table(path,sep=",",dec=",",header=T)
head(donnees)
summary(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")

# Transforme tout en numerique
for (i in 4:ncol(donnees)){
  donnees[,i]<-as.numeric(as.character(donnees[,i]))}

# plot des correlations entre les variables
plot(donnees)
cor(donnees[,-c(1:3)],use="complete.obs")

# Modele complet:
mod<-lm(data=donnees,SAP_FLOW~NETRAD_1havant +PPFD_IN_1havant + RH +SW_IN_1havant +TA +FC_1havant +H_1havant+LE+VPD+SB+G)
summary(mod)

# moyenne des parametres pour chaque heure
for (i in unique(donnees$heure_solaire)){
  apply()
}

donnees_jour<-donnees[donnees$NETRAD_1havant>0,]
donnees_nuit<-donnees[donnees$NETRAD_1havant<0,]


# Modele jour:
mod_jour<-lm(data=donnees_jour,SAP_FLOW~NETRAD_1havant +PPFD_IN_1havant + RH +SW_IN_1havant +TA +FC_1havant +H_1havant+LE+VPD+SB+G)
summary(mod_jour)

# Modele nuit:
mod_nuit<-lm(data=donnees_nuit,SAP_FLOW~NETRAD_1havant +PPFD_IN_1havant + RH +SW_IN_1havant +TA +FC_1havant +H_1havant+LE+VPD+SB+G)
summary(mod_nuit)