# Importer les données:
path<-"F:/MIASHS/TER/Vegeta/Anaelle/data/data_pour_model.csv"
donnees <- read.table(path,sep=",",dec=",",header=T)
head(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")

# Modele complet:
mod<-lm(data=donnees,SAP_FLOW~NETRAD_1havant +PPFD_IN_1havant + RH +SW_IN_1havant +TA +FC_1havant +H_1havant+LE+VPD+SB+G)
