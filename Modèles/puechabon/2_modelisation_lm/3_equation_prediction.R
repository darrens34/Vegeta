# Importer les données:
path<-"F:/MIASHS/TER/Vegeta/data/data_post_modelisation/Puechabon_2010_post_model.csv"
donnees <- read.table(path,sep=",",dec=".",header=T)
head(donnees)
summary(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")

donnees = donnees[,-which(colnames(donnees)=="X")]

X = donnees[,-which(names(donnees) %in% c("SAP_FLOW"))]

# Modele complet:
mod<-lm(SAP_FLOW ~  PPFD_IN_1h + TA+ TS +  WD_1h30+CO2+
          FC_1h + LE_30m + SH_3h + ZL_3h + VPD , data = donnees)
summary(mod)


# plot pour une journée moyenne:
path<-"F:/MIASHS/TER/Vegeta/data/data_pour_visu_nabil/Puechabon/par_heure/X_par_heure.csv"
X <- read.table(path,sep=",",dec=".",header=T,row.names = 1)
head(X)
plot(predict.lm(mod,newdata=as.data.frame(t(X)))~seq(0,23.50,0.50),ylab="Flux de sève predit",xlab="Heure solaire",type="l",col="darkgreen")
write.csv(data.frame(predict.lm(mod,newdata=as.data.frame(t(X)))),"y_predit_par_heure.csv")


# equation du modèle global:
coef<-round(mod$coefficients,4)
equation_modele_global <- paste("Flux de sève = ",coef[1]," + ",coef[2]," PPFD_IN_1h + ",coef[3],"  TA + ",coef[4]," TS + ",
                                coef[5]," WD_1h30 + ",coef[6],"  CO2 + ",coef[7]," FC_1h + ",coef[8]," LE_30m + ",
                                coef[9]," SH_3h + ",coef[10]," ZL_3H + ",coef[11]," VPD")

