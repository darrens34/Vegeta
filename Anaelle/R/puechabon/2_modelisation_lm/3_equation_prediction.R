# Importer les données:
path<-"F:/MIASHS/TER/Vegeta/data/data_post_modelisation/Puechabon_2010_post_model.csv"
donnees <- read.table(path,sep=",",dec=".",header=T)
head(donnees)
summary(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")

donnees = donnees[,-1]

X = donnees[,-which(names(donnees) %in% c("SAP_FLOW","dates","heure_solaire"))]

# Modele complet:
mod<-lm(SAP_FLOW ~ PPFD_IN_1h  +TS+ WD_2h+ CO2 +FC_1h+H2O_3h + SB+ SH_3h+SLE_3h+ ZL_3h + VPD, data = donnees)
summary(mod)

# moyenne des parametres pour chaque heure pour faire une courbe de sap flow predite moyenne
var_heure<-NULL
for (i in levels(donnees$heure_solaire)){
  vec<-apply(X[donnees$heure_solaire==i,],MARGIN=2,FUN=mean,na.rm=TRUE)
  var_heure<-cbind(var_heure,vec)
}
colnames(var_heure)<-levels(donnees$heure_solaire)
#write.csv(var_heure,"X_par_heure.csv")

# plot pour une journée moyenne:
plot(predict.lm(mod,newdata=as.data.frame(t(var_heure)))~seq(0,23.50,0.50),ylab="Flux de sève predit",xlab="Heure solaire",type="l",col="darkgreen")
#write.csv(data.frame(predict.lm(mod,newdata=as.data.frame(t(var_heure)))),"y_predit_par_heure.csv")


# equation du modèle global:
coef<-round(mod$coefficients,4)
equation_modele_global <- paste("Flux de sève = ",coef[1]," + ",coef[2]," PPFD_IN_1h + ",coef[3]," TS + ",
                                coef[4]," WD_2h + ",coef[5],"CO2 + ",coef[6]," FC_1h + ",coef[7]," H2O_3h + ",
                                coef[8]," SB + ",coef[9]," SH_3h + ",coef[10]," SLE_3h + ",coef[11]," ZL_3h + ",coef[12]," VPD")

