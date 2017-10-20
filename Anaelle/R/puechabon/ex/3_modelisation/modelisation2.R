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
cor(donnees[,-c(1:3)],use="complete.obs")

# Modele complet:
mod<-lm(data=donnees,SAP_FLOW~NETRAD_1havant +PPFD_IN_1havant + RH +SW_IN_1havant +TA +FC_1havant +H_1havant+LE+VPD+SB+G)
summary(mod)

# Test pour voir si on peut supprimer une variable (RH ou netrad)
donnees_jour<-donnees[donnees$NETRAD_1havant>0,]
donnees_nuit<-donnees[donnees$NETRAD_1havant<0,]
# Modele jour:
mod_jour<-lm(data=donnees_jour,SAP_FLOW~NETRAD_1havant +PPFD_IN_1havant + RH +SW_IN_1havant +TA +FC_1havant +H_1havant+LE+VPD+SB+G)
summary(mod_jour) # rh et netrad significatifs
# Modele nuit:
mod_nuit<-lm(data=donnees_nuit,SAP_FLOW~NETRAD_1havant +PPFD_IN_1havant + RH +SW_IN_1havant +TA +FC_1havant +H_1havant+LE+VPD+SB+G)
summary(mod_nuit) # rh significatif

# moyenne des parametres pour chaque heure pour faire une courbe de sap flow predite moyenne
var_heure<-NULL
for (i in levels(donnees$heure_solaire)){
  vec<-apply(donnees[donnees$heure_solaire==i,-c(1:4)],MARGIN=2,FUN=mean,na.rm=TRUE)
  var_heure<-cbind(var_heure,vec)
}
colnames(var_heure)<-levels(donnees$heure_solaire)
write.csv(var_heure,"X_par_heure.csv")

# plot pour une journée moyenne:
plot(predict.lm(mod,newdata=as.data.frame(t(var_heure)))~seq(0,23.50,0.50),ylab="Flux de sève predit",xlab="Heure solaire",type="l",col="darkgreen")

# equation du modèle global:
coef<-round(mod$coefficients,4)
equation_modele_global <- paste("Flux de sève = ",coef[1]," + ",coef[2]," NETRAD_1havant + ",coef[3]," PPFD_IN_1havant + ",
                                coef[4]," RH + ",coef[5],"SW_IN_1havant + ",coef[6]," TA + ",coef[7]," FC_1havant + ",
                                coef[8]," H_1havant + ",coef[9]," LE + ",coef[10]," VPD + ",coef[11]," SB + ",coef[12]," G")

