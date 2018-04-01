######### ######### #########
######### importer et transformer les données
######### ######### #########

############# Importer les données
path<-"F:/MIASHS/TER/Vegeta/data/data_train_test/Puechabon_train.csv" 
donnes_train<-read.table(path,sep=",",dec=".",header=T,na.strings = c("-9999","NA"))
donnes_train$type<-"train"

setwd("F:/MIASHS/TER/Vegeta/Anaelle")
summary(donnes_train)

donnes_train = donnes_train[,-which(colnames(donnes_train)=="X")]# supprime premier colonne d'index

#############  Transforme les dates
donnes_train$dates = strptime(donnes_train$date,format = "%Y-%m-%d %H:%M:%S")
donnes_train$heure_solaire = strptime(donnes_train$heure_solaire,format = "%H:%M")

#############  Supprime colonnes inutiles pour le moment
donnees<- donnes_train[,-which(names(donnes_train) %in% c("dates","heure_solaire","type"))]

############# Visualisation de la relation avec le VPD
plot(donnees$SAP_FLOW~donnees$VPD,xlab="VPD")


############# Recherche du point de rupture
# Point de rupture fixé à 1.25
donnees1<- donnes_train[donnes_train$VPD < 1.25,]
donnees1<- donnees1[-which(donnees1$SAP_FLOW <1 & donnees1$VPD >0.5),]
donnees1<- donnees1[-which(donnees1$SAP_FLOW <2 & donnees1$VPD >0.75),]
donnees1<- donnees1[-which(donnees1$SAP_FLOW <3 & donnees1$VPD >0.9),]
donnees1<- donnees1[-which(donnees1$SAP_FLOW <4 & donnees1$VPD >1),]
donnees1<- donnees1[-which(donnees1$SAP_FLOW <5 & donnees1$VPD >1.1),]
plot(donnees1$SAP_FLOW~donnees1$VPD,xlab="VPD")

donnees2<- donnes_train[donnes_train$VPD >= 1.25,]
donnees2<- donnees2[donnees2$SAP_FLOW >= 7,]
plot(donnees2$SAP_FLOW~donnees2$VPD,xlab="VPD")

# modele pour vpd < 1.25
mod1<-lm(SAP_FLOW ~  PPFD_IN_1h  + VPD , data = donnees1)
summary(mod1)

# modele pour vpd >= 1.25
mod2<-lm(SAP_FLOW ~  PPFD_IN_1h  + VPD , data = donnees2)
summary(mod2)

####################### PREDICTIONS
path<-"F:/MIASHS/TER/Vegeta/data/data_train_test/Puechabon_test.csv" 
donnes_test<-read.table(path,sep=",",dec=".",header=T,na.strings = c("-9999","NA"))
donnes_test= donnes_test[,-which(colnames(donnes_test)=="X")]# supprime premier colonne d'index

#############  Supprime colonnes inutiles pour le moment
donnes_test<- donnes_test[,-which(names(donnes_test) %in% c("dates","heure_solaire","type"))]

# prediction
donnes_test1<- donnes_test[donnes_test$VPD < 1.25,]
donnes_test2<- donnes_test[donnes_test$VPD >= 1.25,]

pred1 <- predict.lm(mod1,donnes_test1)
pred2 <- predict.lm(mod2,donnes_test2)
rmse <- sqrt(mean((c(donnes_test1$SAP_FLOW,donnes_test2$SAP_FLOW) - c(pred1,pred2))^2,na.rm=TRUE)) ; rmse
# 1.22077


# plot pour une journée moyenne:
path<-"F:/MIASHS/TER/Vegeta/data/data_pour_visu_nabil/Puechabon/par_heure/X_par_heure.csv"
X <- read.table(path,sep=",",dec=".",header=T,row.names = 1)
head(X)
X <- X[c("PPFD_IN_1h","VPD"),]

lesPred=c()
for(i in 1:ncol(X)){
  if(X["VPD",i]<1.25){
    lesPred=c(lesPred,predict.lm(mod1,newdata=as.data.frame(t(X))[i,]))
  }else{lesPred=c(lesPred,predict.lm(mod2,newdata=as.data.frame(t(X))[i,]))}
}
plot(lesPred~seq(0,23.50,0.50),ylab="Flux de sève predit",xlab="Heure solaire",type="l",col="darkgreen")
write.csv(lesPred,"y_predit_par_heure.csv")


