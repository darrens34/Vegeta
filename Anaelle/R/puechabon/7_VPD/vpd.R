######### ######### #########
######### importer et transformer les donn�es
######### ######### #########

############# Importer les donn�es
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


############# Nettoyage des donnees : 
donnees1<- donnes_train[donnes_train$VPD < 1,]
donnees1<- donnees1[-which(donnees1$SAP_FLOW <1 & donnees1$VPD >0.5),]
donnees1<- donnees1[-which(donnees1$SAP_FLOW <2 & donnees1$VPD >0.75),]
donnees1<- donnees1[-which(donnees1$SAP_FLOW <3 & donnees1$VPD >0.9),]
plot(donnees1$SAP_FLOW~donnees1$VPD,xlab="VPD")

donnees2<- donnes_train[donnes_train$VPD >= 1,]
donnees2<- donnees2[donnees2$SAP_FLOW >= 7,]
plot(donnees2$SAP_FLOW~donnees2$VPD,xlab="VPD")

# donnees combi : 
donneesOK = rbind(donnees1,donnees2)
plot(donneesOK$SAP_FLOW~donneesOK$VPD,xlab="VPD")


######### ######### #########
######### Relation sigmoidale
######### ######### #########
# Asym : asymptote
# xmid : Point d'inflection : 50% de Y
# scal : scale parameter
compt=0
xmid=0
scal=0
flogis <- function(x,  xmid, scal)9/(1+exp((xmid-x)/scal))

for (i in c("VPD")){
  compt=compt+1
  #Ajust
  y<-donneesOK$SAP_FLOW
  x<-donneesOK[,which(names(donneesOK)==i)]
  
  # Plot obs:
  plot(y~x,xlab=i,ylab="sap flow")
  
  fit0<-NULL
  fit0<-nls(y ~ flogis(x, xmid, scal), data = data.frame(x, y),control=nls.control(maxiter = 100000),start=list( xmid=10, scal=3))
  xmid[compt]<-summary(fit0)$coef[1]
  scal[compt]<-summary(fit0)$coef[2]
  xplot<-min(x,na.rm=T):max(x,na.rm=T)
  points(9.5 / ( 1 + exp ((xmid[compt]-xplot) / scal[compt])) ~ xplot,type="l",col="red",lwd=4)
  
  trans <- 9.5/ ( 1 + exp ((xmid[compt]- x) / scal[compt]))
  plot(donneesOK$SAP_FLOW~trans,xlab =i)
}


#Ajust
i="VPD"
y<-donneesOK$SAP_FLOW
x<-donneesOK[,which(names(donneesOK)==i)]
plot(y~x,xlab=i,ylab="sap flow")
xplot<-min(x,na.rm=T):max(x,na.rm=T)
points(8.5 / ( 1 + exp ((0.7598-xplot) / 0.1)) ~ xplot,type="l",col="red",lwd=4)

trans <- 8.5 / ( 1 + exp ((0.7598-x) / 0.1))
trans[which(trans=="Inf")]<-0
plot(donneesOK$SAP_FLOW~trans,xlab =i)
donneesOK$"VPD_trans"<-trans

mod = lm(SAP_FLOW ~ PPFD_IN_1h + VPD_trans, data = donneesOK)
summary(lm(SAP_FLOW ~ PPFD_IN_1h + VPD_trans, data = donneesOK))


####################### PREDICTIONS
path<-"F:/MIASHS/TER/Vegeta/data/data_train_test/Puechabon_test.csv" 
donnes_test<-read.table(path,sep=",",dec=".",header=T,na.strings = c("-9999","NA"))
donnes_test= donnes_test[,-which(colnames(donnes_test)=="X")]# supprime premier colonne d'index

#############  Supprime colonnes inutiles pour le moment
donnes_test<- donnes_test[,-which(names(donnes_test) %in% c("dates","heure_solaire","type"))]
VPD_trans = 8.5 / ( 1 + exp ((0.7598-donnes_test[,which(colnames(donnes_test)=="VPD")]) / 0.1))

# prediction
pred <- predict.lm(mod,donnes_test)

rmse <- sqrt(mean((donnes_test$SAP_FLOW - pred)^2,na.rm=TRUE)) ; rmse
# 1.066


# plot pour une journ�e moyenne:
path<-"F:/MIASHS/TER/Vegeta/data/data_pour_visu_nabil/Puechabon/par_heure/X_par_heure.csv"
X <- read.table(path,sep=",",dec=".",header=T,row.names = 1)
head(X)
X <- X[c("PPFD_IN_1h","VPD"),]
X["VPD_trans",] = 8.5 / ( 1 + exp ((0.7598-X["VPD",]) / 0.1))

lesPred=c()
for(i in 1:ncol(X)){
    lesPred=c(lesPred,predict.lm(mod,newdata=as.data.frame(t(X))[i,]))
}

plot(lesPred~seq(0,23.50,0.50),ylab="Flux de s�ve predit",xlab="Heure solaire",type="l",col="darkgreen")
write.csv(lesPred,"y_predit_par_heure.csv")


