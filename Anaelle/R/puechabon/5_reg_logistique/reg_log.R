######### ######### #########
######### importer et transformer les données
######### ######### #########

############# Importer les données
path<-"F:/MIASHS/TER/Vegeta/data/data_train_test/Puechabon_train.csv" 
donnes_train<-read.table(path,sep=",",dec=".",header=T,na.strings = c("-9999","NA"))
donnes_train$type<-"train"
path<-"F:/MIASHS/TER/Vegeta/data/data_train_test/Puechabon_test.csv" 
donnes_test<-read.table(path,sep=",",dec=".",header=T,na.strings = c("-9999","NA"))
donnes_test$type<-"test"
setwd("F:/MIASHS/TER/Vegeta/Anaelle")
summary(donnes_train)

donnes_train = donnes_train[,-which(colnames(donnes_train)=="X")]# supprime premier colonne d'index

#############  Transforme les dates
donnes_train$dates = strptime(donnes_train$date,format = "%Y-%m-%d %H:%M:%S")
donnes_train$heure_solaire = strptime(donnes_train$heure_solaire,format = "%H:%M")

#############  Supprime colonnes inutiles pour le moment
donnees<- donnes_train[,-which(names(donnes_train) %in% c("dates","heure_solaire","type"))]

############# Visualisation des relations avec le sap flow TODO!
for (i in names(donnees)){
  plot(donnees$SAP_FLOW~donnees[,which(names(donnees)==i)],xlab=i)
}

"""
Relation lineaire avec le sap flow pour : 
NETRAD, PPFD_IN, PPFD_OUT, RH, SW_IN, SW_OUT

Relation sigmoidale pour : 
TA, H, LE, SB, G, VPD

Genre de LOG inverse:
CO2, FC, TAU, USTAR, WS
"""

######### ######### #########
######### Relation sigmoidale pour TA
######### ######### #########
# A : y max : valeur du plateau final
#X0 : Point d'inflection : 50% de Y
#B : point ou la vitesse est maximale
compt=0
for (i in c("TA", "H_1h30", "LE_30m", "SB", "G", "VPD")){
  compt=compt+1
# Parametres à estimer : 
  A<-c(9,9,9,9,9,9)
  X0<-c(20,100,100,10,5,1)
  B<-c(3,-100,0,-10,-10,0)
  
  #Ajust
  y<-donnees$SAP_FLOW
  x<-donnees[,which(names(donnees)==i)]
  
  # Plot obs:
  plot(y~x)
  
  fit0<-NULL
  fit0<-nls(y ~ A / ( 1 + exp (-((x - X0) / B))),start = list(A = A[compt], B = B[compt], X0 =X0[compt]),algorithm="port")
  A[compt]<-summary(fit0)$coef[1]
  B[compt]<-summary(fit0)$coef[2]
  X0[compt]<-summary(fit0)$coef[3]
  x<-min(x,na.rm=T):max(x,na.rm=T)
  points(A[compt] / ( 1 + exp (-((x - X0[compt]) / B[compt])))~x,type="l",col="red",lwd=4)
  
  #### Transformer TA
  
  trans <- A[compt] / ( 1 + exp (-((donnees[,which(names(donnees)==i)]- X0[compt]) / B[compt])))
  plot(donnees$SAP_FLOW~trans)
  trans_nom <- paste0(i,"_trans")
  donnees$trans_nom<-trans
}
