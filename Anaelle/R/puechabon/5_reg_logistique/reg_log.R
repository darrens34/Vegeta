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


############# ############# ############# ############# 
"""
Relation lineaire avec le sap flow pour : 
NETRAD_1h30, PPFD_IN_1h, PPFD_OUT_1h, RH, SW_IN_1h, SW_OUT_30m H_1h30

Relation sigmoidale pour : 
TA, SB, G

Relation log :ln(Ax)
LE_30m, VPD

Genre de LOG inverse: ln(1/AX)+B
CO2, FC_1h, TAU_30m, USTAR_30m, WS

Pas de relation:
P_1h PA_3h PPFD_DIF_1h TS TS_2 TS_3 WD_1h30 WS H2O_3h SC_3h SH_3h SLE_3h ZL_3h
"""

######### ######### #########
######### Relation sigmoidale pour TA
######### ######### #########
# A : y max : valeur du plateau final
#X0 : Point d'inflection : 50% de Y
#B : point ou la vitesse est maximale
compt=0
# Parametres à estimer : 
A<-c(10,9,9)
X0<-c(15,0,0)
B<-c(3,-5,-5)

for (i in c("TA", "SB", "G")){
  compt=compt+1
  #Ajust
  y<-donnees$SAP_FLOW
  x<-donnees[,which(names(donnees)==i)]
  
  # Plot obs:
  plot(y~x,xlab=i,ylab="sap flow")
  
  fit0<-NULL
  fit0<-nls(y ~ A / ( 1 + exp (-((x - X0) / B))),start = list(A = A[compt], B = B[compt], X0 =X0[compt]),nls.control(maxiter = 100000, tol = 1e05),algorithm="port")
  A[compt]<-summary(fit0)$coef[1]
  B[compt]<-summary(fit0)$coef[2]
  X0[compt]<-summary(fit0)$coef[3]
  x<-min(x,na.rm=T):max(x,na.rm=T)
  points(A[compt] / ( 1 + exp (-((x - X0[compt]) / B[compt])))~x,type="l",col="red",lwd=4)
  
  #### Transformer TA
  
  trans <- A[compt] / ( 1 + exp (-((donnees[,which(names(donnees)==i)]- X0[compt]) / B[compt])))
  plot(donnees$SAP_FLOW~trans,xlab =i)
  trans_nom <- paste0(i,"_trans")
  donnees$trans_nom<-trans
}
