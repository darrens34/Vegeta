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
LE_30m, VPD

Genre de LOG inverse: ln(1/AX)+B
CO2, FC_1h, TAU_30m, USTAR_30m, WS

Pas de relation:
P_1h PA_3h PPFD_DIF_1h TS TS_2 TS_3 WD_1h30 WS H2O_3h SC_3h SH_3h SLE_3h ZL_3h
"""

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

for (i in c("TA", "SB", "G","LE_30m", "VPD")){
  compt=compt+1
  #Ajust
  y<-donnees$SAP_FLOW
  x<-donnees[,which(names(donnees)==i)]
  
  # Plot obs:
  plot(y~x,xlab=i,ylab="sap flow")
  
  fit0<-NULL
  fit0<-nls(y ~ flogis(x, xmid, scal), data = data.frame(x, y),control=nls.control(maxiter = 100000),start=list( xmid=10, scal=3))
  xmid[compt]<-summary(fit0)$coef[1]
  scal[compt]<-summary(fit0)$coef[2]
  xplot<-min(x,na.rm=T):max(x,na.rm=T)
  points(9.5 / ( 1 + exp ((xmid[compt]-xplot) / scal[compt])) ~ xplot,type="l",col="red",lwd=4)
  
  trans <- 9.5/ ( 1 + exp ((xmid[compt]- x) / scal[compt]))
  plot(donnees$SAP_FLOW~trans,xlab =i)
}

######### ######### #########
######### TA
######### ######### #########
# paufinement
#Ajust
i="TA"
y<-donnees$SAP_FLOW
x<-donnees[,which(names(donnees)==i)]
plot(y~x,xlab=i,ylab="sap flow")
xplot<-min(x,na.rm=T):max(x,na.rm=T)
points(9.5 / ( 1 + exp ((20-xplot) / 4)) ~ xplot,type="l",col="red",lwd=4)


trans <- 9.5 / ( 1 + exp ((20-x) / 4))
trans[which(trans=="Inf")]<-0
plot(donnees$SAP_FLOW~trans,xlab =i)
donnees$"TA_trans"<-trans

######### ######### #########
######### SB
######### ######### #########
# paufinement
#Ajust
i="SB"
y<-donnees$SAP_FLOW
x<-donnees[,which(names(donnees)==i)]
plot(y~x,xlab=i,ylab="sap flow")
xplot<-min(x,na.rm=T):max(x,na.rm=T)
points(9.5 / ( 1 + exp ((8-xplot) / 4)) ~ xplot,type="l",col="red",lwd=4)

trans <- 9.5 / ( 1 + exp ((8-x) / 4))
trans[which(trans=="Inf")]<-0
plot(donnees$SAP_FLOW~trans,xlab =i)
donnees$"SB_trans"<-trans

######### ######### #########
######### G
######### ######### #########
# paufinement
#Ajust
i="G"
y<-donnees$SAP_FLOW
x<-donnees[,which(names(donnees)==i)]
plot(y~x,xlab=i,ylab="sap flow")
xplot<-min(x,na.rm=T):max(x,na.rm=T)
points(9.5 / ( 1 + exp ((6-xplot) / 3)) ~ xplot,type="l",col="red",lwd=4)

trans <- 9.5 / ( 1 + exp ((6-x) / 3))
trans[which(trans=="Inf")]<-0
plot(donnees$SAP_FLOW~trans,xlab =i)
donnees$"G_trans"<-trans

######### ######### #########
######### LE_30m
######### ######### #########
# paufinement
#Ajust
i="LE_30m"
y<-donnees$SAP_FLOW
x<-donnees[,which(names(donnees)==i)]
plot(y~x,xlab=i,ylab="sap flow")
xplot<-min(x,na.rm=T):max(x,na.rm=T)
points(9 / ( 1 + exp ((92-xplot) / 34)) ~ xplot,type="l",col="red",lwd=4)

trans <- 9 / ( 1 + exp ((92-x) / 34))
trans[which(trans=="Inf")]<-0
plot(donnees$SAP_FLOW~trans,xlab =i)
donnees$"LE_30m_trans"<-trans

######### ######### #########
######### VPD
######### ######### #########
# paufinement
#Ajust
i="VPD"
y<-donnees$SAP_FLOW
x<-donnees[,which(names(donnees)==i)]
plot(y~x,xlab=i,ylab="sap flow")
xplot<-min(x,na.rm=T):max(x,na.rm=T)
points(9 / ( 1 + exp ((1.06-xplot) / 0.33)) ~ xplot,type="l",col="red",lwd=4)

trans <- 9 / ( 1 + exp ((1.06-x) / 0.33))
trans[which(trans=="Inf")]<-0
plot(donnees$SAP_FLOW~trans,xlab =i)
donnees$"VPD_trans"<-trans

######### ######### ################## ######### #########
######### Relation log inverse
######### ######### ################## ######### #########

######### ######### #########
######### CO2
######### ######### #########
i = "CO2"

#Ajust
y<-donnees$SAP_FLOW
x<-donnees[,which(names(donnees)==i)]

# Plot obs:
plot(y~x,xlab=i,ylab="sap flow")

xplot<-330:max(x,na.rm=T)
#points(0.0000000000000000000001 * exp(10000/(xplot-200)) ~ xplot,type="l",col="red",lwd=4)
points(0.00000000002 * exp(10000/(xplot+0)) ~ xplot,type="l",col="red",lwd=4)

trans <- 0.00000000002 * exp(10000/(x+0))
trans[which(trans=="Inf")]<-0
plot(donnees$SAP_FLOW~trans,xlab =i,xlim=c(0,20))
donnees$"CO2_trans"<-trans


######### ######### #########
######### FC_1h
######### ######### #########
i = "FC_1h"

#Ajust
y<-donnees$SAP_FLOW
x<-donnees[,which(names(donnees)==i)]

# Plot obs:
plot(y~x,xlab=i,ylab="sap flow")
xplot<-min(x,na.rm=T):max(x,na.rm=T)
#points(0.0000000000000000000001 * exp(10000/(xplot-200)) ~ xplot,type="l",col="red",lwd=4)
points(0.01 * exp(210/(xplot+50)) ~ xplot,type="l",col="red",lwd=4)

trans <- 0.01 * exp(210/(x+50))
trans[which(trans=="Inf")]<-0
plot(donnees$SAP_FLOW~trans,xlab =i,xlim=c(0,20))
donnees$"FC_1h_trans"<-trans


######### ######### #########
######### TAU_30m
######### ######### #########
i = "TAU_30m"

#Ajust
y<-donnees$SAP_FLOW
x<-donnees[,which(names(donnees)==i)]

# Plot obs:
plot(y~x,xlab=i,ylab="sap flow")
xplot<-seq(from=min(x,na.rm=T),to=max(x,na.rm=T),by=0.01)
#points(0.0000000000000000000001 * exp(10000/(xplot-200)) ~ xplot,type="l",col="red",lwd=4)
points(0.01 * exp(1/(xplot+0)) ~ xplot,type="l",col="red",lwd=4)

trans <- 0.01 * exp(1/(x+0))
trans[which(trans=="Inf")]<-0
plot(donnees$SAP_FLOW~trans,xlab =i,xlim=c(0,20))
donnees$"TAU_30m_trans"<-trans



######### ######### ######### 
######### Enleve données de base qui ont été transformées
######### ######### ######### 
donnees = donnees[,-which(colnames(donnees) %in% c("TA","SB","G",  "LE_30m","VPD","CO2","FC_1h","TAU_30m"))]
donnees <-donnees[-1102,] # outlier

######### ######### ######### 
######### Regarde les correlations entre variables
######### ######### ######### 
corDF = cor(donnees,use="complete.obs")
dissimilarity <- 1 - abs(corDF)
distance <- as.dist(dissimilarity)
hc <- hclust(distance)
clusterV = cutree(hc,h=0.10) # On coupe à 90% de similarité
print(clusterV)

"""
     SAP_FLOW   NETRAD_1h30          P_1h         PA_3h   PPFD_DIF_1h    PPFD_IN_1h   PPFD_OUT_1h            RH      SW_IN_1h 
            1             2             3             4             5             2             2             6             2 
SW_OUT_30m            TS          TS_2          TS_3       WD_1h30            WS        H_1h30        H2O_3h         SC_3h 
2             7             7             7             8             9            10            11            12 
SH_3h        SLE_3h     USTAR_30m         ZL_3h      TA_trans      SB_trans       G_trans  LE_30m_trans     VPD_trans 
13            14            15            16            17            18            18             1            19 
CO2_trans   FC_1h_trans TAU_30m_trans 
20            21            22

Groupe 2 : NETRAD_1h30, PPFD_IN_1h, PPFD_OUT_1h, SW_IN_1h, SW_OUT_30m
Groupe 7 : Ts, ts_2, ts_3
Groupe 18 : SB_trans, G_trans
"""

# Dans chaque groupe, on garde uniquement le plus correlé au sap flow
cor(donnees$SAP_FLOW,donnees[,-which(colnames(donnees)=="SAP_FLOW")],use="complete.obs")

#############  Suppression des colonnes suivantes:
donnees<- donnees[,-which(names(donnees) %in% c("NETRAD_1h30","PPFD_OUT_1h"	,"SW_IN_1h","SW_OUT_30m"	,"TS_2","TS_3","G_trans"))]


######### Selection du meilleur modele :
library(leaps)

mod1 = regsubsets(SAP_FLOW ~  ., data = donnees,method = "exhaustive",nvmax=ncol(donnees)-1) 

plot(mod1, scale = "bic")
obj_mod1 = summary(mod1)
res_mod1 = obj_mod1$which[which.min(obj_mod1$bic),]*1
sum(res_mod1) # nb de variables retenues
res_mod1

summary(lm(SAP_FLOW ~  P_1h + PPFD_IN_1h + TS +  WD_1h30+ WS+
              SH_3h +USTAR_30m+ ZL_3h +SB_trans+LE_30m_trans+ VPD_trans+ CO2_trans, data = donnees))

mod = lm(SAP_FLOW ~ P_1h + PPFD_IN_1h + TS +  WD_1h30+ WS+
           SH_3h +USTAR_30m+ ZL_3h +SB_trans+LE_30m_trans+ VPD_trans+ CO2_trans, data = donnees)

######
summary(mod) # R2 adj = 0.93
plot(mod)



# plot pour une journée moyenne:
path<-"F:/MIASHS/TER/Vegeta/Vegeta_Web/data/puechabon/reg_non_lin/X_par_heure.csv"
X <- read.table(path,sep=",",dec=".",header=T,row.names = 1)
head(X)
# Transformation des X gardés dans le modèle : 
SB_trans =  9.5 / ( 1 + exp ((8-X[which(rownames(X)=="SB"),]) / 4))
X = rbind(X,"SB_trans"=SB_trans)
X = X[-which(rownames(X)=="SB"),]

LE_30m_trans = 9 / ( 1 + exp ((92-X[which(rownames(X)=="LE_30m"),]) / 34))
X = rbind(X,"LE_30m_trans"=LE_30m_trans)
X = X[-which(rownames(X)=="LE_30m"),]

VPD_trans = 9 / ( 1 + exp ((1.06-X[which(rownames(X)=="VPD"),]) / 0.33))
X = rbind(X,"VPD_trans"=VPD_trans)
X = X[-which(rownames(X)=="VPD"),]

CO2_trans = 0.00000000002 * exp(10000/X[which(rownames(X)=="CO2"),])
X = rbind(X,"CO2_trans"=CO2_trans)
X = X[-which(rownames(X)=="CO2"),]

plot(predict.lm(mod,newdata=as.data.frame(t(X)))~seq(0,23.50,0.50),ylab="Flux de sève predit",xlab="Heure solaire",type="l",col="darkgreen")
write.csv(data.frame(predict.lm(mod,newdata=as.data.frame(t(X)))),"y_predit_par_heure.csv")


# equation du modèle global:
coef<-round(mod$coefficients,4)
equation_modele_global <- paste("Flux de sève = ",coef[1]," + ",coef[2]," P_1h + ",coef[3],"  PPFD_IN_1h + ",coef[4]," TS + ",
                                coef[5]," WD_1h30 + ",coef[6],"  WS + ",coef[7]," SH_3h + ",coef[8]," USTAR_30m + ",
                                coef[9]," ZL_3h + ",coef[10]," SB_trans + ",coef[11]," LE_30m_trans + ",coef[12]," VPD_trans + ",coef[13]," CO2_trans")


