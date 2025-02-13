########## Importer les librairies
library(leaps)
?regsubsets

######### ######### ######### 
######### importer et transformer les donn�es
######### ######### ######### 

############# Importer les donn�es
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
# On enl�ve PRI car on a pas de mesure la nuit! Pas possible de pr�dire avec les NA ensuite :(
#donnees = donnees[,-which(colnames(donnees)=="PRI")]

######### ######### ######### 
######### Regarde les correlations entre variables
######### ######### ######### 
corDF = cor(donnees,use="complete.obs")
dissimilarity <- 1 - abs(corDF)
distance <- as.dist(dissimilarity)
hc <- hclust(distance)
clusterV = cutree(hc,h=0.10) # On coupe � 90% de similarit�
print(clusterV)

"""
   SAP_FLOW NETRAD_1h30        P_1h       PA_3h PPFD_DIF_1h  PPFD_IN_1h PPFD_OUT_1h          RH    SW_IN_1h  SW_OUT_30m          TA 
          1           2           3           4           5           2           2           6           2           2           7 
TS        TS_2        TS_3     WD_1h30          WS         CO2       FC_1h      H_1h30      H2O_3h      LE_30m          SB 
8           8           8           9          10          11          12          13          14           1          15 
SC_3h       SH_3h      SLE_3h     TAU_30m   USTAR_30m       ZL_3h           G         VPD 
16          17          18          19          19          20          15           6 

Groupe 2 : Netrad, ppfd_in, ppfd_out, sw_in, sw_out
Groupe 6 : RH, VPD
Groupe 8 : Ts, ts_2, ts_3
Groupe 15 : SB, G
Groupe 19 : TAU, USTAR
"""
# Dans chaque groupe, on garde uniquement le plus correl� au sap flow
cor(donnees$SAP_FLOW,donnees[,-which(colnames(donnees)=="SAP_FLOW")],use="complete.obs")

#############  Suppression des colonnes suivantes:
donnees<- donnees[,-which(names(donnees) %in% c("NETRAD_1h30","PPFD_OUT_1h"	,"SW_IN_1h","SW_OUT_30m"	,"TS_2","TS_3","TAU_30m","G","RH"))]


######### Selection du meilleur modele :

mod1 = regsubsets(SAP_FLOW ~  ., data = donnees,method = "exhaustive",nvmax=ncol(donnees)-1) 

plot(mod1, scale = "bic")
obj_mod1 = summary(mod1)
res_mod1 = obj_mod1$which[which.min(obj_mod1$bic),]*1
sum(res_mod1) # nb de variables retenues
res_mod1

summary(lm(SAP_FLOW ~  PPFD_IN_1h + TA+ TS +  WD_1h30+CO2+
             FC_1h + LE_30m + SH_3h + ZL_3h + VPD , data = donnees))

mod = lm(SAP_FLOW ~  PPFD_IN_1h + TA+ TS +  WD_1h30+CO2+
           FC_1h + LE_30m + SH_3h + ZL_3h + VPD , data = donnees)

######
summary(mod)
# R2 adj = 0.918
plot(mod)


####################### PREDICTIONS
path<-"F:/MIASHS/TER/Vegeta/data/data_train_test/Puechabon_test.csv" 
donnes_test<-read.table(path,sep=",",dec=".",header=T,na.strings = c("-9999","NA"))
donnes_test= donnes_test[,-which(colnames(donnes_test)=="X")]# supprime premier colonne d'index

#############  Supprime colonnes inutiles pour le moment
donnes_test<- donnes_test[,-which(names(donnes_test) %in% c("dates","heure_solaire","type"))]

# prediction
pred <- predict.lm(mod,donnes_test)
rmse <- sqrt(mean((donnes_test$SAP_FLOW - pred)^2,na.rm=TRUE)) ; rmse
# 0.782518


# On ecrit les donn�es post modelisation : 
write.csv(donnees,"Puechabon_2010_post_model.csv")
