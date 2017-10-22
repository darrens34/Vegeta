########## Importer les librairies
library(leaps)
?regsubsets

######### ######### ######### 
######### importer et transformer les données
######### ######### ######### 

############# Importer les données
path<-"F:/MIASHS/TER/Vegeta/data/data_sans_NA/Puechabon_2010_vpd.csv" 
donnees_base <- read.table(path,sep=",",dec=".",header=T,na.strings = c("-9999","NA"))
head(donnees_base)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")
summary(donnees_base)

donnees_base = donnees_base[,-1]# supprime premier colonne d'index
donnees_base = donnees_base[,-which(colnames(donnees_base)=="TS_2")]
donnees_base = donnees_base[,-which(colnames(donnees_base)=="TS_3")] # Enleve TS_2 et TS_3 car correles a TS

#############  Transforme les dates
donnees_base$dates = strptime(donnees_base$date,format = "%Y-%m-%d %H:%M:%S")
donnees_base$heure_solaire = strptime(donnees_base$heure_solaire,format = "%Y-%m-%d %H:%M:%S")

#############  Supprime colonnes inutiles pour le moment
donnees<- donnees_base[,-which(names(donnees_base) %in% c("TIMESTAMP_START","TIMESTAMP_END","DTime","dates","heure_solaire"))]
# On enlève PRI car on a pas de mesure la nuit! Pas possible de prédire avec les NA ensuite :(
donnees = donnees[,-which(colnames(donnees)=="PRI")]

######### ######### ######### 
######### Regarde les correlations entre variables
######### ######### ######### 
core <- cor(donnees,method="pearson",use = "pairwise.complete.obs")
#write.csv(core,"correlations_entre_variables.csv")

#############  Supprime colonnes corrélées
donnees<- donnees[,-which(names(donnees) %in% c("NETRAD","PPFD_DIF"	,"PPFD_OUT"	,"SW_IN","SW_OUT"	,"H","LE","RH","TA","TAU","USTAR","G"))]


######### ######### ######### 
######### Recherche des variables au différents temps
######### ######### ######### 

######### Selection du meilleur modele au temps t
mod1 = regsubsets(SAP_FLOW ~  P+ PA  +PPFD_IN + TS + WD+WS+ CO2 +  FC  + H2O  + SB + SC +SH  +SLE+ZL   + VPD, 
                  data = donnees,method = "exhaustive",nvmax=ncol(donnees)-1) # 28 est le nombre total de variable
plot(mod1, scale = "bic")
obj_mod1 = summary(mod1)
res_mod1 = obj_mod1$which[which.min(obj_mod1$bic),]*1
sum(res_mod1) # nb de variables retenues
res_mod1

summary(lm(SAP_FLOW ~  PA + PPFD_IN  + TS + WD + WS + CO2 + FC +  H2O  + SH + VPD , data = donnees))


######### Selection du meilleur modele au temps t + 30 min
donnees_30 = cbind(SAP_FLOW=donnees$SAP_FLOW[-1],donnees[-nrow(donnees),-which(colnames(donnees)=="SAP_FLOW")])

mod1 = regsubsets(SAP_FLOW ~  P+ PA  +PPFD_IN + TS + WD+WS+ CO2 +  FC  + H2O  + SB + SC +SH  +SLE+ZL   + VPD, 
                  data = donnees_30,method = "exhaustive",nvmax=ncol(donnees_30)-1) # 28 est le nombre total de variableplot(mod1, scale = "bic")
obj_mod1 = summary(mod1)
res_mod1 = obj_mod1$which[which.min(obj_mod1$bic),]*1
sum(res_mod1) # nb de variables retenues
res_mod1

summary(lm(SAP_FLOW ~  PA + PPFD_IN  + TS + WD + WS + CO2 + FC  +SB + SH + SLE + VPD , data = donnees_30))

# - H2O + SB + SLE

######### Selection du meilleur modele au temps t + 1 heure
donnees_1h = cbind(SAP_FLOW=donnees$SAP_FLOW[-c(1,2)],donnees[-c(nrow(donnees)-1,nrow(donnees)),-which(colnames(donnees)=="SAP_FLOW")])

mod1 = regsubsets(SAP_FLOW ~  P+ PA  +PPFD_IN + TS + WD+WS+ CO2 +  FC  + H2O  + SB + SC +SH  +SLE+ZL   + VPD, 
                  data = donnees_1h,method = "exhaustive",nvmax=ncol(donnees_1h)-1) # 28 est le nombre total de variableplot(mod1, scale = "bic")

plot(mod1, scale = "bic")
obj_mod1 = summary(mod1)
res_mod1 = obj_mod1$which[which.min(obj_mod1$bic),]*1
sum(res_mod1) # nb de variables retenues
res_mod1

summary(lm(SAP_FLOW ~   PPFD_IN  + TS + WD + WS + CO2 + FC + SB  + SH +SLE + VPD , data = donnees_1h))
# -PA H2O + SB + SLE

######### Selection du meilleur modele au temps t + 1 heure 30
donnees_1h30 = cbind(SAP_FLOW=donnees$SAP_FLOW[-c(1:3)],donnees[-c(nrow(donnees):(nrow(donnees)-2)),-which(colnames(donnees)=="SAP_FLOW")])

mod1 = regsubsets(SAP_FLOW ~  P+ PA  +PPFD_IN + TS + WD+WS+ CO2 +  FC  + H2O  + SB + SC +SH  +SLE+ZL   + VPD, 
                  data = donnees_1h30,method = "exhaustive",nvmax=ncol(donnees_1h30)-1) # 28 est le nombre total de variableplot(mod1, scale = "bic")

plot(mod1, scale = "bic")
obj_mod1 = summary(mod1)
res_mod1 = obj_mod1$which[which.min(obj_mod1$bic),]*1
sum(res_mod1) # nb de variables retenues
res_mod1

summary(lm(SAP_FLOW ~   PPFD_IN  + TS  + WS  + FC +  H2O  +SB +SC  + VPD , data = donnees_1h30))

# -PA  - WD - CO2 + SB + SC - SH

######### Selection du meilleur modele au temps t + 2 heure
donnees_2h = cbind(SAP_FLOW=donnees$SAP_FLOW[-c(1:4)],donnees[-c(nrow(donnees):(nrow(donnees)-3)),-which(colnames(donnees)=="SAP_FLOW")])

mod1 = regsubsets(SAP_FLOW ~  P+ PA  +PPFD_IN + TS + WD+WS+ CO2 +  FC  + H2O  + SB + SC +SH  +SLE+ZL   + VPD, 
                  data = donnees_2h,method = "exhaustive",nvmax=ncol(donnees_2h)-1) # 28 est le nombre total de variableplot(mod1, scale = "bic")

plot(mod1, scale = "bic")
obj_mod1 = summary(mod1)
res_mod1 = obj_mod1$which[which.min(obj_mod1$bic),]*1
sum(res_mod1) # nb de variables retenues
res_mod1

summary(lm(SAP_FLOW ~   PPFD_IN  + TS + CO2 + FC +  SB+ SC+ SH +ZL + VPD , data = donnees_2h))
# - PA - WD WS H20 + SB SC ZL


######### Selection du meilleur modele au temps t + 2 heure 30 TODO
donnees_2h30 = cbind(SAP_FLOW=donnees$SAP_FLOW[-c(1:5)],donnees[-c(nrow(donnees):(nrow(donnees)-4)),-which(colnames(donnees)=="SAP_FLOW")])

mod1 = regsubsets(SAP_FLOW ~  P+ PA  +PPFD_IN + TS + WD+WS+ CO2 +  FC  + H2O  + SB + SC +SH  +SLE+ZL   + VPD, 
                  data = donnees_2h30,method = "exhaustive",nvmax=ncol(donnees_2h30)-1) # 28 est le nombre total de variableplot(mod1, scale = "bic")

plot(mod1, scale = "bic")
obj_mod1 = summary(mod1)
res_mod1 = obj_mod1$which[which.min(obj_mod1$bic),]*1
sum(res_mod1) # nb de variables retenues
res_mod1

summary(lm(SAP_FLOW ~  PA + PPFD_IN  + TS +  CO2 + FC +  SB + SC + SH +SLE + ZL + VPD , data = donnees_2h30))
# - WD WS H20 + SB SC SLE ZL - VPD

######### Selection du meilleur modele au temps t + 3 heure
donnees_3h = cbind(SAP_FLOW=donnees$SAP_FLOW[-c(1:6)],donnees[-c(nrow(donnees):(nrow(donnees)-5)),-which(colnames(donnees)=="SAP_FLOW")])

mod1 = regsubsets(SAP_FLOW ~  P+ PA  +PPFD_IN + TS + WD+WS+ CO2 +  FC  + H2O  + SB + SC +SH  +SLE+ZL   + VPD, 
                  data = donnees_3h,method = "exhaustive",nvmax=ncol(donnees_3h)-1) # 28 est le nombre total de variableplot(mod1, scale = "bic")

plot(mod1, scale = "bic")
obj_mod1 = summary(mod1)
res_mod1 = obj_mod1$which[which.min(obj_mod1$bic),]*1
sum(res_mod1) # nb de variables retenues
res_mod1

summary(lm(SAP_FLOW ~  PA + PPFD_IN  + TS +  CO2 + FC + SB + SC  + SH +SLE + ZL + VPD , data = donnees))
# - WD WS H20 + SB SC SLE ZL

######### ######### ######### 
######### Choix du temps pour les variables selectionnées à différents temps
######### ######### ######### 

# Le même modèle a été sélectionné pour toutes les heures.
# Pour chaque variable, on choisi la meilleure heure 


liste_variable = c("P", "PA", "PPFD_IN"  , "TS" , "WD" , "WS" , "CO2" , "FC" ,  "H2O" , "SB" , "SC" , "SH" ,"SLE","ZL", "VPD")
liste_decalage = c("donnees", "donnees_30","donnees_1h","donnees_1h30","donnees_2h","donnees_2h30","donnees_3h")
decalage = list()
variable = list()
compt = 0

for( i in c(liste_variable))
  {
    res_bic = list()
    for(les_donnees in liste_decalage )
    {
      res_bic = c(res_bic, BIC(lm(SAP_FLOW~get(i),data = get(les_donnees))))
    }
    compt = compt + 1
    decalage[compt] = liste_decalage[which.min(res_bic)]
    variable[compt] = i
}

cbind(decalage,variable)

######### ######### ######### 
######### Nouveau jeu de données avec les variables décallées
######### ######### ######### 

data = data.frame(SAP_FLOW = donnees$SAP_FLOW[-c(1:6)],
            dates = donnees_base$dates[-c(1:6)],
            heure_solaire = donnees_base$heure_solaire[-c(1:6)],
            P_2h30 = donnees$P[-c(1,nrow(donnees)-4,nrow(donnees)-3,nrow(donnees)-2,nrow(donnees)-1,nrow(donnees))],
            PA_3h = donnees$PA[-c(nrow(donnees)-5,nrow(donnees)-4,nrow(donnees)-3,nrow(donnees)-2,nrow(donnees)-1,nrow(donnees))],
            PPFD_IN_1h = donnees$PPFD_IN[-c(1:4,nrow(donnees)-1,nrow(donnees))],
            TS = donnees$TS[-c(1:6)],
            WD_2h = donnees$WD[-c(1:2,nrow(donnees)-3,nrow(donnees)-2,nrow(donnees)-1,nrow(donnees))],
            WS_3h = donnees$WS[-c(nrow(donnees)-5,nrow(donnees)-4,nrow(donnees)-3,nrow(donnees)-2,nrow(donnees)-1,nrow(donnees))],
            CO2 = donnees$CO2[-c(1:6)],
            FC_1h = donnees$FC[-c(1:4,nrow(donnees)-1,nrow(donnees))],
            H2O_3h = donnees$H2O[-c(nrow(donnees)-5,nrow(donnees)-4,nrow(donnees)-3,nrow(donnees)-2,nrow(donnees)-1,nrow(donnees))],
            SB = donnees$SB[-c(1:6)],
            SC_3h = donnees$SC[-c(nrow(donnees)-5,nrow(donnees)-4,nrow(donnees)-3,nrow(donnees)-2,nrow(donnees)-1,nrow(donnees))],
            SH_3h = donnees$SH[-c(nrow(donnees)-5,nrow(donnees)-4,nrow(donnees)-3,nrow(donnees)-2,nrow(donnees)-1,nrow(donnees))],
            SLE_3h = donnees$SLE[-c(nrow(donnees)-5,nrow(donnees)-4,nrow(donnees)-3,nrow(donnees)-2,nrow(donnees)-1,nrow(donnees))],
            ZL_3h = donnees$ZL[-c(nrow(donnees)-5,nrow(donnees)-4,nrow(donnees)-3,nrow(donnees)-2,nrow(donnees)-1,nrow(donnees))],
            VPD = donnees$VPD[-c(1:6)])
            
            


######### ######### ######### 
######### Selection finale des parametres:
######### ######### ######### 

summary(data)

mod_fin = regsubsets(SAP_FLOW ~  P_2h30+ PA_3h  +PPFD_IN_1h + TS + WD_2h+WS_3h+ CO2 +  FC_1h  + H2O_3h  + SB + SC_3h +SH_3h +SLE_3h +ZL_3h   + VPD, 
                  data = data,method = "exhaustive",nvmax=ncol(data)-1) # 28 est le nombre total de variableplot(mod1, scale = "bic")

plot(mod_fin, scale = "bic")
obj_mod_fin = summary(mod_fin)
res_mod_fin = obj_mod_fin$which[which.min(obj_mod_fin$bic),]*1
sum(res_mod_fin) # nb de variables retenues
res_mod_fin

summary(lm(SAP_FLOW ~ PPFD_IN_1h  +TS+ WD_2h+ CO2 +FC_1h+H2O_3h + SB+ SH_3h+SLE_3h+ ZL_3h + VPD, data = data))

data<- data[,-which(names(data) %in% c("P_2h30","PA_3h","WS_3h","SC_3h"))]

# On ecrit les données post modelisation : 
write.csv(data,"Puechabon_2010_post_model.csv")
