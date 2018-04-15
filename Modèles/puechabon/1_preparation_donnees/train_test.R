# Importer les données:
path<-"F:/MIASHS/TER/Vegeta/data/data_brut/Puechabon_2010_vpd.csv" 
donnees <- read.table(path,sep=",",dec=",",header=T,na.strings = c("-9999","NA"))
head(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")

# Nettoyage valeurs bizarre de PA
donnees<-donnees[-which(as.numeric(donnees$PA) < 96),]

# Enleve lignes ou NA dans sap flow
donnees<-donnees[-which(is.na(donnees$SAP_FLOW)),]

# supprime colonnes inutiles
donnees = donnees[,-which(colnames(donnees)=="X")]# supprime premier colonne d'index

# Pas PRI la nuit : On supprime la colonne
donnees = donnees[,-which(colnames(donnees)=="PRI")]
summary(donnees)

# Correlation pour choisir le decalage pour chaque variable
sub_donnees = donnees[,-which(colnames(donnees)%in% c("TIMESTAMP_START","TIMESTAMP_END","DTime","dates","heure_solaire"))]
for(i in 1:ncol(sub_donnees)){
  sub_donnees[,i]<-as.numeric(as.character(sub_donnees[,i]))
}

h0 <- cor(sub_donnees[,-which(colnames(sub_donnees)%in% c("SAP_FLOW"))],sub_donnees$SAP_FLOW,method="pearson",use = "pairwise.complete.obs")
colnames(h0) <- "SAP_FLOW"
h0 <- rbind(h0, "decalage"= 0)

sub_donnees_30 = cbind(SAP_FLOW=sub_donnees$SAP_FLOW[-1],sub_donnees[-nrow(sub_donnees),-which(colnames(sub_donnees)=="SAP_FLOW")])
h30 <- cor(sub_donnees_30[,-which(colnames(sub_donnees_30)%in% c("SAP_FLOW"))],sub_donnees_30$SAP_FLOW,method="pearson",use = "pairwise.complete.obs")
colnames(h30) <- "SAP_FLOW"
h30 <- rbind(h30, "decalage"= 30)

sub_donnees_1h = cbind(SAP_FLOW=sub_donnees$SAP_FLOW[-c(1,2)],sub_donnees[-c(nrow(sub_donnees)-1,nrow(sub_donnees)),-which(colnames(sub_donnees)=="SAP_FLOW")])
h1 <- cor(sub_donnees_1h[,-which(colnames(sub_donnees_1h)%in% c("SAP_FLOW"))],sub_donnees_1h$SAP_FLOW,method="pearson",use = "pairwise.complete.obs")
colnames(h1) <- "SAP_FLOW"
h1 <- rbind(h1, "decalage"= 60)

sub_donnees_1h30 = cbind(SAP_FLOW=sub_donnees$SAP_FLOW[-c(1,2,3)],sub_donnees[-c(nrow(sub_donnees)-2,nrow(sub_donnees)-1,nrow(sub_donnees)),-which(colnames(sub_donnees)=="SAP_FLOW")])
h130 <- cor(sub_donnees_1h30[,-which(colnames(sub_donnees_1h30)%in% c("SAP_FLOW"))],sub_donnees_1h30$SAP_FLOW,method="pearson",use = "pairwise.complete.obs")
colnames(h130) <- "SAP_FLOW"
h130 <- rbind(h130, "decalage"= 90)

sub_donnees_2h = cbind(SAP_FLOW=sub_donnees$SAP_FLOW[-c(1,2,3,4)],sub_donnees[-c(nrow(sub_donnees)-3,nrow(sub_donnees)-2,nrow(sub_donnees)-1,nrow(sub_donnees)),-which(colnames(sub_donnees)=="SAP_FLOW")])
h2 <- cor(sub_donnees_2h[,-which(colnames(sub_donnees_2h)%in% c("SAP_FLOW"))],sub_donnees_2h$SAP_FLOW,method="pearson",use = "pairwise.complete.obs")
colnames(h2) <- "SAP_FLOW"
h2 <- rbind(h2, "decalage"= 120)

sub_donnees_2h30 = cbind(SAP_FLOW=sub_donnees$SAP_FLOW[-c(1,2,3,4,5)],sub_donnees[-c(nrow(sub_donnees)-4,nrow(sub_donnees)-3,nrow(sub_donnees)-2,nrow(sub_donnees)-1,nrow(sub_donnees)),-which(colnames(sub_donnees)=="SAP_FLOW")])
h230 <- cor(sub_donnees_2h30[,-which(colnames(sub_donnees_2h30)%in% c("SAP_FLOW"))],sub_donnees_2h30$SAP_FLOW,method="pearson",use = "pairwise.complete.obs")
colnames(h230) <- "SAP_FLOW"
h230 <- rbind(h230, "decalage"= 150)

sub_donnees_3h = cbind(SAP_FLOW=sub_donnees$SAP_FLOW[-c(1,2,3,4,5,6)],sub_donnees[-c(nrow(sub_donnees)-5,nrow(sub_donnees)-4,nrow(sub_donnees)-3,nrow(sub_donnees)-2,nrow(sub_donnees)-1,nrow(sub_donnees)),-which(colnames(sub_donnees)=="SAP_FLOW")])
h3 <- cor(sub_donnees_3h[,-which(colnames(sub_donnees_3h)%in% c("SAP_FLOW"))],sub_donnees_3h$SAP_FLOW,method="pearson",use = "pairwise.complete.obs")
colnames(h3) <- "SAP_FLOW"
h3 <- rbind(h3, "decalage"= 180)

les_cor<-data.frame(h0,h30,h1,h130,h2,h230,h3)

decalage <- NA
for (i in 1:nrow(les_cor)){
  decalage[i]<-les_cor["decalage",][which.max(abs(les_cor[i,]))]
}

# jeu final à la main  : 
data = data.frame(SAP_FLOW = sub_donnees$SAP_FLOW[-c(1:6)],
                  dates = donnees$dates[-c(1:6)],
                  heure_solaire = donnees$heure_solaire[-c(1:6)],
                  NETRAD_1h30 = sub_donnees$NETRAD[-c(1:3,nrow(sub_donnees)-2,nrow(sub_donnees)-1,nrow(sub_donnees))],
                  P_1h =  sub_donnees$P[-c(1:4,nrow(sub_donnees)-1,nrow(sub_donnees))],
                  PA_3h = sub_donnees$PA[-c(nrow(sub_donnees)-5,nrow(sub_donnees)-4,nrow(sub_donnees)-3,nrow(sub_donnees)-2,nrow(sub_donnees)-1,nrow(sub_donnees))],
                  PPFD_DIF_1h = sub_donnees$PPFD_DIF[-c(1:4,nrow(sub_donnees)-1,nrow(sub_donnees))],
                  PPFD_IN_1h =  sub_donnees$PPFD_IN[-c(1:4,nrow(sub_donnees)-1,nrow(sub_donnees))],
                  PPFD_OUT_1h =  sub_donnees$PPFD_OUT[-c(1:4,nrow(sub_donnees)-1,nrow(sub_donnees))],
                  RH = sub_donnees$RH[-c(1:6)],
                  SW_IN_1h =  sub_donnees$SW_IN[-c(1:4,nrow(sub_donnees)-1,nrow(sub_donnees))],
                  SW_OUT_30m = sub_donnees$SW_OUT[-c(1:5,nrow(sub_donnees))],
                  TA = sub_donnees$TA[-c(1:6)],
                  TS = sub_donnees$TS[-c(1:6)],
                  TS_2 = sub_donnees$TS_2[-c(1:6)],
                  TS_3 = sub_donnees$TS_3[-c(1:6)],
                  WD_1h30 = sub_donnees$WD[-c(1:3,nrow(sub_donnees)-2,nrow(sub_donnees)-1,nrow(sub_donnees))],
                  WS = sub_donnees$WS[-c(1:6)],
                  CO2 =sub_donnees$CO2[-c(1:6)],
                  FC_1h =sub_donnees$FC[-c(1:4,nrow(sub_donnees)-1,nrow(sub_donnees))],
                  H_1h30 = sub_donnees$H[-c(1:3,nrow(sub_donnees)-2,nrow(sub_donnees)-1,nrow(sub_donnees))],
                  H2O_3h = sub_donnees$H2O[-c(nrow(sub_donnees)-5,nrow(sub_donnees)-4,nrow(sub_donnees)-3,nrow(sub_donnees)-2,nrow(sub_donnees)-1,nrow(sub_donnees))],
                  LE_30m =sub_donnees$LE[-c(1:5,nrow(sub_donnees))],
                  SB = sub_donnees$SB[-c(1:6)],
                  SC_3h = sub_donnees$SC[-c(nrow(sub_donnees)-5,nrow(sub_donnees)-4,nrow(sub_donnees)-3,nrow(sub_donnees)-2,nrow(sub_donnees)-1,nrow(sub_donnees))],
                  SH_3h = sub_donnees$SH[-c(nrow(sub_donnees)-5,nrow(sub_donnees)-4,nrow(sub_donnees)-3,nrow(sub_donnees)-2,nrow(sub_donnees)-1,nrow(sub_donnees))],
                  SLE_3h = sub_donnees$SLE[-c(nrow(sub_donnees)-5,nrow(sub_donnees)-4,nrow(sub_donnees)-3,nrow(sub_donnees)-2,nrow(sub_donnees)-1,nrow(sub_donnees))],
                  TAU_30m =sub_donnees$TAU[-c(1:5,nrow(sub_donnees))],
                  USTAR_30m =sub_donnees$USTAR[-c(1:5,nrow(sub_donnees))],
                  ZL_3h = sub_donnees$ZL[-c(nrow(sub_donnees)-5,nrow(sub_donnees)-4,nrow(sub_donnees)-3,nrow(sub_donnees)-2,nrow(sub_donnees)-1,nrow(sub_donnees))],
                  G = sub_donnees$G[-c(1:6)],
                  VPD = sub_donnees$VPD[-c(1:6)]
)



# moyenne des parametres pour chaque heure pour faire une courbe de sap flow predite moyenne
X = data[,-which(names(data) %in% c("SAP_FLOW","dates","heure_solaire"))]
var_heure<-NULL
for (i in levels(data$heure_solaire)){
  vec = NULL
  for(j in names(X)){
    vec=c(vec,mean(X[data$heure_solaire==i,j],na.rm=TRUE))
  }
  var_heure<-cbind(var_heure,vec)
}
var_heure<-as.data.frame(var_heure)
colnames(var_heure)<-levels(data$heure_solaire)
rownames(var_heure)<-names(X)
write.csv(var_heure,"F:/MIASHS/TER/Vegeta/data/data_pour_visu_nabil/Puechabon/par_heure/X_par_heure.csv")

# Separer train et test au hasard
sample <- sample.int(n = nrow(data), size = round(nrow(data)*70/100,0), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]

# Sauvegarde des données : 
write.csv(train,"F:/MIASHS/TER/Vegeta/data/data_train_test/Puechabon_train.csv")
write.csv(test,"F:/MIASHS/TER/Vegeta/data/data_train_test/Puechabon_test.csv")

