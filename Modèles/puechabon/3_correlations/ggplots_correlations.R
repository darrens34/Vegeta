
########## IMPORTATION #################
path<-"F:/MIASHS/TER/Vegeta/data/data_pour_visu_nabil/Puechabon/correlations/Puechabon_2010_post_model.csv" 
data <- read.table(path,sep="\t",dec=".",header=T,na.strings = c("-9999","NA"))
head(data)
setwd("F:/MIASHS/TER/Vegeta/Anaelle/R/puechabon")
summary(data)

########## Librairies ################
library(ggplot2)
theme_set(theme_bw())

## PPFD_IN_1h
p<-ggplot(data,
       aes(x=PPFD_IN_1h, y=SAP_FLOW))+
        theme(axis.text=element_text(size=12,color="black"),
        axis.title=element_text(size=14))+
        geom_point( size = 2, color = "black") +  
        xlab(bquote('PPFD_IN 1h avant ('*mu~ 'mol' ~m^-2~s^-1*')'))+
        ylab(bquote('Flux de sève (mmol '*H[2]*'O '*~ m^-2~s^-1*')'))
p+ annotate(geom="text", x=100, y=9.5, label="R² = 0.83",color="black") +
  geom_smooth(method='lm',formula=y~x,color="red")

summary(lm(data$SAP_FLOW~data$PPFD_IN_1h))

## TS
p<-ggplot(data,
          aes(x=TS, y=SAP_FLOW))+
  theme(axis.text=element_text(size=12,color="black"),
        axis.title=element_text(size=14))+
  geom_point( size = 2, color = "black") +  
  xlab(bquote('Température du  sol (°C)'))+
  ylab(bquote('Flux de sève (mmol '*H[2]*'O '*~ m^-2~s^-1*')'))
p+ annotate(geom="text", x=8, y=9.5, label="R² = 0.11",color="black")
  #geom_smooth(method='lm',formula=y~x,color="red")

summary(lm(data$SAP_FLOW~data$TS))

#  WD_2h
p<-ggplot(data,
          aes(x=WD_2h, y=SAP_FLOW))+
  theme(axis.text=element_text(size=12,color="black"),
        axis.title=element_text(size=14))+
  geom_point( size = 2, color = "black") +  
  xlab(bquote('Direction du vent 2 heures avant (°décimal)'))+
  ylab(bquote('Flux de sève (mmol '*H[2]*'O '*~ m^-2~s^-1*')'))
p+ annotate(geom="text", x=30, y=10, label="R² = 0.005",color="black")
#geom_smooth(method='lm',formula=y~x,color="red")

summary(lm(data$SAP_FLOW~data$WD_2h))


#  CO2
p<-ggplot(data,
          aes(x=CO2, y=SAP_FLOW))+
  theme(axis.text=element_text(size=12,color="black"),
        axis.title=element_text(size=14))+
  geom_point( size = 2, color = "black") +  
  xlab(bquote('Concentration du '*CO[2]* '(ppm)'))+
  ylab(bquote('Flux de sève (mmol '*H[2]*'O '*~ m^-2~s^-1*')'))
p+ annotate(geom="text", x=350, y=10, label="R² = 0.31",color="black")
#geom_smooth(method='lm',formula=y~x,color="red")

summary(lm(data$SAP_FLOW~data$CO2))


#  FC_1h
p<-ggplot(data,
          aes(x=FC_1h, y=SAP_FLOW))+
  theme(axis.text=element_text(size=12,color="black"),
        axis.title=element_text(size=14))+
  geom_point( size = 2, color = "black") +  
  xlab(bquote('Flux de '*CO[2]*' 1 heure avant ('*mu~ 'mol' ~m^-2~s^-1*')'))+
  ylab(bquote('Flux de sève (mmol '*H[2]*'O '*~ m^-2~s^-1*')'))
p+ annotate(geom="text", x=-35, y=10, label="R² = 0.57",color="black")
#geom_smooth(method='lm',formula=y~x,color="red")

summary(lm(data$SAP_FLOW~data$FC_1h))

#  H2O_3h
p<-ggplot(data,
          aes(x=H2O_3h, y=SAP_FLOW))+
  theme(axis.text=element_text(size=12,color="black"),
        axis.title=element_text(size=14))+
  geom_point( size = 2, color = "black") +  
  xlab(bquote('Eau (mmol '*H[2]*'O '*mol^-1*') 3 heures avant'))+
  ylab(bquote('Flux de sève (mmol '*H[2]*'O '*~ m^-2~s^-1*')'))
p+ annotate(geom="text", x=8, y=10, label="R² = 0.06",color="black")
#geom_smooth(method='lm',formula=y~x,color="red")

summary(lm(data$SAP_FLOW~data$H2O_3h))

#  SB
p<-ggplot(data,
          aes(x=SB, y=SAP_FLOW))+
  theme(axis.text=element_text(size=12,color="black"),
        axis.title=element_text(size=14))+
  geom_point( size = 2, color = "black") +  
  xlab(bquote('Stock de chaleur dans la biomasse (W '*m^-2*')'))+
  ylab(bquote('Flux de sève (mmol '*H[2]*'O '*~ m^-2~s^-1*')'))
p+ annotate(geom="text", x=-10, y=10, label="R² = 0.52",color="black")
#geom_smooth(method='lm',formula=y~x,color="red")

summary(lm(data$SAP_FLOW~data$SB))

#  SH_3h
p<-ggplot(data,
          aes(x=SH_3h, y=SAP_FLOW))+
  theme(axis.text=element_text(size=12,color="black"),
        axis.title=element_text(size=14))+
  geom_point( size = 2, color = "black") +  
  xlab(bquote('Flux de Stockage de chaleur sensible (W '*m^-2*') 3 heures avant'))+
  ylab(bquote('Flux de sève (mmol '*H[2]*'O '*~ m^-2~s^-1*')'))
p+ annotate(geom="text", x=-40, y=10, label="R² = 0.25",color="black")
#geom_smooth(method='lm',formula=y~x,color="red")

summary(lm(data$SAP_FLOW~data$SH_3h))

#  SLE_3h
p<-ggplot(data,
          aes(x=SLE_3h, y=SAP_FLOW))+
  theme(axis.text=element_text(size=12,color="black"),
        axis.title=element_text(size=14))+
  geom_point( size = 2, color = "black") +  
  xlab(bquote('Latent heat storage flux (W '*m^-2*') 3 heures avant'))+
  ylab(bquote('Flux de sève (mmol '*H[2]*'O '*~ m^-2~s^-1*')'))
p+ annotate(geom="text", x=-100, y=10, label="R² = 0.02",color="black")
#geom_smooth(method='lm',formula=y~x,color="red")

summary(lm(data$SAP_FLOW~data$SLE_3h))

#  ZL_3h
p<-ggplot(data,
          aes(x=ZL_3h, y=SAP_FLOW))+
  theme(axis.text=element_text(size=12,color="black"),
        axis.title=element_text(size=14))+
  geom_point( size = 2, color = "black") +  
  xlab(bquote('Parametre de stabilité 3 heures avant'))+
  ylab(bquote('Flux de sève (mmol '*H[2]*'O '*~ m^-2~s^-1*')'))
p+ annotate(geom="text", x=-40, y=10, label="R² = 0.09",color="black")
#geom_smooth(method='lm',formula=y~x,color="red")

summary(lm(data$SAP_FLOW~data$ZL_3h))


#   VPD
p<-ggplot(data,
          aes(x= VPD, y=SAP_FLOW))+
  theme(axis.text=element_text(size=12,color="black"),
        axis.title=element_text(size=14))+
  geom_point( size = 2, color = "black") +  
  xlab(bquote('Déficit de pression de vapeur (kPa)'))+
  ylab(bquote('Flux de sève (mmol '*H[2]*'O '*~ m^-2~s^-1*')'))
p+ annotate(geom="text", x=0.5, y=10, label="R² = 0.66",color="black")
#geom_smooth(method='lm',formula=y~x,color="red")

summary(lm(data$SAP_FLOW~data$ VPD))


