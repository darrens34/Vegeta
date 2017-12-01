
########## IMPORTATION #################
path<-"F:/MIASHS/TER/Vegeta/data/data_pour_visu_nabil/Puechabon/correlations/Puechabon_2010_post_model.csv" 
data <- read.table(path,sep="\t",dec=".",header=T,na.strings = c("-9999","NA"))
head(data)
setwd("F:/MIASHS/TER/Vegeta/Anaelle/R/puechabon")
summary(data)

########## Librairies ################
library(ggplot2)
theme_set(theme_bw())

ggplot(data,
       aes(x=PPFD_IN_1h,
           y=SAP_FLOW))+
  geom_point( size = 2, color = "black") +  xlab("PPFD_IN 1h avant (??mol m-2 s-1)") + ylab("Flux de sève (mmolH2O m-2 s-1)")

