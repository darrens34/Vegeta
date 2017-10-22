

# Importer les données:
path<-"F:/MIASHS/TER/Vegeta/data/data_brut/Shale_Hills.csv" 
donnees <- read.table(path,sep="\t",dec=".",header=T,na.strings = c("-9999","NA",""))
head(donnees)
setwd("F:/MIASHS/TER/Vegeta/Anaelle")

# ligne ou le time est a part
ligne = which(!is.na(donnees$time) )
donnees$date = as.character(donnees$date)
donnees$time = as.character(donnees$time)
donnees$date[ligne] <- paste( donnees$date[ligne],donnees$time[ligne])
donnees$date = strptime(donnees$date,format = "%d/%m/%Y %H:%M")

# Supprime qd NA pour sap flow
donnees = donnees[-which(is.na(donnees$sap_flow)),]
donnees = donnees[,-which(names(donnees)=="time")]

# enregistrer le CSV
write.csv(donnees,"Shale_Hills.csv")
