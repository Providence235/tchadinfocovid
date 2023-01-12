4
#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
#             Projet de series Temporelles ITS3                 #
#                                                               #
#                                                               #
#            TJEGA CHRISTINE LAETITIA & WILLIAM MARION NDALA           
#                                                               #
#                                                               #
#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""                                                               #




# PAckages
library(dse1)
library(vars)
library(dse)
library(ggplot2)
library(plotly)
library(readxl)
library(tseries)
require(graphics)

# REPERTOIRE DE TRAVAIL 

setwd("C:\\Users\\FAYETRADING\\Desktop\\ITS3\\tpserietemptt\\projetserietemp\\SERIE TEMP")

###IMPORTATION DE LA BASE DE TRAVAIL

services.frame<-read.table("services.txt", sep="", header=T)
services.frame

afp.frame<-read.table("afp.txt", sep="", header=T)

afp.frame

PIB.frame<-read.table("PIB.txt", sep="", header=T)

PIB.frame

#description de la base

# STRUCTURE des données

str(services.frame) 
str(afp.frame)
str(PIB.frame)

# Statistiques descriptive de la base

summary(services.frame)
summary(afp.frame)
summary(PIB.frame)

# vérification de la présence des valeurs manquantes

any(is.na(services.frame))
any(is.na(afp.frame))
any(is.na(PIB.frame))

###CONFIGURATION DE L'INDICE TEMPORELLE'

services<-ts(services.frame[,1], start=c(1969,1), frequency=1)
services

afp<-ts(afp.frame[,1], start=c(1968,1), frequency=1)
afp

PIB<-ts(PIB.frame[,1], start=c(1969,1), frequency=1)
PIB

#vue d"ensemble jusquen à 2020

#REpresentation sur la meme fenetre

par(mfrow = c(1,3))

plot(services, type = "l", lwd = 4)

plot(afp, type = "l", lwd = 4)

plot(PIB, type = "l", lwd = 4)


# REPresentation conjointe

# Services et PIB

plot(PIB, xlab="Date", ylab="PIB & Services")
lines(services, col="red")

# AFP ET PIB

plot(PIB, xlab="Date", ylab="PIB & AFP")
lines(afp, col="red")

####____Analyse descriptive pour évaluer les effets de la COVID-19___#####

## Pour la variable services

servicescovid<-window(services, start=c(2010,1),end=c(2020,1))

seservicescovid

plot(servicescovid, type = "l", lwd = 4)

## Pour AFP

afp_bis<-window(afp, start=c(2010,1),end=c(2020,1))
afp_bis

plot(afp_bis, type = "l", lwd = 4)

## Pour LE PIB

PIB_bis<-window(PIB, start=c(2013,1),end=c(2020,1))
PIB_bis

plot(PIB_bis, type = "l", lwd = 4)
# REPresentation conjointe

# Services et PIB

plot(PIB_bis, xlab="Date", ylab="PIB & Services")
lines(servicescovid, col="red")

# AFP ET PIB

plot(PIB_bis, xlab="Date", ylab="PIB & aFP")
lines(afp_bis, col="red")


#Modelisation MUltivariee.

length(PIB)
length(services)
length(afp)

par(mfrow = c(1,3))

ts.plot(PIB)
ts.plot(services)

ts.plot(afp)

#stationnarité

## La  série AFP semble être non stationnaire

adf.test(PIB)
adf.test(services)
adf.test(afp)

pp.test(PIB)
kpss.test(PIB)

pp.test(afp)
kpss.test(afp)

pp.test(services)
kpss.test(services)


# differenciation AFP


DiffPIB<-diff(PIB)
Diffservices<-diff(services)

# PIB Est STat maintenant

adf.test(DiffPIB)
pp.test(DiffPIB)
kpss.test(DiffPIB)

adf.test(Diffservices)
pp.test(Diffservices)
kpss.test(Diffservices)

length(Diffservices)
length(DiffPIB)


# creation de la matrice

XY<-matrix(rep(0,1),53,2)

XY[,1]<-DiffPIB
XY[,2]<-Diffservices

XY

# aFC

acf(XY,20,"correlation")



#Representation des séries à l'aide d'une seule fonction

plot.ts(XY, main="PIB services", xlab="Croissance")

##### Determer l'ordre p de VAR(p)

ordre=VARselect(XY, lag.max =3)
ordre 
# on retient l'ordre 2


##### Estimation du modele

## affectation 

colnames(XY)<-c("y1","y2") 


varest_1=VAR(XY,p=2)

varest_1


#validation du modèle 

summary(varest_1)
coef(varest_1)

#ReprÃ©sentation de l'inverse des racines
roots(varest_1) ###le modèle est stable



#####Analyse des rÃ©sidus

resid(varest_1)  

#normalité
normality.test(varest_1)



###Test de corrélation

#Portmanteau-Tes t 

varserial=serial.test(varest_1, lags.pt=20, type="PT.asymptotic")
varserial
Box.test(resid(varest_1)[,1], lag = 1, type = "Box-Pierce") #Série1
Box.test(resid(varest_1)[,2], lag = 1, type = "Box-Pierce") #Série2


plot(varserial, names="y1")

plot(varserial, names="y2")


#test d'homoscedasticité 
Arch<-arch.test(varest_1, lags.single = 10, multivariate.only = TRUE)

Arch

## Test de causalité

causality(varest_1,cause = "y1")
causality(varest_1,cause = "y2")

#fonctions impulsionnelles
impul1 <- irf(varest_1, reponse="y1" , impulse = c("y2"),n.ahead =6, boot = TRUE)
plot(impul1)


##### Prédiction 

predict(varest_1, n.ahead=8, ci=0.95) 
predictions<-predict(varest_1, n.ahead=5, ci=0.95) 
class(predictions)

plot(predictions, names="y1")
plot(predictions, names="y2")


fanchart(predictions)

 




