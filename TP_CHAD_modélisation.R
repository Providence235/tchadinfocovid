


########################## CHAPITRE 1: MODELISATION DE LA CROISSANCE DU PIB ########################
require(graphics)
library(ggplot2)
library(plotly)
library(readxl)
library(tseries)
## Importation de la base

chad_dta_covid2 <- read_excel("C:/Users/lenovo/Desktop/chad_dta_covid2.xlsx")
View(chad_dta_covid2)
####### Recuperation de la donnée croissance
dim(chad_dta_covid2)
Data<-chad_dta_covid2$CrPIB

##### Configuration de l'indice temporelle à partir de 1989
D1<-ts(Data, start=c(1993,1), end=2019)
D1

### Test de stationnarité
adf.test(D1, k=1)
pp.test(D1)
kpss.test(D1)

### Première différence
D2=diff(D1)

adf.test(D2, k=1)
pp.test(D2)
kpss.test(D2)
### Détermination de l'ordre
par(mfrow=c(1,1))
acf(D2, main="Croissance différenciée stationnaire") # q=0,1
pacf(D2, main="Croissance différenciée stationnaire") ## p=0,1, 2

##### Estimation des modèles
estima1<-arima(D2, order=c(0,1,1)) ## éliminé
estima2<-arima(D2, order=c(1,1,0)) ## éliminé
estima3<-arima(D2, order=c(1,1,1)) ### BON
estima4<-arima(D2, order=c(2,1,0)) ## BON
estima5<-arima(D2, order=c(2,1,1)) ## BON, Retenu finalement


### Significativité des modèles
abs(estima1$coef)>1.96*sqrt(diag(estima1$var.coef)) ## 
abs(estima2$coef)>1.96*sqrt(diag(estima2$var.coef)) ## 
abs(estima3$coef)>1.96*sqrt(diag(estima3$var.coef)) ## Signif
abs(estima4$coef)>1.96*sqrt(diag(estima4$var.coef)) ## Signif
abs(estima5$coef)>1.96*sqrt(diag(estima5$var.coef)) ## Signif

### Recuperation des résidus
res1=estima1$residuals
res2=estima2$residuals
res3=estima3$residuals
res4=estima4$residuals
res5=estima5$residuals

### Dianostic des modèles
tsdiag(estima1)
tsdiag(estima2)
tsdiag(estima3)
tsdiag(estima4)
tsdiag(estima5)

### Normalité des residus

jarque.bera.test(res1)
jarque.bera.test(res2)
jarque.bera.test(res3)
jarque.bera.test(res4)
jarque.bera.test(res5)

#### Critères de selection
BIC3=log(estima1$sigma2)+(1+1)*log(length(res1))/length(res1)
BIC3
BIC4=log(estima2$sigma2)+(2+0)*log(length(res2))/length(res2)
BIC4
BIC5=log(estima3$sigma2)+(2+1)*log(length(res3))/length(res3)
BIC5

### Representation des residus
ts.plot(res5) ## Representation des residus
y=density(res5)
plot(y, main="Résidus du modèle lauréat")#, type="l", lty=c(3,1))

#superpose densité théorique

hist(res5, breaks=30, col="pink")

z=seq(min(res5), max(res5), 0.01)

lines(z, dnorm(z,0,1), lty=3, col="blue")

curve(dnorm(x,mean=mean(res5), sd=sd(res5)), add=T, col="red")

#### Test de Box-Pierce
Box.test(res5, lag = 1, type = c("Box-Pierce", "Ljung-Box"))
#### Test de Ljung-Box
Box.test(res5,type ="Ljung-Box",lag=10) ### On ne rejette pas l'hypothèse nulle donc les aautocorelations ne sont pas significatives

plot(res5, col="red")
abline(a=0, b=0)

### Test de Durbin-Watson
resmoy1=window(res5, start=c(1993,1))
resmoy1

formulaDW=sum((res5-resmoy1)^2)/sum(res1^2)
dwtest(formulaDW)


# Compare graphiquement la distribution d'un échantillon avec une distribution normale 
# Si points alignés, la distribution est normale
qqnorm(res5, main="Résidus du modèle lauréat") #, main="", ylab="")
qqline(res5, col="blue")
ks.test(res5, rnorm)#, distribution="normal") on rejette la normalité

#### Les points ne sont pas alignés donc la distribution n'est pas normale


######## PREVISION
par(mfrow=c(2,2), oma= c(1,1,3,1)) #Oma=c(.,.) Permet de représenter le titre

previ<-predict(estima5, n.ahead=5)
previ
par(mfrow=c(1,1))
#### On recupère les prévisions. Il faut noter que nous avions travaillé avec l'inverse
PRE<-previ$pred
PRE
### On recupère les standard errors
previstd<-previ$se
previstd

####
PREDI<-window(PRE, start=c(2020, 1), units="years", frequency=1)
bornesup<-PREDI+1.96*previstd
borneinf<-PREDI-1.96*previstd

###Représentation graphique d'une année
ts.plot(window(D1, start=c(2019,1)), PREDI, bornesup, borneinf,lty=c(1,3,2,2), main="")
lines(PREDI, col='blue', lty=3)
lines(borneinf, col='red', lty=3) 
lines(bornesup, col='red', lty=3) 
#par(mfrow=c(3,1), oma= c(1,1,3,1)) #Oma=c(.,.) Permet de représenter le titre
mtext("Prédiction du taux de croissance de l'économie Tchadienne", side =3,cex=1, outer =TRUE, font = 2 )

##### Fin




############## CHAPITRE2: MODELISATION MULTIVARIEE ###################

##Installation des packages
library(stats)
library(tseries)
library(vars)
library(stats)
library(grDevices)
library(graphics)
library(forecast)
library(timeSeries)
library(urca)
library(foreign)

####### Chargement de la base
library(readxl)
chad_dta_covid2 <- read_excel("C:/Users/lenovo/Desktop/chad_dta_covid2.xlsx")
 View(chad_dta_covid2)
##### partie var



View(chad_dta_covid2)

#### Recupération des données
Cpib<-chad_dta_covid2$CrPIB
Petrole<-chad_dta_covid2$Oil_rents
Tradec<-chad_dta_covid2$`Trade(%GDP)`
Agriculture<-chad_dta_covid2$Agri_PIB

Cpib<-ts(Cpib, start=c(1989,1))
Cpib
Petrole<-ts(Petrole, start=c(1989, 1))
Petrole
Tradec<-ts(Tradec, start=c(1989,1))
Tradec
Agriculture<-ts(Agriculture, start=c(1989,1))
Agriculture

#### Choix des variables

cor(BASE1)

cor(BASE2)


### Test de stationnarité


## Pétrole
adf.test(Petrole, k=1)
### 1ère Diff
Petrole1=diff(Petrole)
### Tests
adf.test(Petrole1, k=1)
pp.test(Petrole1)
kpss.test(Petrole1)

### Commerce
adf.test(Tradec, k=1)
### 1ère Diff
Trade1=diff(Tradec)
## Tests
adf.test(Trade1, k=1)
pp.test(Trade1)
kpss.test(Trade1)

#### Verification de la stationnarité
adf.test(Cpib, k=1)### Croissance non stationnaire à l'issu des trois tests
pp.test(Cpib)
kpss.test(Cpib)

### 1ère Différence

Cpib1=diff(Cpib) # 1ère diff

adf.test(Cpib1, k=1)### Croissance stationnaire à l'issu des trois tests
pp.test(Cpib1)
kpss.test(Cpib1)

##### Agriculture
adf.test(Agriculture, k=1) ### Agriculture non stationnaire à l'issu des trois tests
pp.test(Agriculture)
kpss.test(Agriculture)

### 1ère Diff
Agriculture1=diff(Agriculture) # 1ère diff

adf.test(Agriculture1, k=1)###  Agriculture stationnaire à l'issu des trois tests
pp.test(Agriculture1)
kpss.test(Agriculture1)


### BASE
BASE<-cbind(Cpib, Agriculture, Tradec, Petrole)
View(BASE)
jotest1=ca.jo(BASE, type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest1) ## 3 des 4 series ne sont pas cointégrées


####### On utilise la base des series différenciées pour le VAR
BASE2<-cbind(Cpib1,Agriculture1, Trade1)
BASE2
dim(BASE2)

### Selection de l'ordre
library(vars)

##### Cpib1, Trade1 et Agriculture1 sont intégrés d'ordre 1

#### Test de cointégration de Hansen
jotest=ca.jo(BASE2, type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)
?ca.jo ### Elles sont cointégrées
###### Création de la base


### Correlation
acf(BASE2,20,"correlation")
### Selection de l'ordre
C2=VARselect(BASE2, lag.max =3)
C2

### On retient l'ordre 2

##Estimation du VAR 

model1<-VAR(BASE2, p=2)

#model2<-VAR(BASE2, p=4)

######## Validation du modÃ¨le
summary(model1)


#ReprÃ©sentation de l'inverse des racines
roots(model1) ###le modèle est stable


?arch.test
#####Analyse des rÃ©sidus

#NormalitÃ©

normality.test(model1)

#AutocorrÃ©lation

serial.test(model1, type="BG")

serial.test(model1, lags.pt = 10, type = "PT.asymptotic")

### Hétéroscedasticité

Arch<-arch.test(model1, lags.single = 10, multivariate.only = TRUE)

Arch
### Le mod
? arch.test

### Pas d'autocorolletion en serie

#Causalité
causality(model1, cause=c("Agriculture1", "Trade1"))### 
causality(model1, cause=c("Cpib1"))###  Causalité unidirectionnelle

### Au seuil de 5%,  On voit bien que l'Agriculture et le commerce causent bel et bien la croissance

plot(model1, names= "Freq")

##### Réaction du taux de croissance face aux chocs simultanés
impul1 <- irf(model1, reponse="Cpib1" , impulse = c("Trade1", "Agriculture1"),n.ahead =6, boot = TRUE)
plot(impul1)

##### Un choc sur l'Agriculture
impul2 <- irf(model1, reponse="Cpib1" , impulse = c("Agriculture1"), n.ahead =6, boot = TRUE, col="blue")
plot(impul2)

##### Un choc sur le commerce
impul3 <- irf(model1, reponse="Cpib1" , impulse = c("Trade1"), n.ahead =6, boot = TRUE, col="blue")
plot(impul3)


#plot(impul1$Lower)

### Test for structural breaks residuals
stable1<-stability(model1, type="OLS-CUSUM")

plot(stable1)

?fevd

#### Nous avons des series stables vue qu'elles evoluent au sein de leur IC
#DÃ©composition de la variance
fevd.Freq1 <- fevd(model1, n.ahead=5)
plot(fevd.Freq1)
table(fevd.Freq1)

#PREVISION

prevision<-predict(model1, n.ahead=6)

prevision$fcst<-ts(prevision$fcst, start=c(2021,1))

prevision$fcst$Cpib1

#ReprÃ©sentation graphique
plot(prevision, main="Prévisions des séries Cpib1, Agriculture1 et Trade1")$Freq
fanchart(prevision, color=NULL, main="Prévisions des séries Cpib1, Agriculture1 et Trade1")


##### Tests de stationnarité des résidus pour passer à un VECM
Resid=residuals(model1)

adf.test(Resid[,1], k=1) ## Résidus du modèle stationnaire
adf.test(Resid[,2], k=1) ## Non stationnaires suivant ADF mais stationnaires suivant DF simple
adf.test(Resid[,3], k=1) ## Résidus du modèle stationnaire

pp.test(Resid[,1]) ## Résidus du modèle stationnaire
pp.test(Resid[,2]) ## Résidus du modèle stationnaire
pp.test(Resid[,3]) ## Résidus du modèle stationnaire

kpss.test(Resid[,1]) ## Résidus du modèle stationnaire
kpss.test(Resid[,2]) ## Résidus du modèle stationnaire
kpss.test(Resid[,3]) ## Résidus du modèle stationnaire

#### Les residus du modèle sont stationnaires donc il serait interessant de passer au VECM


Acoef(model1) #### Coefficient estimé

Bcoef(model1)

Cpib2<-window(Cpib1, start=c(1992,1), end=2020)
Agriculture2<-window(Agriculture1, start=c(1992,1), end=2020)
Trade2<-window(Trade1, start=c(1992,1), end=2020)


length(Cpib2)
#### Fiiting
fitt=fitted(model1)
length(fitt[,1])

par(mfrow=c(3,1))
ts.plot(fitt[,1], Cpib2,ylab="Growth and fitting growth", col=c("blue", "red"), main="Fitting for Growth")
ts.plot(fitt[,2],Agriculture2, ylab="Agriculture and fitting Agriculture", col=c("green", "blue"), main="Fitting for Agriculture")
ts.plot(fitt[,3], Trade2, ylab="Trade and fitting Trade",col=c("yellow", "blue"), main="Fitting for Trade")


######## CHAPITRE3: MODELE VECM #######

#### Base sur des series initiales non différenciées
BASE1<-cbind(Cpib,Agriculture,Tradec, Petrole)
BASE1

#### Test de cointégration de Hansen
jotest=ca.jo(BASE1, type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest) ## 3 des 4 series ne sont pas cointégrées
### Chargement de la librairie
library(tsDyn)

#### Estiimation du modèle
model2<-VECM(BASE1, lag=1, r=3, estim="ML")

#toLatex(model2)

summary(model2)

##### Réaction du taux de croissance face aux chocs simultanés
impul1 <- irf(model2, reponse="Cpib" , impulse = c("Tradec", "Agriculture", "Petrole"),n.ahead =6, boot = TRUE)
plot(impul1)

##### Un choc sur l'Agriculture
impul2 <- irf(model2, reponse="Cpib" , impulse = c("Agriculture"), n.ahead =6, boot = TRUE, col="blue")
plot(impul2)

##### Un choc sur le commerce
impul3 <- irf(model2, reponse="Cpib" , impulse = c("Tradec"), n.ahead =6, boot = TRUE, col="blue")
plot(impul3)

##### Un choc sur le Pétrole
impul4 <- irf(model2, reponse="Cpib" , impulse = c("Agriculture"), n.ahead =6, boot = TRUE, col="blue")
plot(impul4)

#DÃ©composition de la variance
fevd.Freq2 <- fevd(model2, n.ahead=5)
plot(fevd.Freq2)
table(fevd.Freq2)

#PREVISION
prevision2<-predict(model2, n.ahead=5)

prevision2<-ts(prevision2, start=c(2021,1))

prevision2
### Ce modèle donne des prévisions beaucoup plus fiables car il prevoit en 2021
## un taux de croissance
par(mfrow=c(2,2))
ts.plot(prevision2[,1], col="blue", ylab="Growth", main="Avec VECM")

ts.plot(prevision2[,2], col="green", ylab="Agriculture",main="Avec VECM" )

ts.plot(prevision2[,3], col="red", ylab="Trade",main="Avec VECM")

ts.plot(prevision2[,4], col="black", ylab="Petrole", main="Avec VECM")



