library(tidyverse)
library(ggpubr)
library(rstatix)
#### Importation de la base
library(readxl)
Base_Essai <- read_excel("C:/Users/lenovo/Desktop/Classeur1.xlsx")
View(Base_Essai)
##### Donn?es de l'ITRAD

##### Recuperation des variables de la base
Bloc<-Base_Essai$Bloc
Variete<-Base_Essai$Variete
Maturite<-Base_Essai$Maturite
Rendement<-Base_Essai$Rendement
Hauteur<-Base_Essai$Hauteur_m
Poids_g<-Base_Essai$Poids_grain
Nbre_epis<-Base_Essai$Nbre_epis
Poids_1000<-Base_Essai$Pois_1000
#### On les convertis en de facteurs
Bloc<-factor(Bloc, ordered=F)
Variete<-factor(Variete, ordered=F)
Maturite<-factor(Maturite, ordered=F)
table(Maturite)
#### On fabrique la nouvelle base
BAZ<-data.frame(Bloc, Variete, Maturite, Rendement,Hauteur, Nbre_epis, Poids_g, Poids_1000)
str(BAZ)
head(BAZ)
summary(BAZ)
par(mfrow=c(1,1))
######## Etude graphique ###################
boxplot(Rendement~Bloc, col="green", main="Rendement suivant les differents blocs")
boxplot(Rendement~Maturite, col="blue", main="Rendement suivant le nombre de jours de maturit?")
boxplot(Rendement~Variete, col="yellow", main="Rendement suivant les differentes vari?t?s")
boxplot(Poids_g)
#### Avec le package ggplot2
library(ggplot2)
a <- ggplot(BAZ, aes(x = Rendement))
#### Rendement dans les blocs
e <- ggplot(BAZ, aes(x = Bloc, y = Rendement))
e + geom_boxplot(aes(fill = Bloc))
e2 <- e + geom_boxplot(aes(color = Bloc)) + theme_minimal()
e2
e3 <- e + geom_boxplot(aes(fill = Bloc)) + theme_minimal()
e3
e + geom_violin(aes(fill = Bloc)) + theme_minimal()
# Add mean or median point: use fun.y = mean or fun.y = median
e + geom_violin(aes(fill = Bloc), trim = FALSE) +
  stat_summary(fun= mean, geom = "point",
               shape = 23, size = 2, color = "red")+ theme_minimal() +geom_boxplot(width = 0.2)

e + geom_violin(aes(fill = Bloc), trim = FALSE) + geom_dotplot(binaxis='y', stackdir='center') +
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
               geom="pointrange", color = "red")
e3 <- e + geom_jitter(aes(color = Bloc), position = position_jitter(0.2)) +
  theme_minimal()
e3
p <- ggplot(data=BAZ, aes(x=Bloc, y=Rendement, group=2))
# Basic line plot with points
p + geom_line() + geom_point()

# Change line type and color
p + geom_line(linetype = "dashed", color = "steelblue")+
  geom_point(color = "steelblue")

f <- ggplot(BAZ, aes(x = Bloc, y = Rendement))
f1 <- ggplot(BAZ, aes(x = Variete, y = Rendement))

# Basic bar plot
f + geom_bar(stat = "identity") + theme_minimal()
# Change bar plot fill colors by groups
f + geom_bar(aes(fill = Bloc), stat="identity")+ theme_minimal()
f + geom_bar(aes(fill = Maturite), stat="identity")+ theme_minimal()
f1 + geom_bar(aes(fill = Bloc), stat="identity")+ theme_minimal()

#e + geom_boxplot(aes(fill = Maturite))
g <- ggplot(data=BAZ, aes(x=Bloc, y=Rendement, fill=Maturite))
# Stacked bar plot
g + geom_bar(stat = "identity")
# Use position=position_dodge()
g + geom_bar(stat="identity", position=position_dodge())+ theme_minimal()
#plot_ly(Base_Essai, x=~Bloc, y=~Rendement, type="box")

##### Valeurs abberantes
BAZ %>% 
  group_by(Bloc) %>%
  identify_outliers(Rendement)
######### Mod?le ANOVA ? trois facteurs avec interaction#########
lm.out<-aov(Rendement~Maturite*Bloc + Variete,data=BAZ)

anov<-anova(lm.out)
a<-lm.out$residuals
#Anova(lm.out)
summary(lm.out)
plot(lm.out)
plot(anov)
### Normalité ANOVA
# normalité de Shapiro-Wilk
shapiro_test(residuals(lm.out))
### par BLOC
BAZ %>% 
  group_by(Bloc) %>%
  shapiro_test(Rendement)
### par BLOC
  levene_test(Rendement, BAZ)
### Heteroscedascit? des r?sidus
bartlett.test(a) ### Mod?le heteroscedastique
leveneTest(Rendement,Maturite)
leveneTest(Rendement,Bloc)
leveneTest(Rendement,Variete)
### Tukey Methodes
par(las=1) # ?criture horizontale des ?tiquettes.
plot(TukeyHSD(anov))
TukeyHSD(anov)
##Bonferroni
pairwise.t.test(Rendement,Variete,p.adjust="bonf")

#### Mod?le lin?aire a effets mixtes ###############
require(lmerTest)
require(lme4)
require(nlme)
require(phia)
#
lmer.out<-lmer(Rendement~Variete + Bloc + (1|Maturite:Bloc), REML=F)

#print(lmer.out, correlation=TRUE)
#### Effets fixes
anova(lmer.out)
### Effets al?atoires
ranova(lmer.out)
#F-tests of 'single term deletions' for all marginal terms:
drop1(lmer.out)
### Estimation des coefficients
summary(lmer.out)

interaction.plot(Variete, Maturite,Rendement, type=c("b"), col=c("blue", "red", "black", "green","purple", "orange", "pink"))
interaction.plot(Maturite,Bloc,Rendement, type=c("b") )
means<-interactionMeans(lmer.out) 
means
plot(means, abbrev.levels=F)
testInteractions(lmer.out)
testInteractions(lmer.out, adjustment="none")
#testInteractions(lmer.out, adjustment="none", pairwise="Variete", fixed="Bloc")
#cont1<-list() 
#cont1$Variete<-cont 
#cont1
#testInteractions(lmer.out, adjustment="none", custom=Variete, fixed="Dose")
require("lattice") 
dotplot(ranef(lmer.out, condVar=T))
st<-step(lmer.out) 

plot(st$fixed)
#library(lmerTest)
#lmerTest::anova(lmer.out, type=1)
ranef(lmer.out)
#coef(lmer.out)
aleatoires<-lmer(Rendement~1+(1|Maturite), data=BAZ)
pr01 <- profile(aleatoires) 
xyplot(pr01, aspect = 1.3, layout=c(4,1))
xyplot(pr01, aspect = 1.3, layout=c(3,1), absVal=T)
splom(pr01)





####### ANALYSE EXPLORATOIRE ##############
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
##### Lin?arit? Rendement et Poids_g
ggscatter(
  BAZ, x = "Rendement", y = "Poids_g",
  color = "Bloc", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Bloc)
  )
#### Lin?arit? Rendement et Nombre epis
ggscatter(
  BAZ, x = "Rendement", y = "Nbre_epis",
  color = "Bloc", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Bloc)
  )

########## Matrice de corr?lation
mydata<-data.frame(Rendement, Nbre_epis, Poids_1000, Poids_g, Hauteur)
#install.packages("corrplot")
library(corrplot)
rquery.cormat(mydata, type="full")
v=cor(mydata)
v

######### ANCOVA, ? un facteur avec Nbre_epis comme covariable ############
anova(lm(Rendement ~ Variete*Nbre_epis + Bloc+ Maturite))
summary(lm(Rendement ~ Variete*Nbre_epis + Bloc+ Maturite))
plot(aov(Rendement ~ Variete*Nbre_epis + Bloc+ Maturite))
######## Regression lin?aire ###########
mod<-lm(Rendement ~ Nbre_epis + Poids_1000 + Poids_g)
anova(mod)
summary(mod) ### Poids grain explique le Rendement

#### Coefficient de variation
cv <- (sd(Rendement) / mean(Rendement) )
print(cv)

#########Bonferroni ########
library(emmeans)
pwc <- BAZ %>% emmeans_test(
    Rendement ~ Bloc, covariate = Poids_g,
    p.adjust.method = "bonferroni"
  )
pwc

ggscatter(
  BAZ, x = "Nbre_epis", y = "Rendement",
  facet.by  = c("Bloc", "Maturite"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)

###### Lin?arit? Rendement Maturit?
BAZ %>%
  anova_test(
    Rendement ~ Nbre_epis + Bloc + Maturite + 
      Bloc*Maturite + Nbre_epis*Maturite +
      Nbre_epis*Bloc + Nbre_epis*Bloc*Maturite)
  
## {Il y a une relation lin?aire entre la covariable (variable nbre epis) et 
#la variable-r?ponse (Rendement) pour chaque groupe, telle qu'?valu?e par une inspection visuelle du diagramme de dispersion.}

BAZ %>%
  unite(col = "Maturite", Maturite, Bloc) %>%
  anova_test(Rendement ~ Maturite*Nbre_epis)

BAZ %>%
  group_by(Bloc) %>%
  anova_test(Rendement ~ Nbre_epis + Maturite)
#### IMPORTANT#######3
BAZ %>%
  group_by(Variete) %>%
  get_summary_stats(Rendement, type = "full")
####
BAZ %>%
  group_by(Bloc) %>%
  get_summary_stats(Rendement, type = "full")
####
BAZ %>%
  group_by(Maturite) %>%
  get_summary_stats(Rendement, type = "full")
##### Outliers
BAZ %>%
  group_by(Bloc, Variete, Maturite) %>%
  identify_outliers(Rendement)
##### normalit?
BAZ %>%
  group_by(Bloc) %>%
  shapiro_test(Rendement)
#### Normalit? des donn?es
ggqqplot(BAZ, "Rendement", ggtheme = theme_bw()) +
  facet_grid(Maturite~ Bloc)
#### Homogeneit?
BAZ %>%
  group_by(Bloc) %>%
  levene_test(Rendement ~ Maturite)

####
# Test ANOVA ? deux facteurs mixtes
res.aov <- anova_test(
  data = BAZ, dv = Rendement, wid= id,
  between = Bloc, within = Variete
)
get_anova_table(res.aov)

pwc2 <- BAZ %>%
  group_by(Maturite) %>%
  pairwise_t_test(
    Rendement ~ Bloc, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) %>%
  select(-df, -statistic, -p) # Supprimer les d?tails
pwc2
###Il n'y avait pas de valeurs extr?mes aberrantes.

############ Package nlme ##########

lme.ut<-lme(Rendement~ Variete + Bloc, random= ~ 1|Maturite, weights = varIdent(form = ~1 |Maturite), data=BAZ)
par(mfrow=c(1,1))
summary(lme.ut)
plot(lme.ut$groups)
print(lme.ut$dims)


### ESTIMATION DES PARAMETRES
DAF<-anova(lme.ut)
DAF ### Les resulats montrent que les facteurs Bloc et Variete ont des effets significatifs sur le rendement
 #
##### Les tests de significativit?
t.test(random.effects(lme.ut))
testFactors(lme.ut)
bartlett.test(DAF)
require("lattice") 
dotplot(ranef(lme.ut, condVar=T))
### Heteroscedascit? des r?sidus
leveneTest(Rendement,Maturite)
leveneTest(Rendement,Bloc)
leveneTest(Rendement,Variete)
lme.ut$fixDF

############# V?rification des hypoth?ses ###########
resid(lme.ut[])
residus<-residuals(lme.ut, type="normalized") #Puisque nous avons normalis?, nous n'avons pas de sous-groupes ici.
residus[1:10]
### Normalit?
BAZ$Residus<-residus
require(nortest)
#.normalite(data=BAZ, X="residus", Y=NULL)
shapiro.test(residus) ### On ne rejette pas la normalite
qqnorm(residus)
### On teste la distribution de la variable al?atoire
aleatoires<-lmer(Rendement~1+(1|Maturite), data=BAZ) 
pr01<- profile(aleatoires) 
xyplot(pr01, aspect = 1.3, layout=c(3,1))
xyplot(pr01, aspect = 1.3, layout=c(3,1), absVal=T)

#### Ind?pendance entre le facteur al?atoire et le r?sidu
splom(pr01)

#### significativit? du facteur al?atoire
require("lattice") 
dotplot(random.effects(lme.ut, condVar=T))

library(lmerTest) 
st<-step(lme.ut) 
st


############ VERIFICATIONS HYPOTHESIS################

residual<-residuals(lme.ut, type="normalized")
View(residual)
BAZ$residual<-residual
View(BAZ)

require(nortest)

.normalite(data=BAZ, X="residual", Y=NULL)

shapiro_test(residual)

#### Normalité de l'effet aléatoire
aleatoires<-lmer(Rendement~1+ (1|Maturite))
pr01 <- profile(aleatoires)
xyplot(pr01, aspect = 1.3, layout=c(3,1))

## permet d'observer de manière plus efficace les intervalles de confiance
xyplot(pr01, aspect = 1.3, layout=c(3,1), absVal=T)

### Indépendance entre residu et effet aléatoire
shapiro.test(pr01$.zeta) ### effet aléatoire non normal
shapiro.test(pr01$.sigma)### residu non normal

a<-cbind(pr01$.sig01, pr01$.sigma, pr01$`(Intercept)`)
colnames(a)<-c("alea", "residu", "constante")
a
scatterplotMatrix(a)
splom(~pr01, groups=Maturite, data=BAZ, panel=panel.superpose, aspect=1, layout=c(1,1))
splom(~pr01)
############
library(HLMdiag)
cd<-case_delete(lme.ut, group="Bloc", type="both")
## mesure de la suppression d'observations.
cd1<-hlm_influence(lme.ut) # on stocke dans l'objet cd1 les résultats des indicateurs diagnostics.
# Pour obtenir les mesures sur les effets fixes :
head(cd1[["fixef_diag"]]) # head per

dotplot_diag(x = cd1$cooksd, data=cd1[3:10],cutoff="internal",name="mdffits",
             modify="dotplot",ylab = "Cooks Distance")


View(BAZ)
dotplot_diag(x = cd1$mdffits, data=cd1[3:10],cutoff="internal",name="mdffits",
             modify="dotplot",ylab = "MDFFITS")

### Modèle mixte meileur qu'un modèle classique#########
GLS <- gls(Rendement ~ Variete + Bloc, data = BAZ, method = "REML")

anova(GLS, lme.ut)


lmer<-lmer(Rendement~(1|Maturite), data=BAZ)
rand(lmer)

outlier_HWY <- boxplot.stats(Rendement)$out
outlier_HWY




lm<-lmer(Rendement~1|Maturite)

anova(lm)
library(lmerTest)
st<-step(lm)

#### prediction des effets aléatoires
alea= random.effects(lme.ut)

alea$`(Intercept)`
plot(alea)

plot(lme.ut$fitted)

lme.ut$varFix

library(lmtest)
###### Hétéroscedasticité################
leveneTest(residuals(lme.ut)~Maturite)


leveneTest(residus~Maturite)

leveneTest(residuals(lmer.out)~Maturite)

lme.out<-lme(Rendement~ Variete + Bloc, random= ~ 1|Maturite, data=BAZ)

anova(lme.out, lme.ut)

plot(st)
x<-rnorm(10000, 100,15) 
r <- c(40,160) 
hist(x,freq=FALSE,col=0,ylim=c(0,0.05),20) 
plot(function(x) dnorm(x,100,15),xlim=r,col=3,add=TRUE)

require("TeachingDemos") 
ci.examp()

x=c(1,2, 2,2,6,6,7,7,3,3,1,1,8,8,9)

y<-numeric(length(x))
for (i in 1:length(x))
{
 if (x[i]==6)
   y[i]=0
  else
    y[i]=1
}
y

anova(lme.ut, lm.out)

