*EXERCICE 1;
*Importer les fichiers;
libname dir "/home/u61991785/sasuser.v94"

* Fichier 1;
proc import file="/home/u61991785/sasuser.v94/MAGA1.txt"
    out=MAGA1
    dbms=tab;
run;

*Fichier 2;
proc import file="/home/u61991785/sasuser.v94/MAGA2.txt"
    out=MAGA2
    dbms=tab;
run;
* 1.  Réunissez les tables SAS associées à ces deux fichiers en utilisant l’instruction MERGE.;
data tab1;
merge MAGA1 MAGA2;
run;

* 2.  Utilisez la fonction MDY afin de remplacer les colonnes jours-mois-années par une seule colonne
Date qui définit les dates associées. N’oubliez pas de choisir un FORMAT de type date de votre
choix pour la variable Date;
* Pour la MAGA1;
data tab2;
set MAGA1;
   Date1=mdy(Mois,Jours,Annees);
  FORMAT Date1 date9.;
run;

*Pour la MAGA2;
data tab3;
set MAGA2;
   Date2=mdy(Mois,Jours,Annees);
  FORMAT Date2 date9.;
run;

*  Refaites la question 1 en utilisant cette fois-ci les tables que vous avez de la question 2;
data tab4;
merge tab2 tab3;

run;


* EXERCICE 2;
*1. Créez la table Separate qui contient que les variables Last, First et MI. Porter une attention aux
blancs de début et de fin de longueur des différentes variables;

/*créer la table people*/
data People;
    input name $25.;
    datalines;
Andy_Lincoln_Bernard
Barry_Michael_Providence
Chad_Simpson_Smith
Derrick_Parson_Henry
Eric_Miller_Luc
Frank_Giovanni_Goodwill
;

run;

/*afficher la table people*/
proc print data=People;
run;
/*création de la table separate*/
data Separate;
    set People;
    Last=scan(name, 1, '_');
    First=scan(name, 2, '_');
    MI=scan(name, 3, '_');
run;
 
/*afficher la table separate*/
proc print data=Separate;
run;

/*2. Utilisez l’étape DATA précédente pour créer une table Flname qui contient que les variables
NewName et CityState. Les valeurs de NewName devraient être la concaténation du prénom et
du nom de famille de chaque personne avec un espace entre chaque*/
DATA Flname;
    set Separate;
    NewName = CATX(" ",Last,First);
         input  CityState $;
         datalines;
         New_York
         Paris
         Dakar
         NDjamena
         ;
         drop name Last First MI;
RUN;

/*3. Créez la table Init qui contient que les variables Name, Initials et CityState. Les valeurs de Initials
devraient être la concaténation du premier caractère du prénom, de l’initiale et du nom de famille
de chaque personne sans aucun délimiteur entre les caractères*/    
data Init;
    set FlName;
    Initials = CATS(substr(First, 1, 1),substr(MI, 1, 1),substr(Last, 1, 1));
    drop Last First MI NewName;
run;

* EXERCICE 3;
/* Creation de la table Adresses avec contant*/
data Adresses;
input contant $25.;
datalines;
Erica_25_Ruelle_110_Rennes
Fleur_27_Ruela_111_Caens
Jacob_78_Ruems_120_Lille
;

run;
/*Nom doit prendre le
nom votre contact, Numero, le numéro de la rue dans laquelle il habite, RUE, le nom de la rue, CP, le
code postale et Ville le nom de la ville.*/

data Adress;
set Adresses;
 Nom=scan(contant, 1, '_');
    Numero=scan(contant, 2, '_');
    RUE=scan(contant, 3, '_');
    CP=scan(contant, 4, '_');
    Ville=scan(contant, 5, '_');
    run;
* EXERCICE 4;
/*Créer la table Final à partir de la table Grade qui contient une nouvelle variable Overall, moyenne de
tous les tests intermédiaires et du test final, le test final valant 2 fois plus que tous les autres. Arrondissez
la moyenne à l’entier le plus près.*/

* Création de la table Grade;
DATA Grade;
INPUT test1 test2 test3 test4 test_final;
cards ;
12 11 14 16 15
10 15 17 19 16
19 18 17 16 17
20 19 13 12 20
;
RUN;
* Calcul de la moyenne Overall;
DATA Final;
SET Grade;
Overall= round((test1 + test2 + test3 + test4 + 2*test_final)/6);
RUN;

*EXERCICE 5;
/* Chargement de la base Noday*/
proc import file="/home/u61991785/sasuser.v94/MAGA3.txt"
    out=Noday
    dbms=tab;
run;

/*Utilisez la table Noday pour créer la table Emphire qui contient une nouvelle variable Hired, date
d’embauche de chaque employé. Supposez que chaque employé a été embauché le 15 de mois. Hired
doit être affichée au format DATE9.
*/
DATA Emphire;
   SET Noday;
  Hired= mdy(Mois,Jours,Annee);
   Format Hired date9.;
   TODAY=today();
      Format TODAY date9.;
   Years=int(yrdif(Hired,TODAY , 'act/ACT'));
   drop Mois Jours Annee TODAY;
RUN;

/*Travaux Dirigés 4 */

/*Exercice 1*/
/* 1. Utilisez PROC MEANS afin de construire une table SAS dans laquelle vous disposerez du
montant commandé auprès de chaque fournisseur */
proc means data= sasuser.fournisseur noprint SUM nonobs;
class fournisseur;
var achat ;
output out=Montant SUM=achat;
run;
/*Afficher la table*/
proc print data=Montant;
run;
/* 2. Ajoutez à cette table la part que présente chaque fournisseur dans les achats de cette entreprise*/

proc tabulate data=sasuser.fournisseur;
class fournisseur;
var achat ;
table ALL (achat)*(SUM MEAN PCTSUM),(fournisseur ALL)
;

run;

/**3. Le directeur de cette entreprise souhaite connaître le prix moyen des produits achetés
en 2013. Une simple moyenne sur la variable PRIX ne donnera pas un résultat correct
puisqu’il faudrait pondérer par les quantités;*/
proc means data= sasuser.fournisseur  MEAN nonobs;
var Prix/WEIGHT= quantite;
output out=Moyenne_Prix;
run;

/* 4. Construisez une table SAS dans laquelle vous présentez les achats effectués chaque mois
par cette entreprise*/
/*création de la variable mois*/
data Mois;
set sasuser.fournisseur;
month=month(date);
run;
/* Calculs du montant par Mois*/
proc means data= Mois  SUM nonobs;
class month;
var achat ;
output out=Mois_Achat;
run;

/* 5.Au moyen de PROC MEANS, construisez une table SAS vous indiquant le nombre de
commandes effectuées, le nombre d’unités commandées et le montant total des achats
effectués pour l’année 2013 et pour chaque référence. ;*/
proc means data= sasuser.fournisseur sum;
class reference;
var quantite achat/ weight=quantite;
output out=Stat ;
run;

* EXERCICE 2;
* 1.Utilisez PROC FREQ afin de construire un tableau croisé donnant, pour chaque fournisseur
et chaque mois, le nombre de commandes effectuées;
proc freq data = Mois;
tables  reference*month/nocol norow nopercent;
run;

/* 2. Á l’intérieur de ce tableau, vous indiquerez le nombre de commande effectuées, 
ainsi que les montants de ces commandes*/

proc tabulate data=Mois;
class month fournisseur;
var achat ;
table ALL ( month*achat)*(N sum), (fournisseur ALL)
;
run;

/*3. Á l’intérieur du tableau, exprimez les
montants moyens commandés, ainsi que la part de chaque fournisseur pour les achats du
mois.*/
proc tabulate data=Mois;
class month fournisseur;
var achat ;
table ALL ( month*achat)*(SUM COLPCTSUM),(fournisseur ALL)
;

run;

/*4.Estimez un modèle linéaire de régression de la quantité commande en fonction du prix
unitaire*/

proc Reg data=sasuser.fournisseur;
	title "Regression linéaire";
	model quantite = prix;
	run;


