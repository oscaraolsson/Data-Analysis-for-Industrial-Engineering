### ----------------------------------------------------------------------------
### Script du cours d'Analyse de données pour le Génie Industriel
### Grenoble INP - Génie Industriel, 2A-ICL, 2017-2018
### (c) P. Lemaire, 2016, 2017, 2018. Ne pas diffuser.
###
### encoding: utf8 (use File>Reopen with encoding in RStudio)

### TP Noté 1

### Pour toutes les questions, n'hésitez pas à écrire des fonctions
### qui vous permettent de ré-utiliser tout ou partie de ce que vous
### faîtes. Vous constaterez que beaucoup de questions se répètent
### pour différents modèles : ne faîtes qu'une fois le travail, puis
### ré-utilisez le à bon escient !

### Les questions doivent avoir une réponse explicite : selon le cas,
### ce sera un script commenté, un résultat numérique commenté, un
### graphique légendé...


### NOMS =====================================================================
# NOM1 : Oscar Olsson
# NOM2 : Samuele Vannoli

### Dans cet exercice, on s'intéresse à la valeur des habitations de
### différentes régions en fonction de différentes
### caractéristiques. Nous allons produire et étudier différents
### modèles de régression linéaires ; parmi les usages possibles de
### tels modèles on peut envisager de chercher les déterminants de la
### valeur (variables permettant de caractériser la valeur, sans que
### cela implique une causalité), ou anticiper la valeur des
### habitations pour d'autres régions similaires.

### Le jeu de données est issu de UCI Machine Learning Repository
### (http://archive.ics.uci.edu/ml/machine-learning-databases/housing/).
### La version utilisée est une adaptation est doit être récupérée sur
### Chamilo.
### 
### Le jeu de données comporte 506 observations et 15 variables décrites ainsi :
###  0. AREA     index of the town in the database
###  1. CRIM     per capita crime rate by town
###  2. ZN       proportion of residential land zoned for lots over 25,000 sq.ft.
###  3. INDUS    proportion of non-retail business acres per town
###  4. CHAS     Charles River dummy variable (= 1 if tract bounds 
###                                            river; 0 otherwise)
###  5. NOX      nitric oxides concentration (parts per 10 million)
###  6. RM       average number of rooms per dwelling
###  7. AGE      age of owner-occupied units, from 1 (newest) to 5 (oldest)
###  8. DIS      weighted distances to five Boston employment centres
###  9. RAD      index of accessibility to radial highways
### 10. TAX      full-value property-tax rate per $10,000
### 11. PTRATIO  pupil-teacher ratio by town
### 12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks 
###              by town
### 13. LSTAT    % lower status of the population
### 14. MEDV     Median value of owner-occupied homes in $1000's

### La variable à expliquer est MEDV.

### Partie 1 : lecture des données ===========================================

## Q1.1 ----------------------------------------------------------------------
## Chargez le jeu de données 'housing' (fichier housing.dat, sur chamilo)
d<- read.delim("hous.dat", sep=";")

## Q1.2 ----------------------------------------------------------------------
## Afin de se familiariser avec les données, affichez le résumé du jeu
## de données et vérifiez la cohérence des valeurs et des
## variables. Tracez les graphiques de chaque couple variables.
plot(d) ##tracing the graph we found that it looks reasonalbe
str(d) ##Data types are correct 


## Q1.3 ----------------------------------------------------------------------
## Sur la base de ces quelques graphiques, quelle variable utiliseriez-vous 
## pour faire une régression linéaire simple de MEDV ? 

#Frome the graphs we can tell that MEDV looks to have the most linnear correlation with RM,
##and logically number of rooms and house prices are reasonable to be highly correlated.
LINMEDVRM<- lm(d$MEDV ~ d$RM)
plot(d$RM,d$MEDV)
abline(LINMEDVRM)

## Q1.4 (bonus, technique) ---------------------------------------------------
## Proposez une fonction qui prend en entrées : un jeu de données et
## un seuil s, et affiche pour chaque variable v la liste des autres
## variables corrélées à v (en valeur absolue) au moins au seuil s.
## Bonus : 
##  - en option, la liste des variables sera triée par corrélation
##    (absolue) décroissante
##  - en option, la corrélation sera indiquée après chaque variable 
## Remarque : 
##  - ne prendre en compte que les attrinuts numériques (is.numeric)
my.data.cor <- function(d, s, tri=FALSE, valeurs=FALSE){
  d_numeric <- d[,sapply(data, is.numeric)]
  corelationmatrix <- cor(d_numeric)
  uppertri <- upper.tri(d_numeric)
  corelationtable <- data.frame(
    row= rownames(corelationmatrix)[row(corelationmatrix)[uppertri]],
    column = rownames(corelationmatrix)[col(corelationmatrix)[uppertri]],
    corelation = abs((corelationmatrix)[uppertri])
  )
  if (tri == TRUE){
    corelationtable <- corelationtable[order(-corelationtable$corelation),]
  }
  filteredcorelationtable <- corelationtable[corelationtable$corelation >= s,]
  return(filteredcorelationtable)
  }
  
  my.data.cor(d, 0.1,TRUE)

## Q1.5 ----------------------------------------------------------------------
## En utilisant la fonction de la question Q1.4, ou tout autre moyen,
## déterminez les variables les mieux corrélées à MEDV.
myr <- function(input) {
  C =(sum((d$MEDV-mean(d$MEDV))*(input-mean(input))))/length(d$MEDV)
  S1=sqrt(var(d$MEDV))
  S2=sqrt(var(input))
  r=C/(S1*S2)
}
RLSTAT <- myr(d$LSTAT)
RB <- myr(d$B)
RPTRATIO <-  myr(d$PTRATIO)
RTAX <- myr(d$TAX)
RRAD <- myr(d$RAD)
RDIS <- myr(d$DIS)
RAGE <- myr(d$DIS)
RRM <- myr(d$RM)
RNOX <-  myr(d$NOX)
RCHAS <-  myr(d$CHAS)
RINDUS <-  myr(d$INDUS)
RZN <-  myr(d$ZN)
RCRIM <-  myr(d$CRIM)

RVector <- c(RLSTAT,RB,RPTRATIO,RTAX,RRAD,RDIS,RAGE,RRM,RNOX,RCHAS,RINDUS,RZN,RCRIM)
plot(RVector)
## the two best are RM and LSTAT which are both around + resp -0,7, 
##the others are between 0,2-0,4 and -0,4- -0,2 

##
### Partie 2 : régression linéaire simple ====================================

### Dans un premier temps, on va s'intéresser à des modèles de MDEV en
### fonction de pourcentage classe populaire (LSTAT) ; en effet,
### graphiquement, une liaison assez forte apparaît (Q1.3) et elle est
### bien corrélée (Q1.5).  (La variable RM aurait été, sur ces
### critères, un choix aussi acceptable).

## Q2.1 ----------------------------------------------------------------------
## Tracez le graphique de la valeur de l'habitation selon la
## proportion de classe populaire
plot(d$MEDV, d$LSTAT)
plot(d$RM, d$LSTAT)


## Q2.2 ----------------------------------------------------------------------
## Calculez la régression linéaire MEDV ~ LSTAT.
## Ajoutez la droite correspondante au graphique précédent.
LINMEDVLSTAT<- lm(d$MEDV ~ d$LSTAT)
plot(d$LSTAT,d$MEDV)
abline(LINMEDVLSTAT)


## Q2.3 ----------------------------------------------------------------------
## Déterminez le coefficient de détermination de la régression linéaire. 
sst<-function(y) sum((y-mean(y))^2)/length(y)
sse<-function(x,y) sum((predict(lm(y~x), newdata = as.data.frame(x))-mean(y))^2)/length(y)
ssr<-function(x,y) sum((y-predict(lm(y~x), newdata = as.data.frame(x)))^2)/length(x)

sst <- sst(d$MEDV)
sse <- sse(d$LSTAT, d$MEDV)
Rsquare <- sse/sst

## Q2.4 ----------------------------------------------------------------------
## Calculez la corrélation linéaire, ainsi que la corrélation linéaire
## de Spearman (corrélation des rangs). Interprétez le résultat
## obtenu.
rLSTAT <- myr(d$LSTAT)
##the linnear correlation is -0,736... which means that when the prices of housing goes up
## The % of lower class population decreases most of the time. 

## Q2.5 ----------------------------------------------------------------------
## Calculez l'erreur absolue moyenne.
era <- sum(abs(d$MEDV - predict(LINMEDVLSTAT, newdata = as.data.frame(d$LSTAT))))/length(d$MEDV)
## 4,5. 

## Q2.6 ----------------------------------------------------------------------
## Calculez la moyenne et l'écart-type des erreurs. Les erreurs vous
## semblent-elles normales ?

mean(abs(d$MEDV - predict(LINMEDVRM1, newdata = as.data.frame(d$LSTAT))))
sqrt(var(d$MEDV - predict(LINMEDVRM1, newdata = as.data.frame(d$LSTAT))))
## Q2.7 ----------------------------------------------------------------------
## Tracez les résidus en fonction des individus, de LSTAT, de MEDV.
## Que remarquez-vous ?
plot(d$MEDV - predict(LINMEDVLSTAT, newdata = as.data.frame(d$LSTAT)))


## Q2.8 ----------------------------------------------------------------------
## Identifiez les individus pour lesquels l'erreur est la plus grande. 
which.max(abs(d$MEDV - predict(LINMEDVLSTAT, newdata = as.data.frame(d$LSTAT))))
##372 is the max error, and we can se in the plot of the errors that the regression is are over
##estimated. 


### Partie 3 : régression linéaire avec une unique variable ==================

## Q3.1 ----------------------------------------------------------------------
## En vous basant sur les observations faîtes dans la partie
## précédente, proposez (au moins) trois modèles de régression
## n'utilisant que LSTAT (ou des transformations de cette variable)
## afin de construire un modèle de MDEV.

summary(lm(d$MEDV ~ (d$LSTAT)))
plot(d$MEDV ~ (d$LSTAT))

summary(lm(d$MEDV ~ (sqrt(d$LSTAT))))
plot(d$MEDV ~ I(sqrt(d$LSTAT)))

summary(lm(d$MEDV ~ I((d$LSTAT)^(1/4))))

summary(lm(d$MEDV ~ I((log(d$LSTAT)))))

summary(lm(d$MEDV ~ I(1/(d$LSTAT))))


## Q3.2 ----------------------------------------------------------------------
## Pour chacun des modèles, calculez les régressions et évaluez
## chacune d'entre elles (n'hésitez pas à faire des fonctions pour
## automatiser cela).
##in the summary you can se that the log(d$LSTAT) gave us a bit better results

## Q3.3 ----------------------------------------------------------------------
## Quel modèle recommandez-vous ?
##Looking at R^2 value we would choose log method because it gives us the highest value...


### Partie 4 : régression linéaire avec une plusieurs variables ==============

### On s'autorise maintenant à utiliser toutes les variables pour construire 
### un modèle de la valeur médiane.

## Q4.1 ----------------------------------------------------------------------
## Calculez le modèle de régression linéaire en fonction de toutes les autres
## variables. Qu'obtenez-vous ? Cela vous semble-t-il bon ?
summary(lm(d$MEDV ~.,d = d[-1]))

## Q4.2 ----------------------------------------------------------------------
## Selon vous, pourquoi ne faut-il pas utiliser la variable AREA ?
##Beacuase it is not numerci and therefore can't be used

## Q4.3 ----------------------------------------------------------------------
## Selon vous, pourquoi la variable AGE ne doit pas être utilisée telle 
## qu'elle ?
##The ages are bundled into 5 groups, and therefor it is not likley that they have a continous
##relation in a linneat regression

## Q4.4 ----------------------------------------------------------------------
## Transformez la variable "AGE" en facteur (as.factor) et recalculez la 
## régression linéaire.
newage_data <- d
newage_data$AGE <- as.factor(newage_data$AGE)
summary(lm(newage_data$MEDV ~.,newage_data = newage_data[-1]))
summary(lm(d$MEDV ~.,d = d[-1]))

summary(lm(newage_data$MEDV ~.,data = newage_data[-1]))

## Q4.5 ----------------------------------------------------------------------
## Proposez un *bon* modèle de MEDV.
## Vous indiquerez votre démarche pour construire votre modèle (sélection et 
## tranformation des variables), vos critères de qualité (performance, 
## simplicité, etc.). Vous commenterez les qualités et défauts de votre modèle
## en discutant des erreurs qu'il commet.

newd <- d

newd <- as.factor(newd$AGE)
newd <- as.factor(newd$CHAS)

##take away Indus, Age and Chas. We also use the log of the LSTAT variable. 
summary(lm(newd$MEDV ~ I(log(newd$LSTAT)) +., data = newd[c(-1,-3,-4,-8)]))

plot(lm(newd$MEDV ~ I(log(newd$LSTAT)) +., data = newd[c(-1,-3,-4,-8)]))



