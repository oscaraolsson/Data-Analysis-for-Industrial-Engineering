
### --------------------------------------------------------------------------
### Cours d'Analyse de donn?es pour le G?nie Industriel
### Grenoble INP - G?nie Industriel, 2A-ICL, 2015-2016-2017-2018-2019-2020
### I. Joly & P. Lemaire, 2016, 2017, 2018, 2019, 2020. Ne pas diffuser.
###
### encoding : windows-1252 (File > Reopen with encoding)
###
### TP Not? 2

### NOMS =====================================================================
# NOM1 : Oscar Olsson
# NOM2 : Samuele Vannoli

### Dans cet exercice, on utilise les outils de simulation (dite de Monte-Carlo)
###  pour montrer (voire d?montrer) certaines propri?t?s de la r?gression lin?aire.

### Il n'y a pas de jeu de donn?es associ? ? ce TP. 
### La simulation produit un r?sultat (s'il est valable) pour tous les jeux de
### donn?es possibles.
### Dans ce TP vous allez cr?er votre propre jeu de donn?es (avec quelques 
### consignes)

### Pour toutes les questions, n'h?sitez pas ? ?crire des fonctions qui vous
### permettent de r?-utiliser tout ou partie de ce que vous fa?tes. Vous 
### constaterez que beaucoup de questions se r?p?tent pour diff?rents mod?les :
### ne fa?tes qu'une fois le travail, puis r?-utilisez le ? bon escient !

### Partie I. : Cr?ation des donn?es ===========================================

## Q1.1 ----------------------------------------------------------------------
#  - G?n?rer un ?chantillon de taille N.Ech= 250, d'une m?me population dont 
#  les trois variables sont X_i, Y_i et U_i, respectant les ?l?ments suivant :
#     1. Les X_i sont issus d'une loi uniforme sur [-10;10]
#     2. Les Y_i sont donn?s par la droite de r?gression de la population, 
#        qui est d?finie par Y_i = 2 + 0.6 X_i  +  U_i
#        o? U_i suit une loi normale d'esp?rance 0 et d'?cart-type : 2

## PARAMETRES GENERAUX de l'?nonc? et valeurs initiales
X.MIN <- -10
X.MAX <- 10 
BETA0 <- 2
BETA1 <- 0.6
StD <- 2
N.Ech <- 250  
###########################



## Q1.2 ----------------------------------------------------------------------
#  - Estimer la droite de r?gression sur cet ?chantillon
#  - Repr?senter sur le m?me graphique la droite de r?gression estim?e et le 
#    nuage de points (X_i,Y_i), ainsi que la droite de r?gression de la 
#    population
###########################



## Q1.3 ----------------------------------------------------------------------
#  - A votre avis (r?pondez sans calcul, ni script suppl?mentaire), que se passera-t-il si vous 
#    augmentez la valeur de l'?cart-type U_i (par exemple ? 20) ? 
###########################


## Q1.4 ----------------------------------------------------------------------
#  - G?n?rer 100 ?chantillons de taille 250, issus de la population pr?c?dente, c'est ? dire respectant
#    les m?mes contraintes, dont l'?cart-type = 2.
#  - Estimer la droite de r?gression sur ces 100 ?chantillons
#  - Repr?senter sur le m?me graphique les 100 droites de r?gression estim?es, 
#  - ainsi que la droite de r?gression de la population (ou la vraie droite)
#  - Quelle droite de r?gression vous para?t la meilleure ? 
#       Pourquoi est-elle la meilleure ?

## El?ments de script pouvant vous aider et ?tre compl?t?s ############
plot(NULL , xlim=c(X.MIN,X.MAX), ylim=c(BETA0+BETA1*X.MIN, BETA0+BETA1*X.MAX), col="red", lwd=3, xlab="x", ylab="y")
# L'option 'NULL' dans l'instruction permet de constituer un graphique vierge, dans lequel 
#  d'autres fonctions plot() pourront s'afficher.
for(n in 1:100){   }
# L'instruction for(){} r?alise une boucle, ici 100 fois. Il reste ? pr?ciser ce que l'on 
# souhaite r?aliser n fois...
###########################




###########################
#  Partie II: Biais d'estimateur et Simulations Monte-Carlo

# L'exercice ici consiste ? expliquer le script donn? et ? interpr?ter les r?sultats

#  Quelques ?l?ments :
#  Dans les lignes de code suivantes :
#    n=rep(N.Ech,N.Exp) : g?n?re un vecteur pour lequel N.Ech est r?p?t? N.Exp fois
#    Ainsi n=rep(2,4)  renvoie : 2 2 2 2

## PARAMETRES GENERAUX de l'?nonc? et valeurs initiales
BETA0 <- 2  ;   BETA1 <- 0.6  ;  N.Ech <- 25  ;  N.exp <- 99   ;   StD <- 2 # choix sur StD ; s'amuser avec !
# Valeurs ? choisir
X.MIN <- -10  ;  X.MAX <- 10 
# D?finition de marges des histogrammes finaux
marg.B0 <- 2  ;  marg.B1 <- 1

## Q2.1 ----------------------------------------------------------------------
#  - Explicitez sommairement les scripts suivant :
##    1. Que fait la fonction one.reg(n,s)
##    2. Que fait la fonction repeat.reg(n=rep(N.Ech,N.exp), beta0=BETA0, beta1=BETA1, sd=StD)

## Exp?riences : calcul de n.exp estimations de beta0, beta1
repeat.reg <- function(n=rep(N.Ech,N.exp), beta0=BETA0, beta1=BETA1, sd=StD){
  one.reg <- function(n,s){
    # calcul des donn?es
    x <- runif(n, min=X.MIN,max=X.MAX) # ou rnorm, ou autre
    u <- rnorm(n, mean=0, sd=s) 
    y <- beta0 + beta1 * x + u
    
    # calcul de la r?gression
    reg <- lm(y ~ x)
    c(reg$coefficients, summary(reg)$coefficients[,2],n, s) 
  }
  resultats <- mapply(one.reg, n, StD)
  row.names(resultats) <- c("beta0", "beta1", "sig0", "sig1", "n","StD")
  return(resultats)
}
###########################



## Q2.2 ----------------------------------------------------------------------
#  - Expliquez ce que sont N.ech et N.exp dans l'appel de la fonction repeat.reg(...) ci-dessous
#  Remarquez que l'on sp?cifie ici, dans le rep(...) : N.ech=20 puis N.ech=150, 
#                                   et N.ech=250 ; N.exp=100

betas <- repeat.reg(n=c(rep(20, 100), rep(150, 100), rep(250, 100)), sd=1 )#,sd = c(1,5))
###########################




## Q2.3 ----------------------------------------------------------------------
#  - Commentez le graphique obtenu ci-dessous
betas1 <- betas[,betas[5,]==20]  ;   betas2 <- betas[,betas[5,]==150]  ;  betas3 <- betas[,betas[5,]==250]
plot(NULL, xlim=c(X.MIN,X.MAX), ylim=c(BETA0+BETA1*X.MIN, BETA0+BETA1*X.MAX), col="red", lwd=3, xlab="x", ylab="y")
# Droites estim?es
mapply(function(a,b) abline(a=a, b=b, col="gray", lwd=0.5), betas1[1,], betas1[2,])
mapply(function(a,b) abline(a=a, b=b, col="red", lwd=2, add=T), betas2[1,], betas2[2,])
mapply(function(a,b) abline(a=a, b=b, col="green", lwd=0.5, add=T), betas3[1,], betas3[2,])
abline(a=BETA0 , b=BETA1, col="black", lwd=2, lty=2)
legend(X.MIN+1, BETA0+BETA1*X.MAX , legend=c("n= 20", "n= 150", "n= 250", "droite pop."),
       col=c("gray", "red", "green","black"), lty=c(1,1,1,2), cex=0.8)
##############################


## Q2.4 ----------------------------------------------------------------------
#  En question Q1.3, on vous demandait : 'que ce passera-t-il si vous augmentez la
#  valeur de l'?cart-type U_i (par exemple ? 20) ? 
#  - Pour illustrer ce ph?nom?ne, modifiez l'appel des fonctions pr?c?dentes. Commentez vos r?sultats

###########################



## Q2.5 ----------------------------------------------------------------------
# - Expliquez ce que produit la fonction mon.histo(xs, titre, bornes) suivante :

mon.histo <- function(xs, titre, bornes){
  hist(xs, freq=F, main=titre, xlim=bornes, xlab = "")
  f <- function(x) dnorm(x, mean=mean(xs), sd=sd(xs))
  curve(f, add=T, col="red", lw=2)
}
###########################


## ----------------------------------------------------------------------
# L'instruction layout() permet de param?trer la fen?tre d'affichage des graphiques selon une disposition
# choisie et renseign?e dans m2. 
# Il ne vous sera probablement pas utile de modifier ces 3 lignes.
# Pr?paration de la mosaique des graphiques dans l'ordre d'affichage en ligne :
size <- 2*length(unique(betas["n",]))
m2 <- rbind( seq(1,size/2,1), seq(1+size/2,size,1))
layout (m2) ; layout.show (size)
###########################


## ----------------------------------------------------------------------
# Dans les deux boucles suivantes, l'objet txt contient le titre du graphique.
# Ce titre est obtenu par l'instruction substitute(...) qui g?re les chaines de caract?res 
# int?grant des valeurs d'objets externes
## Q2.6 ----------------------------------------------------------------------
# - Commentez les r?sultats obtenus. Que repr?sentent ces graphiques ? 
#   Que d?duit-on de chaque s?rie de 3 graphiques ?

for(n in unique(betas["n",])){
  txt <-  substitute(   paste(beta[0], "=",  m, "; N.ech = ", n ),
                        list(m = BETA0, n =n) )
  mon.histo(  betas["beta0"  ,   betas["n",]==n]   , txt,  c(BETA0-marg.B0,BETA0+marg.B0))

}

for(n in unique(betas["n",])){
  txt <-  substitute( paste(beta[1], "=",  m, "; N.ech = ", n ),
                      list(m = BETA1, n =n) )
  mon.histo(  betas["beta1"  ,   betas["n",]==n]   , txt,  c(BETA1-marg.B1,BETA1+marg.B1))
}
###########################



## Q2.7 ----------------------------------------------------------------------
# - Faites varier la valeur de l'?cart-type des U_i et produisez les 7 graphiques correspondants (graphiques des droites superpos?es et les histrogrammes).
# - Commentez vos r?sultats.

###########################

## Q2.8 ----------------------------------------------------------------------
# On souhaite maintenant voir l'impact de la loi des U_i
# - Reconstruisez ci-dessous les ?l?ments n?cessaires pour refaire les r?gressions et les 7 graphiques pr?c?dents sur des tailles d'?chantillon variables, en rempla?ant la loi des U_i par une loi uniforme
# - Commentez vos r?sultats.

###########################

# pour remettre ? z?ro les param?tres graphiques (et vider la fen?tre) : d?commenter la ligne suivante:
#dev.off()
