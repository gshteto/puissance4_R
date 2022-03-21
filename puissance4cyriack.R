require(plotrix)
require(gtools)

### my.circle.P4 prend 3 arguments
### - x : l'abscisse du centre du cercle
### - y : l'ordonnee du centre du cercle
### - r : rayon du cercle
### - lwd : le type de la ligne du cercle
### - col : une valeur de couleur pour le disque
###
### my.circle.P4 ne revoie rien mais affiche un cercle   un graphique existant
###
my.circle.P4 <- function(x,y,r,lwd=1,col=1){
  angle <- seq(0,2*pi,length=100)
  xx <- x+r*cos(angle)
  yy <- y+r*sin(angle)
  polygon(xx,yy,col=col,border=1)
}

### Affiche.Puissance4 prend 1 argument
### - M : une matrice de taille 3x3
###
### Affiche.Puissance4 ne revoie rien mais affiche le jeu dans l'etat de la matrice M
###
Affiche.Puissance4 <- function(M){
  M <- t(M)
  mar=c(0.1,0.1,0.1,1.1)
  plot(c(0,7),c(0,6),xlim=c(0,7),ylim=c(0,6),col=0,yaxt="n",xaxt="n",xlab="",ylab="")
  segments(0,0,0,6)
  segments(0,0,7,0)
  segments(7,6,7,0)
  segments(7,6,0,6)
  for (i in 1:7){
    for (j in 1:6){
      if (is.na(M[i,j])){
        rect(i-1,j-1,i,j,col=0)
      } else if (M[i,j]=="+"){
        rect(i-1,j-1,i,j,col=0)
        my.circle.P4(x=i-0.5,y=j-0.5,r=0.3,lwd=2,col=2)
      } else if (M[i,j]=="-"){
        rect(i-1,j-1,i,j,col=0)
        my.circle.P4(x=i-0.5,y=j-0.5,r=0.3,lwd=2,col=7)
      }
    }
  }
}

### Gagnant.Puissance4.plot prend 3 arguments
### - M : une matrice de taille 3x3
### - i : un indice de ligne
### - j : un indice de colonne
###
### Gagnant.Puissance4.plot est identique a la fonction Gagnant.Puissance4 sauf qu'elle renvoie les coordonnees de la serie de 4 pions gagnante
###
Gagnant.Puissance4.plot <- function(M,i,j){
  ## Horizontal
  res <- FALSE
  if (j<=4){
    tmp <- all(M[i,1:4]==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=rep(i,times=4),j=1:4))}
  }
  if (j>=2 & j <= 5){
    tmp <- all(M[i,2:5]==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=rep(i,times=4),j=2:5))}
  }
  if (j>=3 & j <= 6){
    tmp <- all(M[i,3:6]==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=rep(i,times=4),j=3:6))}
  }
  if (j>=4 & j <= 7){
    tmp <- all(M[i,4:7]==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=rep(i,times=4),j=4:7))}
  }
  ## Vertical
  if (i<=4){
    tmp <- all(M[1:4,j]==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=1:4,j=rep(j,times=4)))}
  }
  if (i>=2 & i <= 5){
    tmp <- all(M[2:5,j]==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=2:5,j=rep(j,times=4)))}
  }
  if (i>=3 & i <= 6){
    tmp <- all(M[3:6,j]==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=3:6,j=rep(j,times=4)))}
  }
  ## Diagonal Droite
  if ( (i+3 <= 6) & (j+3 <= 7)){
    tmp <- all(c(M[i,j],M[i+1,j+1],M[i+2,j+2],M[i+3,j+3])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=i:(i+3),j=j:(j+3)))}
  }
  if ( (i+2 <= 6) & (j+2 <= 7) & (i-1 >=1) & (j-1 >= 1)){
    tmp <- all(c(M[i-1,j-1],M[i,j],M[i+1,j+1],M[i+2,j+2])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=(i-1):(i+2),j=(j-1):(j+2)))}
  }
  if ( (i+1 <= 6) & (j+1 <= 7) & (i-2 >=1) & (j-2 >= 1)){
    tmp <- all(c(M[i-2,j-2],M[i-1,j-1],M[i,j],M[i+1,j+1])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=(i-2):(i+1),j=(j-2):(j+1)))}
  }
  if ( (i-3 >=1) & (j-3 >= 1)){
    tmp <- all(c(M[i-3,j-3],M[i-2,j-2],M[i-1,j-1],M[i,j])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=(i-3):i,j=(j-3):j))}
  }
  ## Diagonal Gauche
  if ( (i+3 <= 6) & (j-3 >= 1)){
    tmp <- all(c(M[i,j],M[i+1,j-1],M[i+2,j-2],M[i+3,j-3])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=i:(i+3),j=j:(j-3)))}
  }
  if ( (i+2 <= 6) & (j-2 >= 1) & (i-1 >=1) & (j+1 <=7)){
    tmp <- all(c(M[i-1,j+1],M[i,j],M[i+1,j-1],M[i+2,j-2])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=(i-1):(i+2),j=(j+1):(j-2)))}
  }
  if ( (i+1 <= 6) & (j-1 >= 1) & (i-2 >=1) & (j+2 <=7)){
    tmp <- all(c(M[i-2,j+2],M[i-1,j+1],M[i,j],M[i+1,j-1])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=(i-2):(i+1),j=(j+2):(j-3)))}
  }
  if ( (i-3 >=1) & (j+3 <=7)){
    tmp <- all(c(M[i-3,j+3],M[i-2,j+2],M[i-1,j+1],M[i,j])==M[i,j])
    if (!is.na(tmp) & tmp){return(list(i=(i-3):i,j=(j+3):j))}
  }
  return(list(i=NA,j=NA))
}

### Gagnant.Puissance4 prend 3 arguments
### - M : une matrice de taille 6x7
### - i : un indice de ligne
### - j : un indice de colonne
###
### Gagnant.Puissance4 revoie un boolean si le case (i,j) dans la matrice M fait partie d'une serie de 4 pions gagnantes
###
Gagnant.Puissance4 <- function(M,i,j){
  res <- 0 ### Par defaut la case (i,j) n'est pas dans une configuration gagnante
  ### Dans la suite, toutes les possibilites sont testees
  ##### Cas 1 : Horizontal si 1 <= j <= 4
  if (j<=4){
    tmp <- all(M[i,1:4]==M[i,j])
    if (!is.na(tmp) & tmp){return(M[i,j])}
  }
  ##### Cas 2 : Horizontal si 2 <= j <= 5
  if (j>=2 & j <= 5){
    tmp <- all(M[i,2:5]==M[i,j])
    if (!is.na(tmp) & tmp){return(M[i,j])}
  }
  ##### Cas 3 : Horizontal si 3 <= j <= 6
  if (j>=3 & j <= 6){
    tmp <- all(M[i,3:6]==M[i,j])
    if (!is.na(tmp) & tmp){return(M[i,j])}
  }
  ##### Cas 4 : Horizontal si 4 <= j <= 7
  if (j>=4 & j <= 7){
    tmp <- all(M[i,4:7]==M[i,j])
    if (!is.na(tmp) & tmp){return(M[i,j])}
  }
  ##### Cas 5 : Vertical si 1 <= i <= 4
  if (i<=4){
    tmp <- all(M[1:4,j]==M[i,j])
    if (!is.na(tmp) & tmp){return(M[i,j])}
  }
  ##### Cas 6 : Vertical si 2 <= i <= 5
  if (i>=2 & i <= 5){
    tmp <- all(M[2:5,j]==M[i,j])
    if (!is.na(tmp) & tmp){return(M[i,j])}
  }
  ##### Cas 7 : Vertical si 3 <= i <= 6
  if (i>=3 & i <= 6){
    tmp <- all(M[3:6,j]==M[i,j])
    if (!is.na(tmp) & tmp){return(M[i,j])}
  }
  ##### Cas 8 : Diagonal Droite si 1 <= i <= 4 et 1 <= j <= 4
  if ( (i+3 <= 6) & (j+3 <= 7)){
    tmp <- all(c(M[i,j],M[i+1,j+1],M[i+2,j+2],M[i+3,j+3])==M[i,j])
    if (!is.na(tmp) & tmp){return(M[i,j])}
  }
  ##### Cas 9 : Diagonal Droite si 2 <= i <= 5 et 2 <= j <= 5
  if ( (i+2 <= 6) & (j+2 <= 7) & (i-1 >=1) & (j-1 >= 1)){
    tmp <- all(c(M[i-1,j-1],M[i,j],M[i+1,j+1],M[i+2,j+2])==M[i,j])
    if (!is.na(tmp) & tmp){return(M[i,j])}
  }
  ##### Cas 10 : Diagonal Droite si 3 <= i <= 6 et 3 <= j <= 7
  if ( (i+1 <= 6) & (j+1 <= 7) & (i-2 >=1) & (j-2 >= 1)){
    tmp <- all(c(M[i-2,j-2],M[i-1,j-1],M[i,j],M[i+1,j+1])==M[i,j])
    if (!is.na(tmp) & tmp){return(M[i,j])}
  }
  ##### Cas 11 : Diagonal Droite si 4 <= i <= 6 et 4 <= j <= 7
  if ( (i-3 >=1) & (j-3 >= 1)){
    tmp <- all(c(M[i-3,j-3],M[i-2,j-2],M[i-1,j-1],M[i,j])==M[i,j])
    if (!is.na(tmp) & tmp){return(M[i,j])}
  }
  ## Diagonal Gauche - identique aux diagnoles droite mais dans l'autre sens
  if ( (i+3 <= 6) & (j-3 >= 1)){
    tmp <- all(c(M[i,j],M[i+1,j-1],M[i+2,j-2],M[i+3,j-3])==M[i,j])
    if (!is.na(tmp) & tmp){return(M[i,j])}
  }
  if ( (i+2 <= 6) & (j-2 >= 1) & (i-1 >=1) & (j+1 <=7)){
    tmp <- all(c(M[i-1,j+1],M[i,j],M[i+1,j-1],M[i+2,j-2])==M[i,j])
    if (!is.na(tmp) & tmp){return(M[i,j])}
  }
  if ( (i+1 <= 6) & (j-1 >= 1) & (i-2 >=1) & (j+2 <=7)){
    tmp <- all(c(M[i-2,j+2],M[i-1,j+1],M[i,j],M[i+1,j-1])==M[i,j])
    if (!is.na(tmp) & tmp){return(M[i,j])}
  }
  if ( (i-3 >=1) & (j+3 <=7)){
    tmp <- all(c(M[i-3,j+3],M[i-2,j+2],M[i-1,j+1],M[i,j])==M[i,j])
    if (!is.na(tmp) & tmp){return(M[i,j])}
  }
  return(res)
}

### EndOfGame prend 1 argument
### - M : une matrice de taille 3x3
###
### EndOfGame revoie un boolean (TRUE: si coup gagnant ou plus de case vide, FALSE: sinon)
###
EndOfGame.Puissance4 <- function(M, i.cur, j.cur){
  if (any(is.na(M))){
    win <- Gagnant.Puissance4(M, i.cur, j.cur)
    if (win != 0){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(TRUE)
  }
}

### Min renvoie une valeur numerique qui correspond à l'evaluation
###

Min <- function(M,symbol="+",Profondeur,i.cur.pred,j.cur.pred){
  ### new.symbol correspond au symbol du coup d'après (c'est à dire le symbole de l'adversaire). Il va etre appele dans la fonction Min
  if (symbol=="-"){
    new.symbol <- "+"
  } else {
    new.symbol <- "-"
  }
  ### Calcul de l'evaluation 
  ###### Cas 1: le jeu est fini avant le coup soit parce qu'il n'y a plus de case dispo, soit parce que nous avons atteint la profondeur nulle
  if (EndOfGame.Puissance4(M,i.cur.pred,j.cur.pred) | Profondeur==0){
    Win.cur <- Gagnant.Puissance4(M,i.cur.pred,j.cur.pred)
    if (Win.cur=="+"){
      if (symbol=="+"){
        tmp <- 1000 ## Evaluation a 1 si le jeu est gagnant avant le coup
      } else {
        tmp <- -1000 ## Evaluation a 1 si le jeu est perdant avant le coup
      }
    } else if (Win.cur=="-"){
      if (symbol=="+"){
        tmp <- -1000 ## Evaluation a 1 si le jeu est perdant avant le coup
      } else {
        tmp <- 1000 ## Evaluation a 1 si le jeu est gagnant avant le coup
      }
    } else {
      tmp <- 0 ## Evaluation a 0 si pas de gagnant
    }
    return(tmp) ### On renvoie l'evaluation si le jeu est fini avant le coup
  } else { ###### Cas 2: le jeu n'est pas fini avant le coup
    eval.M <- matrix(NA,ncol=7,nrow=6) ### Matrice stockant l'evaluation de chacun des coups possibles
    ### Double boucle for pour evaluer toutes les cases
    for (j.cur in 1:7) {
      tmp.vide <- which(is.na(M[,j.cur]))
      if (length(tmp.vide) > 0){ ### dans ce cas, la colonne n'est pas remplie
        i.cur <- min(tmp.vide) ### indice de la ligne pour la colonne choisie
      } else {
        i.cur <- NA
      }
      if (is.na(i.cur) == FALSE){
        M.tmp <- M ### Stockage temporaire de la matrice du jeu avant le coup
        M.tmp[i.cur,j.cur] <- symbol ### On joue le coup
        ### Appel à la fonction Min. 
        ### Ici on évalue le coup en anticipant le coup suivant de l'adversaire
        ### Pour ce coup suivant le jeu avant ce coup est dans M.tmp, le symbol sera celui de l'adversaire et la profondeur sera diminuée de 1
        ### A noter que la fonction Min est récursive (elle fait appel à elle-même) et renvoie l'évaluation du coup dans la variable tmp
        tmp <- -Min(M.tmp,symbol=new.symbol,Profondeur=Profondeur-1,i.cur,j.cur) ### Evaluation du coup en anticipant le coup
        eval.M[i.cur,j.cur] <- tmp ### Mise à jour de l'evaluation
      }
    }
  }
  wmax <- which(eval.M == max(eval.M,na.rm=TRUE), arr.ind = TRUE) ### On recupere les indices de ligne et de colonne sont maximum (on cherche le coup ayant une evaluation maximale!)
  ind.max <- sample(1:nrow(wmax),1) ### Selection parmi les maximums s'il en ya plusieurs ex-aequo
  max.i <- wmax[ind.max,1] ### Stockage de l'indice de ligne du coup que l'on va jouer
  max.j <- wmax[ind.max,2] ### Stockage de l'indice de colonne du coup que l'on va jouer
  return(eval.M[max.i,max.j]) ### On renvoie l'evaluation
}


### Play.OneShot renvoie les indices du coup qui va être joué
###

Play.OneShot <- function(M,symbol="-",Profondeur){
  ### new.symbol correspond au symbol du coup d'après (c'est à dire le symbole de l'adversaire). Il va etre appele dans la fonction Min
  if (symbol=="-"){
    new.symbol <- "+" 
  } else {
    new.symbol <- "-"
  }
  max.i <- NA ### variable stockant l'indice de la ligne dans laquelle le coup va etre joue
  max.j <- NA ### variable stockant l'indice de la colonne dans laquelle le coup va etre joue
  eval.M <- matrix(NA,ncol=7,nrow=6) ### Matrice stockant l'evaluation de chacun des coups possibles
  ### Boucle for pour evaluer les 7 colonnes
  for (j.cur in 1:7){
    M.tmp <- M ### Copie temporaire de la matrice jeu pour conserver l'etat initial du jeu avant le coup
    tmp.vide <- which(is.na(M[,j.cur]))
    if (length(tmp.vide) > 0){ ### dans ce cas, la colonne n'est pas remplie
      i.cur <- min(tmp.vide) ### indice de la ligne pour la colonne choisie
    } else {
      i.cur <- NA
    }  
    if (is.na(i.cur) == FALSE){
      M.tmp[i.cur,j.cur] <- symbol
      ### Evaluation du coup
      ###### Evaluation si le coup termine le jeu
      if (EndOfGame.Puissance4(M.tmp,i.cur,j.cur)){
        Win.cur = Gagnant.Puissance4(M.tmp,i.cur,j.cur)
        if (Win.cur=="+"){
          if (symbol=="+"){
            tmp = 1000
          } else {
            tmp = -1000
          }
        } else if (Win.cur=="-"){
          if (Win.cur=="+"){
            tmp = -1000
          } else {
            tmp = 1000
          }
        } else {
          tmp = 0 # Win.cur=0 pas de gagnant et plus de cases vides
        }
      } else {
        ###### Evaluation si le coup ne termine pas le jeu
        ### Appel à la fonction Min. 
        ### Ici on évalue le coup suivant qui sera jouer par l'adversaire
        ### Pour ce coup suivant le jeu avant ce coup est dans M.tmp, le symbol sera celui de l'adversaire et la profondeur sera diminuée de 1
        ### A noter que la fonction Min est récursive (elle fait appel à elle-même) et renvoie l'évaluation du coup dans la variable tmp
        tmp <- -Min(M.tmp,symbol=new.symbol,Profondeur=Profondeur-1,i.cur,j.cur)
      }
      eval.M[i.cur,j.cur] <- tmp ### Mise à jour de la matrice d'evaluation
    }
  }
  wmax <- which(eval.M == max(eval.M,na.rm=TRUE), arr.ind = TRUE) ### On recupere les indices de ligne et de colonne sont maximum (on cherche le coup ayant une evaluation maximale!)
  #  print(eval.M) ### Affichage de la matrice d'evaluation
  #  print(wmax) ### Affichage de l'ensemble des cases maximum
  ind.max <- sample(1:nrow(wmax),1) ### Selection parmi les maximums s'il en ya plusieurs ex-aequo
  #  print(ind.max)
  max.i <- wmax[ind.max,1] ### Stockage de l'indice de ligne du coup que l'on va jouer
  max.j <- wmax[ind.max,2] ### Stockage de l'indice de colonne du coup que l'on va jouer
  return(list(i=max.i,j=max.j)) ### On renvoie les indices de ligne et colonne du coup que l'on va jouer
}

### Play.Computers.Puissance4 prend 3 arguments
### - POSA : Une fonction qui correspond a la fonction Play.OneShot.Random en terme d'arguments d'entree et de sortie
### - POSB : Une fonction qui correspond a la fonction Play.OneShot.Random en terme d'arguments d'entree et de sortie
### - Profondeur : un entier indiquant la profondeur d' valuation, c'est   dire le nombre de coup   l'avance anticip  pour l' valuation
###
### La fonction Play.Computers.Puissance4 joue la partie en alternant la fonction POSA et POSB, c'est a dire en alternant les deux joueurs.
### La fonction Play.Computers.Puissance4 renvoie le vainqueur du jeu ou Match nul
###
Play.Computers.Puissance4 <- function(POS.A,POS.B,Profondeur){
  name.A <- readline(prompt ="Nom du joueur A")
  name.B <- readline(prompt="Nom du joueur B")
  start <- sample(x=c(1,2),size=1)
  col1 <- "Jaune"
  col2 <- "Rouge"
  if (start==1){
    Player1 <- name.A
    POS1 <- POS.A
    Player2 <- name.B
    POS2 <- POS.B
  } else {
    Player1 <- name.B
    POS1 <- POS.B
    Player2 <- name.A
    POS2 <- POS.A
  }
  cat(Player1,":",col1, "## ",Player2,":",col2, "- \n")
  M <- matrix(NA,ncol=7,nrow=6)
  tour <- 1
  Affiche.Puissance4(M)
  End <- FALSE
  nb.coup <- 0
  while(!End & nb.coup <= 42){
    readline(prompt="Press Enter")
    nb.coup <- nb.coup+1
    #print(tour)
    if (tour%%2==0){
      print(paste("Au tour de",Player2,"##",col2))
      new.ind <- POS2(M,symbol="+",Profondeur=Profondeur)
      M[new.ind$i,new.ind$j] <- "+"
      G <- Gagnant.Puissance4.plot(M,new.ind$i,new.ind$j)
      if (!is.na(G$i[1])){
        Affiche.Puissance4(M)
        segments(x0=G$j[1]-0.5,y0=G$i[1]-0.5,x1=G$j[4]-0.5,y1=G$i[4]-0.5)
        print(paste("VAINQUEUR:",Player2))
        return(list(M=M,new.ind=new.ind))
      }
    } else {
      print(paste("Au tour de",Player1,"##",col1))
      new.ind <- POS1(M,symbol="-",Profondeur=Profondeur)
      M[new.ind$i,new.ind$j] <- "-"
      G <- Gagnant.Puissance4.plot(M,new.ind$i,new.ind$j)
      if (!is.na(G$i[1])){
        Affiche.Puissance4(M)
        segments(x0=G$j[1]-0.5,y0=G$i[1]-0.5,x1=G$j[4]-0.5,y1=G$i[4]-0.5)
        print(paste("VAINQUEUR:",Player1))
        return(list(M=M,new.ind=new.ind))
      }
    }
    # print(M)
    #cat("La partie continue:",EndOfGame.Puissance4(M),"\n")
    cat("La partie continue:","\n")
    if (EndOfGame.Puissance4(M,new.ind$i,new.ind$j)){
      print("Pas Vainqueur")
      End <- TRUE
      return(list(M=M,new.ind=list(i=NA,j=NA)))
    }
    Affiche.Puissance4(M)
    tour <- tour+1
  }
}

M.final <- Play.Computers.Puissance4(Play.OneShot,Play.OneShot,Profondeur = 5)

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################

#pas de gagnant et certaine(s) cases vides
Evaluation = function(M,i,j) {
  res <- c(0) ### Par defaut la case (i,j) n'est pas dans une configuration gagnante
  ### Dans la suite, toutes les possibilites sont testees
  ##### Cas I : 3 points alignés
  ##### Cas 1 : Horizontal si 1 <= j <= 3
  if (j<=3){
    tmp <- all(M[i,1:3]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 750)}
  }
  ##### Cas 2 : Horizontal si 2 <= j <= 4
  if (j>=2 & j<=4){
    tmp <- all(M[i,2:4]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 750)}
  }
  ##### Cas 3 : Horizontal si 3 <= j <= 5
  if (j>=3 & j<=5){
    tmp <- all(M[i,3:5]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 750)}
  }
  ##### Cas 4 : Horizontal si 4 <= j <= 6
  if (j>=4 & j<=6){
    tmp <- all(M[i,4:6]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 750)}
  }
  ##### Cas 5 : Horizontal si 5 <= j <= 7
  if (j>=5 & j<=7){
    tmp <- all(M[i,5:7]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 750)}
  }
  ##### Cas 6 : Vertical si 1 <= i <= 3
  if (i<=3){
    tmp <- all(M[1:3,j]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 750)}
  }
  ##### Cas 7 : Vertical si 2 <= i <= 4
  if (i>=2 & i <= 4){
    tmp <- all(M[2:4,j]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 750)}
  }
  ##### Cas 8 : Vertical si 3 <= i <= 5
  if (i>=3 & i <= 5){
    tmp <- all(M[3:5,j]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 750)}
  }
  ##### Cas 9 : Vertical si 4 <= i <= 6
  if (i>=4 & i <= 6){
    tmp <- all(M[4:6,j]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 750)}
  }
  
  ##### Cas 8 : Diagonal Droite si 1 <= i <= 4 et 1 <= j <= 4
  if ( (i+3 <= 6) & (j+3 <= 7)){
    tmp <- all(c(M[i,j],M[i+1,j+1],M[i+2,j+2],M[i+3,j+3])==M[i,j])
    if (!is.na(tmp) & tmp){return(TRUE)}
  }
  ##### Cas 9 : Diagonal Droite si 2 <= i <= 5 et 2 <= j <= 5
  if ( (i+2 <= 6) & (j+2 <= 7) & (i-1 >=1) & (j-1 >= 1)){
    tmp <- all(c(M[i-1,j-1],M[i,j],M[i+1,j+1],M[i+2,j+2])==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 750)}
  }
  
  
  
  
  ##### Cas II : 2 points alignés
  ##### Cas 1 : Horizontal si 1 <= j <= 2
  if (j<=2){
    tmp <- all(M[i,1:2]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 500)}
  }
  ##### Cas 2 : Horizontal si 2 <= j <= 3
  if (j>=2 & j<=3){
    tmp <- all(M[i,2:3]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 500)}
  }
  ##### Cas 3 : Horizontal si 3 <= j <= 4
  if (j>=3 & j<=4){
    tmp <- all(M[i,3:4]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 500)}
  }
  ##### Cas 4 : Horizontal si 4 <= j <= 5
  if (j>=4 & j<=5){
    tmp <- all(M[i,4:5]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 500)}
  }
  ##### Cas 5 : Horizontal si 5 <= j <= 6
  if (j>=5 & j<=6){
    tmp <- all(M[i,5:6]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 500)}
  }
  ##### Cas 6 : Horizontal si 6 <= j <= 7
  if (j>=6 & j<=7){
    tmp <- all(M[i,6:7]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 500)}
  }
  ##### Cas 7 : Vertical si 1 <= i <= 2
  if (i<=2){
    tmp <- all(M[1:2,j]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 500)}
  }
  ##### Cas 8 : Vertical si 2 <= i <= 3
  if (i>=2 & i <= 3){
    tmp <- all(M[2:3,j]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 500)}
  }
  ##### Cas 9 : Vertical si 3 <= i <= 4
  if (i>=3 & i <= 4){
    tmp <- all(M[3:4,j]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 500)}
  }
  ##### Cas 10 : Vertical si 4 <= i <= 5
  if (i>=4 & i <= 5){
    tmp <- all(M[4:5,j]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 500)}
  }
  ##### Cas 11 : Vertical si 5 <= i <= 6
  if (i>=5 & i <= 6){
    tmp <- all(M[5:6,j]==M[i,j])
    if (!is.na(tmp) & tmp){res = c(res, 500)}
  }
  
  
  return(max(res))
}


