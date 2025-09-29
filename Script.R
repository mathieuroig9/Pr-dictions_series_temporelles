#install.packages("zoo")
#install.packages("tseries")
#install.packages("fUnitRoots")
#install.packages("ggplot2")
#install.packages("xtable")
#install.packages("forecast")

library(zoo)
library(tseries)
library(fUnitRoots)
library(ggplot2)
library(xtable)
library(forecast)


#### Q1 ####
###IMPORTATION DES DONNEES###
path <- 'W://Bureau//projet série temp' ##A MODIFIER AVANT D'EXECUTER
setwd(path) #definit l'espace de travail (working directory ou "wd")
datafile <- "valeurs_mensuelles.csv" #definit le fichier de donnees
data <- read.csv(datafile,sep=";") #importe un fichier .csv dans un objet de classe data.frame

data <- data[4:nrow(data), c(1, 2)]  # colonnes 1 et 2 : date et valeur

colnames(data) <- c("date", "valeur")


data$date <- as.yearmon(data$date, "%Y-%m") #plus pratique pour l'utilisation avec zoo
data$valeur <- as.numeric(data$valeur)


#### Q2 ####

serie <- zoo(data$valeur, order.by = data$date)


plot(serie)
#on trace la série pour vérifier visuellement si statio, clairement présence d'une tendance
#essayons d'abord de voir si cette tendance est déterministe

##TENDANCE DETERMINISTE ?

date <- time(serie)  #on prend les indices temporelles pour faire la regression
summary(lm(coredata(serie) ~ date)) #on fait un OLS
#beta=-38.43 et p-value: < 2.2e-16, on pourrait alors penser qu'il y'a bien une tendance déterministe
#car le coefficient est élevé et significatif
#MAIS RESULTATS NON INTERPRETABLES A CE STADE CAR ON NE SAIT PAS SI LES RESIDUS SONT STATIONNAIRES

#on va alors faire un test ADF sur la série pour savoir si elle est bien stationnaire autour de la tendance déterministe

adf <- adfTest(serie, lag=0, type="ct") #test ADF avec constante et tendance déterministe
adf
#p-value: 0.1995, on serait tenté de dire qu'on ne rejette pas H0
#MAIS RESULTATS NON INTERPRETABLES A CE STADE CAR ON NE SAIT PAS SI LES RESIDUS SONT AUTOCORRELES

#On va tester avec Ljung-box si les résidus sont autocorrélés

Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}


Qtests(adf@test$lm$residuals, 24, fitdf = length(adf@test$lm$coefficients))

#les pval sont à chaque fois très faibles, donc les résidus sont extremement corrélés
#donc le test ADF réalisé plus tôt avec un lag=0 n'est pas valide, donc on ne peut pas interpréter les résultats

#On va chercher maintenant le lag du test ADF avec lequel les résidus ne sont plus corrélés

series <- serie; kmax <- 24; adftype="ct"
adfTest_valid <- function(series, kmax, adftype){
  k <- 0
  noautocorr <- 0
  while (noautocorr == 0 && k <= kmax){
    cat(paste0("ADF with ", k, " lags: residuals OK? "))
    adf <- adfTest(series, lags = k, type = adftype)
    fitdf <- length(adf@test$lm$coefficients)
    pvals <- Qtests(adf@test$lm$residuals, kmax, fitdf = fitdf)[, 2]
    valid_pvals <- pvals[!is.na(pvals)]
    
    if (length(valid_pvals) == 0) {
      cat("Pas de pval (tous NA)\n")
      return(NULL)
    } else if (sum(valid_pvals < 0.05) == 0) {
      noautocorr <- 1
      cat("OK\n")
    } else {
      cat("NOK\n")
    }
    k <- k + 1
  }
  
  if (k > kmax) {
    cat("Aucune configuration trouvée avec résidus non autocorrélés jusqu’à", kmax, "lags.\n")
    return(NULL)
  }
  
  return(adf)
}

adf <- adfTest_valid(serie,50,adftype="ct")
#Même en mettant un lag de 50 on arrive pas à rendre les résidus non corrélés,
#on peut alors laisser de côté cette piste de tendance déterministe car les résidus sont trop fortement corrélés,
#on ne pourra pas interpréter le test ADF

##ON VA ALORS DIFFERENCIER LA SERIE

dserie <- diff(serie,1)
plot(dserie)

#Verifions que notre serie différenciée n'ait pas une tendance
date <- time(dserie)  
summary(lm(coredata(dserie) ~ date))
#beta=0.44,p-val=0.02, on ne peut toujours pas interpréter mais il ne semble pas y avoir de tendance déterministe
#car bien que le coefficient soit significatif, sa valeur absolue est trop faible pour être prise en compte
#de même, R2=0.012 donc le temps ne semble pas être significatif dans la regression
#MAIS RESULTATS NON INTERPRETABLES A CE STADE CAR ON NE SAIT PAS SI LES RESIDUS SONT STATIONNAIRES


#on va directement chercher le lag des résidus qui nous permet de faire un test ADF interprétable
adf <- adfTest_valid(dserie,24,"nc")
Qtests(adf@test$lm$residuals, 24, fitdf = length(adf@test$lm$coefficients))
#on se rend compte qu'à partir du lag 5, les résidus sont décorrélés, on peut alors interpréter le test ADF
#on a bien les résidus qui sont décorrélés d'après le test du Ljungbox car les pval sont suffisamment grandes


adf
#on peut alors interpréter le test ADF et on déduit qu'on a stationnarité de la série différenciée
#en effet la pval est faible donc on rejette H0 donc stationnarité

#on peut vérifier nos résultats avec un test de Perron Phillips
pp.test(dserie)

#on en arrive à la même conclusion, la pval est très faible donc on rejette présence de racine unité
#donc même conclusion : la série différenciée est stationnaire

#on voudrait enfin vérifier la stationnarité par un dernier test, le test KPSS
kpss.test(coredata(dserie), null = "Level")
#p-value = 0.01, ce qui apparait comme une contradiction avec les résultats des 2 tests précédents
#on se souvient que le test KPSS a comme H0 : stationnarité
#on a alors une contradiction car il prévoit de la non stationnarité alors que les 2 autres tests prévoient de la stationnarité
#or KPSS est plus strict par rapport aux faibles variations comme un léger drift ici présent (estimée par OLS)
#alors, on va plutôt conclure étant donné l'allure visuelle de la série différenciée et les résultats des tests ADF et PP,
#à la stationnarité de la série différenciée, avec éventuellement un léger drift mais négligeable en pratique


#### Q3 ####

plot(cbind(serie, dserie), xlab = "Date", main = "Xt et ΔXt")

#### Q4 ####


##On sait que notre série est un ARIMA(p,1,q), on va chercher p et q

#On identifie pmax et qmax avec le PACF et le ACF
par(mfrow=c(1,2))
pacf(dserie,24);acf(dserie,24) #on regarde jusqu'à deux ans de retard
pmax=3 #car après le 3eme pic, les suivants sont pratiquement dans l'intervalle de significativité +/- 1.96/sqrt(n)
qmax=4 #car après le 5eme pic, les suivants sont pratiquement dans l'intervalle de significativité +/- 1.96/sqrt(n)

#on vérifie si notre ARMA(pmax,qmax) a des résidus qui sont pas auto-corrélés
arima304 <- arima(dserie,c(3,0,4)) 

Box.test(arima304$residuals, lag=8, type="Ljung-Box", fitdf=7) #
#p-value = 0.07, on peut pas rejeter à 5% que les résidus sont décorrélés

Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}
round(Qtests(arima304$residuals,24,fitdf=7),3)
#les résidus sont bien décorrélés
#il faudrait théoriquement s'assurer que les résidus suivent bien un bruit blanc, 
#donc d'espérance nulle et de variance constante en plus de la non corrélation vérifiée précédemment
#on peut se contenter de le vérifier visuellement car nous n'avons pas vu d'autres méthodes pour le vérifier

check_residuals_plot <- function(model) {
  res <- residuals(model)
  par(mfrow = c(1,2))
  
  #on trace d'abord l'évolution dans le temps 
  plot(res, type = "l", main = "Résidus dans le temps", ylab = "Résidus")
  
  #puis un histogramme pour comparer avec celui de la loi normale
  hist(res, breaks = 30, main = "Histogramme des résidus", xlab = "Résidus", col = "lightgray")
}

check_residuals_plot(arima304)
#les résidus semblent bien suivrent un bruit blanc


#le modèle est donc valide, mais peut on le simplifier ?

signif <- function(estim){ #fonction de test des significations individuelles des coefficients
  coef <- estim$coef
  se <- sqrt(diag(estim$var.coef))
  t <- coef/se
  pval <- (1-pnorm(abs(t)))*2
  return(rbind(coef,se,pval))
}

signif(arima304) 
#les coefficients de plus haut degré ne sont pas significatifs (ar3 et ma4), 
#on peut alors réduire la complexité du modele

arimafit <- function(estim){
  adjust <- round(signif(estim),3)
  pvals <- Qtests(estim$residuals,24,length(estim$coef)-1)
  pvals <- matrix(apply(matrix(1:24,nrow=6),2,function(c) round(pvals[c,],3)),nrow=6)
  colnames(pvals) <- rep(c("lag", "pval"),4)
  cat("tests de nullite des coefficients :\n")
  print(adjust)
  cat("\n tests d'absence d'autocorrelation des residus : \n")
  print(pvals)
}

#On va alors tester tous les modèles envisageables et garder ceux qui ont :
#des résidus décorrélés et les coefficients de plus haut degré significatifs

estim <- arima(dserie,c(1,0,0)); arimafit(estim)
#on garde pas

estim <- arima(dserie,c(2,0,0)); arimafit(estim)
#on garde pas

estim <- arima(dserie,c(3,0,0)); arimafit(estim)
#on garde pas


estim <- arima(dserie,c(0,0,1)); arimafit(estim)
#on garde pas

estim <- arima(dserie,c(0,0,2)); arimafit(estim)
#on garde pas

estim <- arima(dserie,c(0,0,3)); arimafit(estim)
#on garde pas

estim <- arima(dserie,c(0,0,4)); arimafit(estim)
#on garde pas


estim <- arima(dserie,c(1,0,1)); arimafit(estim)
#on garde pas

estim <- arima(dserie,c(1,0,2)); arimafit(estim)
#on garde pas

estim <- arima(dserie,c(1,0,3)); arimafit(estim)
#on garde pas

estim <- arima(dserie,c(1,0,4)); arimafit(estim)
#on garde pas


estim <- arima(dserie,c(2,0,1)); arimafit(estim)
#on garde pas

estim <- arima(dserie,c(2,0,2)); arimafit(estim)
#on garde
ar2ma2 <- estim

estim <- arima(dserie,c(2,0,3)); arimafit(estim)
#on garde
ar2ma3 <- estim

estim <- arima(dserie,c(2,0,4)); arimafit(estim)
#on garde pas


estim <- arima(dserie,c(3,0,1)); arimafit(estim)
#on garde pas

estim <- arima(dserie,c(3,0,2)); arimafit(estim)
#on garde
ar3ma2 <- estim

estim <- arima(dserie,c(3,0,3)); arimafit(estim)
#on garde pas

#on a gardé seulement 3 modeles parmi les 19 modeles possibles d'origine
models <- c("ar2ma2","ar2ma3","ar3ma2"); names(models) <- models


# Liste des modèles à comparer
models <- list(ar2ma2 = ar2ma2, ar2ma3 = ar2ma3, ar3ma2 = ar3ma2)

#vérifions que parmi les modèles qu'on a gardé, les racines des polynômes AR et MA soient à l'extérieur du cercle unité
#pour le polynôme AR, ça garantie que le processus est stationnaire
#pour le polynôme MA, ça garantie que le modèle est inversible
#on veut les 2 simultanément

check_roots <- function(model) {
  ar_coefs <- model$model$phi
  ma_coefs <- model$model$theta
  
  cat("Polynôme AR :\n")
  print(Mod(polyroot(c(1, -ar_coefs))))  # racines du polynôme AR
  cat("Polynôme MA :\n")
  print(Mod(polyroot(c(1, ma_coefs))))   # racines du polynôme MA
}

plot_roots <- function(model, model_name = "Modèle ARMA") {
  # Extraire les racines (inverse des racines des polynômes caractéristiques)
  ar_roots <- polyroot(c(1, -model$coef[grep("ar", names(model$coef))]))
  ma_roots <- polyroot(c(1, model$coef[grep("ma", names(model$coef))]))
  
  # Tracé du cercle unité
  plot(cos(seq(0, 2*pi, length.out = 100)), sin(seq(0, 2*pi, length.out = 100)),
       type = "l", lty = 2, col = "black", xlab = "Partie réelle", ylab = "Partie imaginaire",
       xlim = c(-2, 2), ylim = c(-2, 2), asp = 1,
       main = paste("Racines inverses du", model_name))
  
  # Ajouter les racines AR et MA
  points(Re(1 / ar_roots), Im(1 / ar_roots), col = "blue", pch = 19)
  points(Re(1 / ma_roots), Im(1 / ma_roots), col = "red", pch = 17)
  
  legend("topright", legend = c("AR roots", "MA roots", "Cercle unité"),
         col = c("blue", "red", "black"), pch = c(19, 17, NA), lty = c(NA, NA, 2))
}


#valeur des racines
check_roots(ar2ma2)
check_roots(ar2ma3)
check_roots(ar3ma2)
#toutes >1 strictement

#on trace ici les inverse donc à s'attend à ce que les racines soient à l'intérieur
#tracer les inverses est plus commode pour ne pas avoir des valeurs trop éloignées
plot_roots(ar2ma2, model_name = "AR2MA2")
plot_roots(ar2ma3, model_name = "AR2MA3")
plot_roots(ar3ma2, model_name = "AR3MA2")

#enfin, pour vérifier qu'un modèle est bien valide, 
#on vérifie visuellement que les résidus sont bien de moyenne nulle et de variance constante

check_residuals_plot(ar2ma2)
check_residuals_plot(ar2ma3)
check_residuals_plot(ar3ma2)


#les 3 modèles passant tous nos tests de vérification de pertinence, on doit maintenant choisir entre les 3


apply(as.matrix(models),1, function(m) c("AIC"=AIC(get(m)), "BIC"=BIC(get(m))))
#on regarde celui qui minimise le AIC et le BIC, c'est ar3ma2 pour les 2 critères d'information

#on peut encore vouloir regarder le R2 ajusté pour préciser notre choix

adj_r2 <- function(model){
  p <- model$arma[1]
  q <- model$arma[2]
  ss_res <- sum(model$residuals^2)
  ss_tot <- sum(dserie[-c(1:max(p,q))]^2)
  n <- model$nobs-max(p,q)
  adj_r2 <- 1-(ss_res/(n-p-q-1))/(ss_tot/(n-1))
  return(adj_r2)
}
adj_r2(ar2ma2)
adj_r2(ar2ma3)
adj_r2(ar3ma2)

#le modele qui maximise le R2 ajusté est encore ar3ma2, ce qui confirme notre choix précédent



#### Q5 ####

#étant donné qu'on a travaillé sur la série différenciée d'ordre 1,
#on considère que notre série initiale est un ARIMA(3,1,2)

#### Q8 ####

#on va faire les prévisions sur notre série initiale, on  veut prévoir Xt+1 et Xt+2
fit <- Arima(serie, order = c(3,1,2))

#à horizon t+2, on veut prendre un intervalle de confiance de 95%
fc <- forecast(fit, h = 2, level = 95)
fc

#les dernières valeurs étaient : janv. 2025  123.06, févr. 2025  119.86,  mars 2025  111.85
#ici on prévoit : Apr 2025 120.08, May 2025 112.26
#les valeurs semblent pertinentes, en revanche les intervalles de confiance sont très larges :
#pour avril : [49 ; 191], pour mai : [29, 196]

#lorsqu'on trace :
autoplot(fc) +
  ggtitle("Prévision à 2 périodes avec intervalle de confiance à 95%") +
  xlab("Temps") + ylab("Valeur") +
  theme_minimal()

#on a fait l'hypothèse que les résidus suivaient une loi normale,
#comparons alors les quantiles des 2 distributions
qqnorm(ar3ma2$residuals, main = "QQ-plot des résidus")
qqline(ar3ma2$residuals, col = "red")
#les quantiles s'éloignent surtout au niveau des queues mais sont similaires entre -1 et 1 