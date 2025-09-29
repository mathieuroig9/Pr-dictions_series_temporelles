# Projet de Séries Temporelles Linéaires

## Contexte et Consignes du Projet

Ce projet s’inscrit dans le cadre du cours de **Séries Temporelles Linéaires** à l’ENSAE Paris.  
L’objectif est de mettre en œuvre les outils économétriques étudiés en cours pour la modélisation et la prévision d’une série temporelle réelle.  

Les consignes étaient les suivantes :
- Le travail devait être réalisé sous **R**. 
- La série étudiée devait être :  
  - extraite du répertoire de l’INSEE,  
  - mensuelle ou trimestrielle,  
  - corrigée des variations saisonnières et des jours ouvrés (CVS-CJO),  
  - comporter au moins 100 observations.  
- Le projet se décomposait en trois parties :  
  1. **Les données** : description, transformations nécessaires pour obtenir une série stationnaire, et représentations graphiques.  
  2. **Modèles ARMA/ARIMA** : identification et validation d’un modèle adapté.  
  3. **Prévision** : production de prévisions à court terme avec intervalles de confiance et discussion des hypothèses.

---

## Méthodologie

### 1. Choix et analyse de la série
- Série choisie : **indice mensuel de la production natinale de vetement de travail** 
- La série est en volume et contient des observations de Janvier 1990 à Mars 2025  
- Analyse visuelle : présence d’une tendance, suggérant une non-stationnarité.  
- Tests de racine unitaire (ADF, Phillips-Perron) sur la série différenciée d’ordre 1 : rejet de l’hypothèse de racine unitaire → la série différenciée est stationnaire.

### 2. Identification du modèle
- Analyse de l’ACF et du PACF sur la série différenciée → les modèles candidats sont de la forme ARMA(p,q) avec p ≤ 3 et q ≤ 4.  
- Estimation des modèles et tests : seuls **ARMA(2,2)**, **ARMA(2,3)** et **ARMA(3,2)** respectent les critères de validité (résidus non autocorrélés, coefficients de plus haut degré significatifs, racines hors du cercle unité).  
- Comparaison AIC/BIC et R² ajusté : le **modèle ARMA(3,2)** est retenu comme le plus pertinent.  
- Conclusion : la série initiale est modélisée par un **ARIMA(3,1,2)**.

### 3. Prévisions 
- Prévisions obtenues :  
  - Avril 2025 : 120.08 (IC95% ≈ [49 ; 191])  
  - Mai 2025 : 112.26 (IC95% ≈ [29 ; 196])  

---

## Résultats Principaux
- La série différenciée d'ordre 1 est stationnaire.  
- Sélection du modèle ARIMA(3,1,2).  
- Prévisions cohérentes mais accompagnées d’intervalles larges.  
- Importance des hypothèses (normalité des résidus, validité du modèle) dans l’interprétation des résultats.
