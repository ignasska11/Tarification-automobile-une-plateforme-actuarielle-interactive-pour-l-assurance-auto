# Tarification-automobile-une-plateforme-actuarielle-interactive-pour-l-assurance-auto
En assurance auto, l'assureur doit fixer un prix juste pour chaque client. Trop bas sur un profil risqué → il perd de l'argent. Trop haut sur un bon conducteur → le client part (eventuellement chez la concurrence). Ce projet construit une plateforme scientifique pour calculer ces prix.

Ce projet implémente ce pipeline de bout en bout sur le jeu de données réel **freMTPL2** (OpenML #41214/#41215) : ~670 000 polices d'assurance auto françaises et ~26 000 sinistres associés.

## Fonctionnalités

- **Modélisation fréquence** : GLM Poisson, GLM Binomiale Négative, XGBoost
- **Modélisation sévérité** : GLM Gamma, GLM Log-Normal, XGBoost
- **Calcul de la prime pure** et tarification par profil type
- **Interprétabilité (XAI)** : coefficients GLM, importance XGBoost, SHAP, PDP, ICE
- **Évaluation** : courbes ROC/AUC, indice de Gini, calibration par décile, backtesting
- **Cartographie interactive** des risques par région (Leaflet)
- **Espace Client** : simulateur de devis personnalisé, position vs portefeuille
- **Espace Assureur** : tableau de bord complet, modèles, rapport HTML exportable

## Stack technique

- **R** / **Shiny** + `bs4Dash` (interface)
- `xgboost`, `MASS` (modélisation)
- `plotly`, `leaflet`, `DT` (visualisation)
- Données via `OpenML`

## Lancer le projet

```r
# Installer les dépendances
install.packages(c("shiny", "bs4Dash", "shinyWidgets", "plotly", "DT",
                    "data.table", "scales", "MASS", "xgboost",
                    "leaflet", "RColorBrewer", "fresh", "OpenML"))

# Lancer l'application
shiny::runApp("app.R")
```

> Note : le premier lancement télécharge les données depuis OpenML et les met en cache localement (`data_cache/`).

## Crédits

Projet personnel réalisé en autoformation, inspiré d'une plateforme similaire développée dans le cadre du Master Actuariat à l'EURIA.
