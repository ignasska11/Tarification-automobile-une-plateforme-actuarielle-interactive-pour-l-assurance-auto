# ==============================================================================
# 03_METRICS_XAI.R
# PDP (Partial Dependence Plot) et ICE (Individual Conditional Expectation)
# pour le modele GLM Binomiale Negative (frequence)
# ==============================================================================
# différence entre PDP et ICE : pour répondre à cette question prenons pour exemple l'effet de l'age 
# PDP répond à la question quel est l'effet de l'âge pour un profil moyen ? tant dis que 
# ICE répond à la question quel est l'effet de l'âge pour 50 profils réels différents, et est-ce que cet effet est le même pour tous ?

library(data.table)
library(dplyr)
library(MASS)

# ------------------------------------------------------------------------------
# 0. CHARGEMENT
# ------------------------------------------------------------------------------

freq   <- readRDS("outputs/freq.rds")
mod_nb <- readRDS("outputs/mod_nb.rds")

# ------------------------------------------------------------------------------
# 1. PROFIL DE BASE (mediane du portefeuille)
# ------------------------------------------------------------------------------
# Toutes les variables sont fixees a une valeur "typique" (mediane / niveau le
# plus frequent). On ne fera varier qu'UNE variable a la fois pour observer
# son effet "toutes choses egales par ailleurs".

base_row <- as.data.frame(freq[1, .(
  DrivAge       = 45L,
  VehAge        = 5L,
  VehPower      = 7L,
  BonusMalus    = 100,
  Density       = 500,
  Area          = Area[1],
  VehBrand      = VehBrand[1],
  VehGas        = VehGas[1],
  groupe_region = groupe_region[1],
  DrivAge_group = DrivAge_group[1],
  Exposure      = 1
)])

# Fonction utilitaire : recalcule DrivAge_group a partir de DrivAge
# (necessaire car le GLM utilise DrivAge_group, pas DrivAge directement)
recalc_age_group <- function(age) {
  factor(dplyr::case_when(
    age <= 25 ~ "Jeune (<=25)",
    age <= 40 ~ "Adulte-Actif (26-40)",
    age <= 60 ~ "Experimente (41-60)",
    TRUE      ~ "Senior (>60)"
  ), levels = levels(freq$DrivAge_group))
}

# ------------------------------------------------------------------------------
# 2. PDP  -  AGE DU CONDUCTEUR
# ------------------------------------------------------------------------------

pdp_age_range <- 18:80
pdp_age <- sapply(pdp_age_range, function(a) {
  r <- base_row
  r$DrivAge       <- a
  r$DrivAge_group <- recalc_age_group(a)
  predict(mod_nb, newdata = r, type = "response")
})
pdp_age_df <- data.frame(DrivAge = pdp_age_range, freq_pred = pdp_age)

# ------------------------------------------------------------------------------
# 3. PDP  -  BONUS-MALUS
# ------------------------------------------------------------------------------

pdp_bm_range <- seq(50, 230, 5)
pdp_bm <- sapply(pdp_bm_range, function(b) {
  r <- base_row
  r$BonusMalus <- b
  predict(mod_nb, newdata = r, type = "response")
})
pdp_bm_df <- data.frame(BonusMalus = pdp_bm_range, freq_pred = pdp_bm)

# ------------------------------------------------------------------------------
# 4. PDP  -  PUISSANCE FISCALE
# ------------------------------------------------------------------------------

pdp_power_range <- 4:15
pdp_power <- sapply(pdp_power_range, function(p) {
  r <- base_row
  r$VehPower <- p
  predict(mod_nb, newdata = r, type = "response")
})
pdp_power_df <- data.frame(VehPower = pdp_power_range, freq_pred = pdp_power)

# ------------------------------------------------------------------------------
# 5. PDP  -  DENSITE DE POPULATION (echelle log)
# ------------------------------------------------------------------------------

pdp_density_range <- c(50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000)
pdp_density <- sapply(pdp_density_range, function(d) {
  r <- base_row
  r$Density <- d
  predict(mod_nb, newdata = r, type = "response")
})
pdp_density_df <- data.frame(Density = pdp_density_range, freq_pred = pdp_density)

# ------------------------------------------------------------------------------
# 6. ICE  -  AGE (50 polices reelles tirees au hasard)
# ------------------------------------------------------------------------------
# Contrairement au PDP (1 profil moyen), l'ICE prend 50 polices REELLES avec
# toutes leurs caracteristiques propres, et fait varier uniquement leur age.
# Cela montre si l'effet de l'age est homogene ou tres variable selon le profil.

set.seed(42)
idx_ice_age <- sample(nrow(freq), 50)
ice_age_base <- as.data.frame(freq[idx_ice_age])
ice_age_base$Exposure <- 1

ice_age_matrix <- sapply(pdp_age_range, function(a) {
  r <- ice_age_base
  r$DrivAge       <- a
  r$DrivAge_group <- recalc_age_group(a)
  predict(mod_nb, newdata = r, type = "response")
})
# ice_age_matrix : 50 lignes (polices) x 63 colonnes (ages 18 a 80)

ice_age_summary <- data.frame(
  DrivAge = pdp_age_range,
  q10     = apply(ice_age_matrix, 2, function(x) quantile(x, .10)),
  q25     = apply(ice_age_matrix, 2, function(x) quantile(x, .25)),
  median  = apply(ice_age_matrix, 2, median),
  q75     = apply(ice_age_matrix, 2, function(x) quantile(x, .75)),
  q90     = apply(ice_age_matrix, 2, function(x) quantile(x, .90))
)

# ------------------------------------------------------------------------------
# 7. ICE  -  BONUS-MALUS (50 polices reelles tirees au hasard)
# ------------------------------------------------------------------------------

set.seed(99)
idx_ice_bm <- sample(nrow(freq), 50)
ice_bm_base <- as.data.frame(freq[idx_ice_bm])
ice_bm_base$Exposure <- 1

ice_bm_matrix <- sapply(pdp_bm_range, function(b) {
  r <- ice_bm_base
  r$BonusMalus <- b
  predict(mod_nb, newdata = r, type = "response")
})

ice_bm_summary <- data.frame(
  BonusMalus = pdp_bm_range,
  q10        = apply(ice_bm_matrix, 2, function(x) quantile(x, .10)),
  q25        = apply(ice_bm_matrix, 2, function(x) quantile(x, .25)),
  median     = apply(ice_bm_matrix, 2, median),
  q75        = apply(ice_bm_matrix, 2, function(x) quantile(x, .75)),
  q90        = apply(ice_bm_matrix, 2, function(x) quantile(x, .90))
)

# ------------------------------------------------------------------------------
# 8. SAUVEGARDE
# ------------------------------------------------------------------------------

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

saveRDS(list(
  pdp_age     = pdp_age_df,
  pdp_bm      = pdp_bm_df,
  pdp_power   = pdp_power_df,
  pdp_density = pdp_density_df,
  ice_age_summary  = ice_age_summary,
  ice_age_matrix   = ice_age_matrix,
  ice_bm_summary   = ice_bm_summary,
  ice_bm_matrix    = ice_bm_matrix,
  base_row         = base_row
), "outputs/pdp_ice.rds")

cat("PDP / ICE calcules et sauvegardes dans outputs/pdp_ice.rds\n")
cat("PDP Age   - min:", round(min(pdp_age_df$freq_pred),4), "max:", round(max(pdp_age_df$freq_pred),4), "\n")
cat("PDP BM    - min:", round(min(pdp_bm_df$freq_pred),4),  "max:", round(max(pdp_bm_df$freq_pred),4), "\n")
