# ==============================================================================
# 02_MODELING.R
# Modelisation frequence et severite + metriques + SHAP + prime pure
# ==============================================================================

library(data.table)
library(dplyr)
library(MASS)
library(xgboost)

# ------------------------------------------------------------------------------
# 0. CHARGEMENT DES DONNEES PREPAREES (issues de 01_data_prep.R)
# ------------------------------------------------------------------------------

freq   <- readRDS("outputs/freq.rds")
sev_ok <- readRDS("outputs/sev_ok.rds")

# ------------------------------------------------------------------------------
# 1. SPLIT TRAIN / TEST  -  FREQUENCE
# ------------------------------------------------------------------------------

set.seed(42)
idx_tr     <- sample(nrow(freq), floor(.8 * nrow(freq)))
train_freq <- freq[idx_tr]
test_freq  <- freq[-idx_tr]

# S'assurer que train/test partagent les memes niveaux de facteurs
for (v in c("Area","VehGas","VehBrand","Region","groupe_region","DrivAge_group","BM_group","VehPower_group")) {
  if (v %in% names(train_freq) && v %in% names(test_freq)) {
    lvls <- levels(train_freq[[v]])
    test_freq[[v]]  <- factor(test_freq[[v]],  levels = lvls)
    train_freq[[v]] <- factor(train_freq[[v]], levels = lvls)
  }
}

# ------------------------------------------------------------------------------
# 2. MODELES DE FREQUENCE
# ------------------------------------------------------------------------------

formule_freq <- ClaimNb ~ offset(log(Exposure)) +
  VehAge + VehPower + BonusMalus + Density +
  Area + VehBrand + VehGas + groupe_region + DrivAge_group

# -- GLM Poisson
mod_pois <- glm(formule_freq, family = poisson(link = "log"), data = train_freq)

# -- Indice de surdispersion (variance / moyenne attendue)
disp_pois <- sum(residuals(mod_pois, "pearson")^2) / df.residual(mod_pois)
cat("Surdispersion Poisson :", round(disp_pois, 3), "\n")

# -- GLM Binomiale Negative (gere la surdispersion)
mod_nb <- glm.nb(formule_freq, data = train_freq)
cat("Theta GLM BN :", round(mod_nb$theta, 4), "\n")

# -- XGBoost frequence
prep_xf <- function(dt) {
  X <- model.matrix(~ VehAge + VehPower + BonusMalus + Density +
                      Area + VehBrand + VehGas + groupe_region + DrivAge_group - 1,
                    data = as.data.frame(dt))
  list(X = X, y = dt$ClaimNb, off = log(dt$Exposure))
}
tr_f <- prep_xf(train_freq)
te_f <- prep_xf(test_freq)

# Aligner les colonnes (certaines categories peuvent etre absentes d'un cote)
cols_f <- intersect(colnames(tr_f$X), colnames(te_f$X))
tr_f$X <- tr_f$X[, cols_f]
te_f$X <- te_f$X[, cols_f]

dtrain_f <- xgb.DMatrix(tr_f$X, label = tr_f$y, base_margin = tr_f$off)
dtest_f  <- xgb.DMatrix(te_f$X, label = te_f$y, base_margin = te_f$off)

xgb_freq <- xgb.train(
  params = list(objective = "count:poisson", max_depth = 5, eta = .05,
                subsample = .8, colsample_bytree = .8, min_child_weight = 50,
                eval_metric = "poisson-nloglik"),
  data = dtrain_f, nrounds = 150,
  evals = list(train = dtrain_f, test = dtest_f),
  early_stopping_rounds = 20, verbose = 0)

# -- Predictions sur le jeu de test
pred_pois_f <- predict(mod_pois, newdata = as.data.frame(test_freq), type = "response")
pred_nb_f   <- predict(mod_nb,   newdata = as.data.frame(test_freq), type = "response")
pred_xgb_f  <- predict(xgb_freq, dtest_f)
y_f         <- test_freq$ClaimNb

# ------------------------------------------------------------------------------
# 3. SPLIT TRAIN / TEST  -  SEVERITE
# ------------------------------------------------------------------------------

set.seed(42)
idx_s     <- sample(nrow(sev_ok), floor(.8 * nrow(sev_ok)))
train_sev <- sev_ok[idx_s][!is.na(VehGas) & !is.na(Region) & !is.na(DrivAge)]
test_sev  <- sev_ok[-idx_s][!is.na(VehGas) & !is.na(Region) & !is.na(DrivAge)]

for (v in c("Area","VehGas","VehBrand","Region","groupe_region","DrivAge_group")) {
  if (v %in% names(train_sev) && v %in% names(test_sev)) {
    lvls <- levels(train_sev[[v]])
    test_sev[[v]]  <- factor(test_sev[[v]],  levels = lvls)
    train_sev[[v]] <- factor(train_sev[[v]], levels = lvls)
  }
}

# ------------------------------------------------------------------------------
# 4. MODELES DE SEVERITE
# ------------------------------------------------------------------------------

formule_sev <- ClaimAmount ~ VehGas + VehAge + VehPower + BonusMalus + Area + groupe_region + DrivAge_group

# -- GLM Gamma (lien log) : modele de reference pour la severite
mod_gamma <- glm(formule_sev, family = Gamma(link = "log"), data = train_sev)

# -- GLM Log-Normal (comparaison)
mod_lognorm <- lm(log(ClaimAmount) ~ VehGas + VehAge + VehPower +
                    BonusMalus + Area + groupe_region + DrivAge_group, data = train_sev)
sigma2_logn <- sigma(mod_lognorm)^2

# -- XGBoost severite (sur log du montant)
prep_xs <- function(dt) {
  X <- model.matrix(~ VehGas + VehAge + VehPower +
                      BonusMalus + Area + groupe_region + DrivAge_group - 1,
                    data = as.data.frame(dt))
  list(X = X, y = dt$ClaimAmount)
}
tr_s <- prep_xs(train_sev)
te_s <- prep_xs(test_sev)

cols_s <- intersect(colnames(tr_s$X), colnames(te_s$X))
tr_s$X <- tr_s$X[, cols_s]
te_s$X <- te_s$X[, cols_s]

dtr_s <- xgb.DMatrix(tr_s$X, label = log(tr_s$y))
dte_s <- xgb.DMatrix(te_s$X, label = log(te_s$y))

xgb_sev <- xgb.train(
  params = list(objective = "reg:squarederror", max_depth = 4, eta = .05,
                subsample = .8, colsample_bytree = .8, eval_metric = "rmse"),
  data = dtr_s, nrounds = 100,
  evals = list(train = dtr_s, test = dte_s),
  early_stopping_rounds = 15, verbose = 0)

# -- Predictions
pred_gamma   <- predict(mod_gamma, newdata = as.data.frame(test_sev), type = "response")
pred_lognorm <- exp(predict(mod_lognorm, newdata = as.data.frame(test_sev)) + sigma2_logn / 2) # c'est la correction théorique qui restaure une estimation non biaisée de la moyenne.
pred_xgb_sev <- exp(predict(xgb_sev, dte_s))
y_s          <- test_sev$ClaimAmount

# ------------------------------------------------------------------------------
# 5. METRIQUES
# ------------------------------------------------------------------------------

rmse_fn <- function(y, yh) sqrt(mean((y - yh)^2))
mae_fn  <- function(y, yh) mean(abs(y - yh))

# Indice de Gini (pouvoir discriminant)
gini_fn <- function(y, yh) {
  ord <- order(yh, decreasing = TRUE)
  ly  <- cumsum(y[ord]) / sum(y)
  n   <- length(y)
  lx  <- seq_len(n) / n
  auc <- sum(diff(c(0, lx)) * (c(0, ly[-length(ly)]) + ly) / 2)
  2 * auc - 1
}

# Deviance Poisson / Gamma (qualite d'ajustement)
pdev_fn <- function(y, mu)
  2 * sum(ifelse(y > 0, y * log(y / pmax(mu, 1e-10)) - (y - mu), -(y - mu)))
gdev_fn <- function(y, mu) {
  mu <- pmax(mu, 1e-6)
  2 * sum((y - mu) / mu - log(y / mu))
}

# -- ROC / AUC : predire P(sinistre > 0) a partir des comptages predits
y_bin <- as.integer(test_freq$ClaimNb > 0)

roc_fn <- function(y_true, prob) {
  thresholds <- seq(0, 1, length.out = 200)
  tpr <- sapply(thresholds, function(t) mean((prob >= t)[y_true == 1]))
  fpr <- sapply(thresholds, function(t) mean((prob >= t)[y_true == 0]))
  auc <- sum(diff(rev(fpr)) * (rev(tpr)[-1] + rev(tpr)[-length(tpr)]) / 2, na.rm = TRUE)
  list(fpr = rev(fpr), tpr = rev(tpr), auc = round(abs(auc), 4))
}

prob_pois <- 1 - exp(-pred_pois_f)
prob_nb   <- 1 - exp(-pred_nb_f)
prob_xgb  <- 1 - exp(-pred_xgb_f)

roc_pois <- roc_fn(y_bin, prob_pois)
roc_nb   <- roc_fn(y_bin, prob_nb)
roc_xgb  <- roc_fn(y_bin, prob_xgb)

# ------------------------------------------------------------------------------
# 6. SHAP (XGBoost frequence + severite)
# ------------------------------------------------------------------------------

set.seed(1)
shap_idx <- sample(nrow(te_f$X), min(1000, nrow(te_f$X)))
shap_mat <- predict(xgb_freq,
                    xgb.DMatrix(te_f$X[shap_idx, ], base_margin = te_f$off[shap_idx]),
                    predcontrib = TRUE)
shap_df  <- as.data.frame(shap_mat[, -ncol(shap_mat)])
shap_imp <- data.frame(
  Feature = colnames(shap_df),
  MeanAbsSHAP = colMeans(abs(shap_df))
) %>% arrange(desc(MeanAbsSHAP)) %>% head(20)

set.seed(2)
shap_idx_s <- sample(nrow(te_s$X), min(500, nrow(te_s$X)))
shap_mat_s <- predict(xgb_sev, xgb.DMatrix(te_s$X[shap_idx_s, ]), predcontrib = TRUE)
shap_df_s  <- as.data.frame(shap_mat_s[, -ncol(shap_mat_s)])
shap_imp_s <- data.frame(
  Feature = colnames(shap_df_s),
  MeanAbsSHAP = colMeans(abs(shap_df_s))
) %>% arrange(desc(MeanAbsSHAP)) %>% head(15)

# ------------------------------------------------------------------------------
# 7. PRIME PURE (sur le jeu de test, Exposure ramenee a 1 = 1 an)
# ------------------------------------------------------------------------------

sev_globale <- mean(predict(mod_gamma, newdata = as.data.frame(sev_ok), type = "response"))

test_prime <- copy(test_freq)
test_prime[, Exposure := 1]
test_prime[, freq_nb   := predict(mod_nb,   newdata = as.data.frame(test_prime), type = "response")]
test_prime[, freq_pois := predict(mod_pois, newdata = as.data.frame(test_prime), type = "response")]
test_prime[, prime_nb   := freq_nb   * sev_globale]
test_prime[, prime_pois := freq_pois * sev_globale]
test_prime[, BM_group_prime := cut(BonusMalus, c(49,80,100,120,150,230),
                                   labels = c("<=80","81-100","101-120","121-150",">150"), right = TRUE)]

cat("Prime pure mediane (GLM BN) :", round(median(test_prime$prime_nb), 2), "EUR\n")
cat("Severite globale (GLM Gamma) :", round(sev_globale, 2), "EUR\n")

# ------------------------------------------------------------------------------
# 8. CALIBRATION PAR DECILE
# ------------------------------------------------------------------------------

preds_calib <- predict(mod_nb, newdata = as.data.frame(freq), type = "response")
br_calib    <- unique(quantile(preds_calib, seq(0, 1, .1), na.rm = TRUE))
dec_calib   <- cut(preds_calib, br_calib, include.lowest = TRUE, labels = FALSE)

calib_dt <- data.table(decile = dec_calib, obs = freq$ClaimNb,
                       pred = preds_calib, expo = freq$Exposure)
calib_dt <- calib_dt[!is.na(decile), .(
  Polices   = .N,
  Obs_sin   = sum(obs),
  Pred_sin  = round(sum(pred), 1),
  Freq_obs  = round(sum(obs) / sum(expo), 5),
  Freq_pred = round(sum(pred) / sum(expo), 5)
), by = decile][order(decile)]
calib_dt[, Ratio := round(Obs_sin / Pred_sin, 3)]

# ------------------------------------------------------------------------------
# 9. PROFILS TYPE
# ------------------------------------------------------------------------------

profils_type <- data.table(
  Profil        = c("Jeune urbain","Conducteur moyen rural","Senior experimente","Haut risque BM eleve","Profil standard"),
  DrivAge       = c(22L, 38L, 65L, 35L, 45L),
  VehAge        = c(3L,  5L,  8L,  2L,  6L),
  VehPower      = c(7L,  6L,  5L, 10L,  6L),
  BonusMalus    = c(100, 85, 75, 180, 95),
  Density       = c(5000, 200, 150, 1200, 500),
  Area          = factor("C", levels = levels(freq$Area)),
  VehBrand      = factor("B12", levels = levels(freq$VehBrand)),
  VehGas        = factor("Regular", levels = levels(freq$VehGas)),
  groupe_region = factor("FaibleRisque", levels = levels(freq$groupe_region)),
  DrivAge_group = factor(
    c("Jeune (<=25)","Adulte-Actif (26-40)","Senior (>60)","Adulte-Actif (26-40)","Experimente (41-60)"),
    levels = levels(freq$DrivAge_group)),
  Exposure = 1
)
profils_type[, freq_pred  := predict(mod_nb, newdata = as.data.frame(profils_type), type = "response")]
profils_type[, prime_pure := round(freq_pred * sev_globale, 2)]
profils_type[, prime_TTC  := round(prime_pure * 1.19, 2)]

# ------------------------------------------------------------------------------
# 10. SAUVEGARDE
# ------------------------------------------------------------------------------

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# Modeles
saveRDS(mod_pois,  "outputs/mod_pois.rds")
saveRDS(mod_nb,    "outputs/mod_nb.rds")
saveRDS(xgb_freq,  "outputs/xgb_freq.rds")
saveRDS(mod_gamma, "outputs/mod_gamma.rds")
saveRDS(mod_lognorm, "outputs/mod_lognorm.rds")
saveRDS(xgb_sev,   "outputs/xgb_sev.rds")

# Donnees de test + predictions (pour graphes/metriques dans l'app)
saveRDS(test_freq, "outputs/test_freq.rds")
saveRDS(test_sev,  "outputs/test_sev.rds")
saveRDS(test_prime, "outputs/test_prime.rds")
saveRDS(list(pred_pois_f = pred_pois_f, pred_nb_f = pred_nb_f, pred_xgb_f = pred_xgb_f, y_f = y_f),
        "outputs/preds_freq.rds")
saveRDS(list(pred_gamma = pred_gamma, pred_lognorm = pred_lognorm, pred_xgb_sev = pred_xgb_sev, y_s = y_s),
        "outputs/preds_sev.rds")

# Metriques / divers
saveRDS(list(disp_pois = disp_pois, roc_pois = roc_pois, roc_nb = roc_nb, roc_xgb = roc_xgb,
             sigma2_logn = sigma2_logn, sev_globale = sev_globale),
        "outputs/metrics_misc.rds")
saveRDS(calib_dt, "outputs/calib_dt.rds")
saveRDS(profils_type, "outputs/profils_type.rds")

# SHAP
saveRDS(list(shap_imp = shap_imp, shap_imp_s = shap_imp_s,
             shap_df = shap_df, shap_df_s = shap_df_s,
             te_f_X = te_f$X[shap_idx, ], te_s_X = te_s$X[shap_idx_s, ]),
        "outputs/shap.rds")

# Fonctions de metriques (reutilisables dans l'app)
saveRDS(list(rmse_fn = rmse_fn, mae_fn = mae_fn, gini_fn = gini_fn,
             pdev_fn = pdev_fn, gdev_fn = gdev_fn, roc_fn = roc_fn),
        "outputs/metric_functions.rds")

cat("\n=== RECAPITULATIF ===\n")
cat("RMSE freq  - Poisson:", round(rmse_fn(y_f, pred_pois_f), 5),
    "| BN:", round(rmse_fn(y_f, pred_nb_f), 5),
    "| XGB:", round(rmse_fn(y_f, pred_xgb_f), 5), "\n")
cat("Gini freq  - Poisson:", round(gini_fn(y_f, pred_pois_f), 4),
    "| BN:", round(gini_fn(y_f, pred_nb_f), 4),
    "| XGB:", round(gini_fn(y_f, pred_xgb_f), 4), "\n")
cat("RMSE sev   - Gamma:", round(rmse_fn(y_s, pred_gamma), 0),
    "| LogNorm:", round(rmse_fn(y_s, pred_lognorm), 0),
    "| XGB:", round(rmse_fn(y_s, pred_xgb_sev), 0), "\n")
cat("\nModeles et resultats sauvegardes dans outputs/\n")
