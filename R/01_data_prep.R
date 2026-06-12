# ==============================================================================
# 01_DATA_PREP.R
# Chargement, nettoyage et feature engineering des donnees freMTPL2
# ==============================================================================

library(data.table)
library(dplyr)
library(OpenML)

# ------------------------------------------------------------------------------
# 1. CACHE + CHARGEMENT OPENML (avec retry)
# ------------------------------------------------------------------------------

CACHE_DIR <- file.path(getwd(), "data_cache")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

CACHE_FREQ <- file.path(CACHE_DIR, "freq_raw.rds")
CACHE_SEV  <- file.path(CACHE_DIR, "sev_raw.rds")

load_openml_retry <- function(data_id, cache_file, label, max_tries = 5, wait_sec = 10) {
  if (file.exists(cache_file)) {
    message(label, ": chargement depuis le cache local...")
    return(readRDS(cache_file))
  }
  for (attempt in seq_len(max_tries)) {
    message(label, ": tentative ", attempt, "/", max_tries, " (OpenML id=", data_id, ")...")
    result <- tryCatch({
      ds <- OpenML::getOMLDataSet(data.id = data_id)
      as.data.table(ds$data)
    }, error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("107|Database connection|high server load|try again", msg, ignore.case = TRUE)) {
        message("  Serveur OpenML surcharge. Attente ", wait_sec, " secondes...")
        Sys.sleep(wait_sec)
      } else {
        message("  Erreur inattendue : ", msg)
        Sys.sleep(5)
      }
      NULL
    })
    if (!is.null(result)) {
      message(label, ": OK! Sauvegarde dans le cache...")
      saveRDS(result, cache_file)
      return(result)
    }
  }
  stop("Impossible de telecharger ", label, " apres ", max_tries, " tentatives. ",
       "Verifiez votre connexion internet et reessayez.")
}

freq_raw <- load_openml_retry(41214, CACHE_FREQ, "freMTPL2freq")
sev_raw  <- load_openml_retry(41215, CACHE_SEV,  "freMTPL2sev")

# ------------------------------------------------------------------------------
# 2. TABLE DE CORRESPONDANCE REGIONS (codes -> nouvelles regions + coordonnees)
# ------------------------------------------------------------------------------

region_map <- data.table(
  Region = c("R11","R21","R22","R23","R24","R25","R26","R31","R41","R42","R43",
             "R52","R53","R54","R72","R73","R74","R82","R83","R91","R93","R94"),
  nouvelle_region = c(
    "Ile-de-France","Grand Est","Hauts-de-France","Normandie",
    "Centre-Val de Loire","Normandie","Bourgogne-Franche-Comte",
    "Hauts-de-France","Grand Est","Grand Est","Bourgogne-Franche-Comte",
    "Pays de la Loire","Bretagne","Nouvelle-Aquitaine",
    "Nouvelle-Aquitaine","Occitanie","Nouvelle-Aquitaine",
    "Auvergne-Rhone-Alpes","Auvergne-Rhone-Alpes","Occitanie",
    "Provence-Alpes-Cote d'Azur","Corse"),
  lat = c(48.67,48.58,50.48,49.18,47.58,49.18,47.28,50.48,48.58,48.58,47.28,
          47.78,48.20,45.65,44.83,43.80,45.65,45.75,45.75,43.80,43.93,42.00),
  lon = c(2.33,5.73,2.55,0.37,1.90,0.37,4.83,2.55,5.73,5.73,4.83,
          -1.27,-3.00,-0.33,-0.57,1.43,-0.33,4.85,4.85,1.43,6.08,9.15)
)

# ------------------------------------------------------------------------------
# 3. NETTOYAGE FREQUENCE
# ------------------------------------------------------------------------------

freq <- copy(freq_raw)
freq <- freq[Exposure > 0 & !is.na(Exposure) & !is.na(ClaimNb)]

freq[, Area     := factor(Area)]
freq[, VehBrand := factor(VehBrand)]
freq[, VehGas   := factor(VehGas)]
freq[, Region   := factor(Region)]

freq <- merge(freq, region_map, by = "Region", all.x = TRUE)
freq[, freq_obs := ClaimNb / Exposure]

# Groupes d'age
regrouper_age <- function(x) {
  dplyr::case_when(
    x <= 25 ~ "Jeune (<=25)",
    x <= 40 ~ "Adulte-Actif (26-40)",
    x <= 60 ~ "Experimente (41-60)",
    TRUE    ~ "Senior (>60)"
  )
}
freq[, DrivAge_group := factor(regrouper_age(DrivAge),
                               levels = c("Jeune (<=25)","Adulte-Actif (26-40)","Experimente (41-60)","Senior (>60)"))]

# Groupes Bonus-Malus
freq[, BM_group := cut(BonusMalus, c(49,80,100,120,150,230),
                       labels = c("<=80","81-100","101-120","121-150",">150"), right = TRUE)]

# Groupes puissance fiscale
freq[, VehPower_group := cut(VehPower, c(3,6,9,12,15),
                             labels = c("4-6 CV","7-9 CV","10-12 CV","13-15 CV"), right = TRUE)]

# Groupe de risque region (faible vs eleve, base sur frequence exposee)
region_freq <- freq[, .(freq_exposee = sum(ClaimNb) / sum(Exposure)), by = Region]
region_freq[, groupe_region := ifelse(freq_exposee < 0.10, "FaibleRisque", "RisqueEleve")]
freq <- merge(freq, region_freq[, .(Region, groupe_region)], by = "Region", all.x = TRUE)
freq[, groupe_region := factor(groupe_region)]

# ------------------------------------------------------------------------------
# 4. NETTOYAGE SEVERITE + ENRICHISSEMENT PAR JOINTURE
# ------------------------------------------------------------------------------

sev <- copy(sev_raw)
sev <- sev[ClaimAmount > 0 & !is.na(ClaimAmount)]

sev_enrichi <- merge(sev,
                     freq[, .(IDpol, VehGas, VehBrand, Area, Region, DrivAge, VehAge, VehPower,
                              BonusMalus, Density, nouvelle_region, lat, lon,
                              DrivAge_group, BM_group, groupe_region)],
                     by = "IDpol", all.x = TRUE)

sev_enrichi[, log_ClaimAmount := log(ClaimAmount)]

sev_ok <- sev_enrichi[!is.na(VehGas) & !is.na(DrivAge) & !is.na(Region)]

sev_ok[, VehAge_group := cut(VehAge, c(-1,2,5,10,100),
                             labels = c("0-2 ans","3-5 ans","6-10 ans",">10 ans"), right = TRUE)]
sev_ok[, VehPower_group := cut(VehPower, c(3,6,9,12,15),
                               labels = c("4-6 CV","7-9 CV","10-12 CV","13-15 CV"), right = TRUE)]

# ------------------------------------------------------------------------------
# 5. RECAP + SAUVEGARDE
# ------------------------------------------------------------------------------

cat("Frequence :", nrow(freq), "polices\n")
cat("Severite  :", nrow(sev_ok), "sinistres\n")
cat("Taux de sinistralite :", round(100 * sum(freq$ClaimNb > 0) / nrow(freq), 2), "%\n")

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
saveRDS(freq,   "outputs/freq.rds")
saveRDS(sev_ok, "outputs/sev_ok.rds")
saveRDS(region_map,  "outputs/region_map.rds")
saveRDS(region_freq, "outputs/region_freq.rds")

cat("Donnees preparees et sauvegardees dans outputs/\n")