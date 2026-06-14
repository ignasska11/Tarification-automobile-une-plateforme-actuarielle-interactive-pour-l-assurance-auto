# ==============================================================================
# app.R
# Point d'entree principal de l'application AutoActuariat
# Lance avec : shiny::runApp() ou en cliquant "Run App" dans RStudio
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PACKAGES
# ------------------------------------------------------------------------------

library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(plotly)
library(DT)
library(leaflet)
library(data.table)
library(dplyr)
library(scales)
library(MASS)
library(RColorBrewer)

# ------------------------------------------------------------------------------
# 2. CHARGEMENT DES HELPERS
# ------------------------------------------------------------------------------

source("R/ui_helpers.R",     local = TRUE)
source("R/server_helpers.R", local = TRUE)

# ------------------------------------------------------------------------------
# 3. VERIFICATION DES FICHIERS NECESSAIRES
# ------------------------------------------------------------------------------

required_files <- c(
  "outputs/freq.rds",
  "outputs/sev_ok.rds",
  "outputs/mod_nb.rds",
  "outputs/mod_pois.rds",
  "outputs/mod_gamma.rds",
  "outputs/mod_lognorm.rds",
  "outputs/preds_freq.rds",
  "outputs/preds_sev.rds",
  "outputs/metrics_misc.rds",
  "outputs/calib_dt.rds",
  "outputs/test_prime.rds",
  "outputs/profils_type.rds",
  "outputs/shap.rds",
  "outputs/pdp_ice.rds",
  "outputs/region_map.rds",
  "outputs/region_freq.rds"
)

missing <- required_files[!file.exists(required_files)]

if (length(missing) > 0) {
  stop(paste0(
    "\n\n=== FICHIERS MANQUANTS ===\n",
    "Les fichiers suivants sont necessaires mais absents :\n",
    paste0("  - ", missing, collapse = "\n"),
    "\n\nLancez d'abord :\n",
    "  source('R/01_data_prep.R')\n",
    "  source('R/02_modeling.R')\n",
    "  source('R/03_metrics_xai.R')\n"
  ))
}

# ------------------------------------------------------------------------------
# 4. UI & SERVER
# ------------------------------------------------------------------------------

ui <- build_ui()

server <- function(input, output, session) {
  build_server(input, output, session)
}

# ------------------------------------------------------------------------------
# 5. LANCEMENT
# ------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)