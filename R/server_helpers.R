# ==============================================================================
# server_helpers.R
# Logique serveur complete : simulateur, dashboard, modeles, XAI, calibration,
# cartographie, profils, rapport
# ==============================================================================

library(data.table)
library(dplyr)
library(plotly)
library(DT)
library(leaflet)
library(scales)
library(RColorBrewer)

# ==============================================================================
# 0. CHARGEMENT DE TOUS LES OBJETS PRECOMPUTES
# ==============================================================================

freq        <- readRDS("outputs/freq.rds")
sev_ok      <- readRDS("outputs/sev_ok.rds")
region_map  <- readRDS("outputs/region_map.rds")
region_freq <- readRDS("outputs/region_freq.rds")

mod_pois    <- readRDS("outputs/mod_pois.rds")
mod_nb      <- readRDS("outputs/mod_nb.rds")
mod_gamma   <- readRDS("outputs/mod_gamma.rds")
mod_lognorm <- readRDS("outputs/mod_lognorm.rds")

preds_freq  <- readRDS("outputs/preds_freq.rds")
preds_sev   <- readRDS("outputs/preds_sev.rds")
metrics     <- readRDS("outputs/metrics_misc.rds")
calib_dt    <- readRDS("outputs/calib_dt.rds")
test_prime  <- readRDS("outputs/test_prime.rds")
profils_type<- readRDS("outputs/profils_type.rds")
shap_data   <- readRDS("outputs/shap.rds")
pdp_ice     <- readRDS("outputs/pdp_ice.rds")

# Raccourcis utiles
pred_pois_f  <- preds_freq$pred_pois_f
pred_nb_f    <- preds_freq$pred_nb_f
pred_xgb_f   <- preds_freq$pred_xgb_f
y_f          <- preds_freq$y_f

pred_gamma   <- preds_sev$pred_gamma
pred_lognorm <- preds_sev$pred_lognorm
pred_xgb_sev <- preds_sev$pred_xgb_sev
y_s          <- preds_sev$y_s

disp_pois    <- metrics$disp_pois
roc_pois     <- metrics$roc_pois
roc_nb       <- metrics$roc_nb
roc_xgb      <- metrics$roc_xgb
sev_globale  <- metrics$sev_globale

shap_imp     <- shap_data$shap_imp
shap_df      <- shap_data$shap_df

# Fonctions metriques
rmse_fn <- function(y, yh) sqrt(mean((y - yh)^2))
mae_fn  <- function(y, yh) mean(abs(y - yh))
gini_fn <- function(y, yh) {
  ord <- order(yh, decreasing = TRUE)
  ly  <- cumsum(y[ord]) / sum(y)
  n   <- length(y)
  lx  <- seq_len(n) / n
  auc <- sum(diff(c(0, lx)) * (c(0, ly[-length(ly)]) + ly) / 2)
  2 * auc - 1
}

# Donnees cartographie
carto_data <- freq[!is.na(nouvelle_region), .(
  f   = sum(ClaimNb) / sum(Exposure),
  nb  = .N,
  bm  = round(mean(BonusMalus), 1),
  lat = first(lat),
  lon = first(lon)
), by = nouvelle_region]

# ==============================================================================
# 1. FONCTION PRINCIPALE DU SERVEUR
# ==============================================================================

build_server <- function(input, output, session) {
  
  # -- Navigation rapide depuis accueil client --------------------------------
  observeEvent(input$go_to_tab, {
    updateTabsetPanel(session, "sidebar_menu", selected = input$go_to_tab)
  })
  observeEvent(input$go_simulateur, {
    updateTabsetPanel(session, "sidebar_menu", selected = "simulateur")
  })
  observeEvent(input$go_position, {
    updateTabsetPanel(session, "sidebar_menu", selected = "client_pos")
  })
  observeEvent(input$btn_home, {
    updateTabsetPanel(session, "sidebar_menu", selected = "accueil_client")
  })
  
  # -- Badge espace nav ------------------------------------------------------
  output$space_badge_nav <- renderUI({
    tab <- input$sidebar_menu %||% "accueil_client"
    client_tabs <- c("accueil_client","simulateur","client_pos")
    is_client <- tab %in% client_tabs
    col <- if (is_client) "#1D9E75" else "#534AB7"
    lbl <- if (is_client) "Espace Client" else "Espace Assureur"
    div(style = paste0(
      "display:flex;align-items:center;gap:6px;",
      "background:", if(is_client)"#E1F5EE" else "#EEEDFE", ";",
      "border:1px solid ", if(is_client)"rgba(29,158,117,0.25)" else "rgba(83,74,183,0.25)", ";",
      "border-radius:20px;padding:4px 12px;",
      "font-size:0.75rem;font-weight:500;color:", col, ";"
    ), lbl)
  })
  
  # ============================================================================
  # 2. SIMULATEUR CLIENT
  # ============================================================================
  
  # Calcul reactif declenche par le bouton
  sim_res <- eventReactive(input$btn_calc, {
    age <- input$s_age
    nd  <- data.frame(
      DrivAge       = age,
      VehAge        = input$s_vehage,
      VehPower      = input$s_power,
      BonusMalus    = input$s_bm,
      Density       = input$s_density,
      Area          = factor(input$s_area, levels = levels(freq$Area)),
      VehBrand      = factor("B12",        levels = levels(freq$VehBrand)),
      VehGas        = factor(input$s_gas,  levels = levels(freq$VehGas)),
      groupe_region = factor("FaibleRisque", levels = levels(freq$groupe_region)),
      DrivAge_group = factor(
        dplyr::case_when(
          age <= 25 ~ "Jeune (<=25)",
          age <= 40 ~ "Adulte-Actif (26-40)",
          age <= 60 ~ "Experimente (41-60)",
          TRUE      ~ "Senior (>60)"
        ), levels = levels(freq$DrivAge_group)),
      Exposure = 1
    )
    freq_pred <- predict(mod_nb, newdata = nd, type = "response")
    pp        <- freq_pred * sev_globale
    ch        <- pp * input$s_charg / 100
    ttc       <- pp + ch
    q33 <- quantile(test_prime$prime_nb, .33, na.rm = TRUE)
    q66 <- quantile(test_prime$prime_nb, .66, na.rm = TRUE)
    pct <- round(100 * sum(test_prime$prime_nb < pp, na.rm = TRUE) / nrow(test_prime), 1)
    list(
      freq = round(freq_pred, 5),
      sev  = round(sev_globale, 0),
      pp   = round(pp, 2),
      ch   = round(ch, 2),
      ttc  = round(ttc, 2),
      risk = ifelse(pp < q33, "FAIBLE", ifelse(pp < q66, "MOYEN", "ELEVE")),
      pct  = pct
    )
  }, ignoreNULL = FALSE)
  
  # Carte resultat principale
  output$sim_result_card <- renderUI({
    r     <- sim_res()
    r_col <- switch(r$risk, "FAIBLE" = "#1D9E75", "MOYEN" = "#BA7517", "ELEVE" = "#E24B4A")
    r_cls <- switch(r$risk, "FAIBLE" = "risk-low", "MOYEN" = "risk-medium", "ELEVE" = "risk-high")
    r_ico <- switch(r$risk, "FAIBLE" = "shield-check", "MOYEN" = "alert-circle", "ELEVE" = "alert-triangle")
    
    div(class = "prime-result-card",
        div(class = "prime-big-label", "Prime commerciale annuelle estimee"),
        div(class = "prime-big-value", paste0(format(r$ttc, big.mark = " "), " EUR")),
        div(style = "max-width:340px; margin:0 auto;",
            div(class = "prime-detail-row",
                span("Frequence predite (GLM BN)"),
                span(paste0(round(r$freq * 100, 3), "%"))),
            div(class = "prime-detail-row",
                span("Severite estimee (GLM Gamma)"),
                span(paste0(format(r$sev, big.mark = " "), " EUR"))),
            div(class = "prime-detail-row",
                span("Prime pure"),
                span(paste0(format(r$pp, big.mark = " "), " EUR"))),
            div(class = "prime-detail-row",
                span(paste0("Chargements (", input$s_charg, "%)")),
                span(paste0(format(r$ch, big.mark = " "), " EUR")))
        ),
        div(class = paste("risk-badge", r_cls),
            tags$i(class = paste0("fas fa-", r_ico)),
            paste0("Risque ", r$risk, " — Percentile ", r$pct, "%")
        )
    )
  })
  
  # Graphique : position vs portefeuille
  output$sim_position <- renderPlotly({
    val  <- sim_res()$pp
    pp_d <- test_prime$prime_nb
    q99p <- quantile(pp_d, .99, na.rm = TRUE)
    PT(plot_ly() %>%
         add_histogram(x = pp_d[pp_d < q99p], nbinsx = 50,
                       marker = list(color = "rgba(83,74,183,0.35)",
                                     line = list(color = "rgba(83,74,183,0.6)", width = 0.5)),
                       name = "Portefeuille") %>%
         add_segments(x = val, xend = val, y = 0, yend = 3000,
                      line = list(color = COL_TEAL, dash = "dash", width = 2.5),
                      name = "Votre prime") %>%
         layout(
           xaxis = list(title = "Prime pure (EUR)"),
           yaxis = list(title = ""),
           showlegend = TRUE))
  })
  
  # Graphique : decomposition prime (donut)
  output$sim_decomp <- renderPlotly({
    r <- sim_res()
    PT(plot_ly(
      labels = c("Prime pure", "Chargements"),
      values = c(r$pp, r$ch),
      type   = "pie", hole = .58,
      textinfo  = "label+percent",
      rotation  = 180,
      marker = list(
        colors = c(COL_VIOLET, COL_TEAL),
        line   = list(color = "#FFFFFF", width = 2))
    ))
  })
  
  # Conseils personnalises
  output$sim_conseil <- renderUI({
    r   <- sim_res()
    bm  <- input$s_bm
    age <- input$s_age
    txt <- paste0(
      if (bm > 130) paste0("<strong>Bonus-Malus eleve (", bm, ") :</strong> chaque annee sans sinistre vous fera baisser de ~5 points. ")
      else paste0("<strong>Bon Bonus-Malus (", bm, ") :</strong> vous beneficiez d'une tarification favorable. "),
      if (age <= 25) "<strong>Jeune conducteur :</strong> la prime diminuera avec l'experience acquise. ",
      "<br>Prime estimee : <strong>", r$ttc, " EUR</strong> — vous etes au percentile <strong>", r$pct, "%</strong> du portefeuille."
    )
    INSIGHT(txt, type = "teal", title = "Conseils personnalises")
  })
  
  # ============================================================================
  # 3. POSITION CLIENT
  # ============================================================================
  
  output$client_bm_pos <- renderPlotly({
    d <- freq[!is.na(BM_group), .(f = round(sum(ClaimNb)/sum(Exposure), 4)), by = BM_group][order(BM_group)]
    pal <- c(COL_TEAL, "#5DCAA5", COL_AMBER, COL_CORAL, COL_DANGER)
    user_bm_f <- tryCatch(
      freq[BonusMalus == input$s_bm, mean(freq_obs, na.rm = TRUE)],
      error = function(e) freq[, mean(freq_obs, na.rm = TRUE)]
    )
    if (is.nan(user_bm_f) || is.na(user_bm_f))
      user_bm_f <- freq[, mean(freq_obs, na.rm = TRUE)]
    PT(plot_ly(d, x = ~BM_group, y = ~f, type = "bar",
               marker = list(color = pal[seq_len(nrow(d))]),
               text = ~round(f, 4), textposition = "outside") %>%
         add_segments(x = 0.5, xend = nrow(d) + 0.5,
                      y = user_bm_f, yend = user_bm_f,
                      line = list(color = COL_VIOLET, dash = "dash", width = 2.5),
                      name = "Votre frequence") %>%
         layout(xaxis = list(title = "Classe Bonus-Malus"),
                yaxis = list(title = "Frequence exposee"), showlegend = TRUE))
  })
  
  output$client_age_pos <- renderPlotly({
    d <- freq[DrivAge >= 18 & DrivAge <= 80, .(f = sum(ClaimNb)/sum(Exposure)), by = DrivAge][order(DrivAge)]
    user_f <- tryCatch(
      predict(mod_nb, newdata = data.frame(
        DrivAge = input$s_age, VehAge = input$s_vehage, VehPower = input$s_power,
        BonusMalus = input$s_bm, Density = input$s_density,
        Area = factor(input$s_area, levels = levels(freq$Area)),
        VehBrand = factor("B12", levels = levels(freq$VehBrand)),
        VehGas = factor(input$s_gas, levels = levels(freq$VehGas)),
        groupe_region = factor("FaibleRisque", levels = levels(freq$groupe_region)),
        DrivAge_group = factor(dplyr::case_when(
          input$s_age <= 25 ~ "Jeune (<=25)", input$s_age <= 40 ~ "Adulte-Actif (26-40)",
          input$s_age <= 60 ~ "Experimente (41-60)", TRUE ~ "Senior (>60)"),
          levels = levels(freq$DrivAge_group)),
        Exposure = 1), type = "response"),
      error = function(e) NULL)
    p <- PT(plot_ly(d, x = ~DrivAge, y = ~f, type = "scatter", mode = "lines",
                    fill = "tozeroy", fillcolor = "rgba(83,74,183,0.08)",
                    line = list(color = COL_VIOLET, width = 2)))
    if (!is.null(user_f))
      p <- p %>% add_markers(x = input$s_age, y = user_f,
                             marker = list(color = COL_TEAL, size = 12, symbol = "star",
                                           line = list(color = "#FFFFFF", width = 1.5)),
                             name = "Vous")
    p %>% layout(xaxis = list(title = "Age"), yaxis = list(title = "Frequence predite"))
  })
  
  output$client_zone_pos <- renderPlotly({
    d <- test_prime[!is.na(Area), .(m = round(median(prime_nb, na.rm = TRUE), 0)), by = Area][order(Area)]
    pal  <- brewer.pal(max(3, nrow(d)), "Purples")[seq_len(nrow(d))]
    cols <- ifelse(d$Area == input$s_area, COL_CORAL, pal)
    PT(plot_ly(d, x = ~Area, y = ~m, type = "bar",
               marker = list(color = cols),
               text = ~paste0(scales::comma(m), " EUR"), textposition = "outside") %>%
         layout(xaxis = list(title = "Zone"), yaxis = list(title = "Prime pure mediane (EUR)")))
  })
  
  output$client_conseil_full <- renderUI({
    r   <- sim_res()
    bm  <- input$s_bm
    age <- input$s_age
    txt <- paste0(
      "<strong>Votre prime estimee : ", r$ttc, " EUR</strong><br><br>",
      if (bm > 130) paste0("Bonus-Malus eleve (", bm, ") — pensez a conduire prudemment pour le reduire.<br>")
      else paste0("Votre Bonus-Malus (", bm, ") est favorable.<br>"),
      if (age <= 25) "En tant que jeune conducteur, votre prime est majoree. Elle diminuera avec l'experience.<br>"
      else if (age > 60) "Profil senior : exposition moderee mais cout par sinistre potentiellement plus eleve.<br>"
      else "Votre tranche d'age presente une sinistralite moderee.<br>",
      "<br>Vous etes dans le <strong>percentile ", r$pct, "%</strong> du portefeuille."
    )
    INSIGHT(txt, type = "teal", title = "Analyse personnalisee")
  })
  
  # ============================================================================
  # 4. KPIS ASSUREUR
  # ============================================================================
  
  output$kpi_polices_acc   <- renderText({ scales::comma(nrow(freq)) })
  output$kpi_sinistres_acc <- renderText({ scales::comma(sum(freq$ClaimNb)) })
  output$kpi_freq_acc      <- renderText({ paste0(round(100 * sum(freq$ClaimNb) / sum(freq$Exposure), 2), "%") })
  output$kpi_prime_acc     <- renderText({ paste0(round(median(test_prime$prime_nb, na.rm = TRUE), 0), " EUR") })
  
  # ============================================================================
  # 5. DASHBOARD
  # ============================================================================
  
  output$vb_polices   <- renderValueBox({ VBOX(scales::comma(nrow(freq)),      "Polices",          "file-contract", "violet") })
  output$vb_freq_obs  <- renderValueBox({ VBOX(paste0(round(100*sum(freq$ClaimNb)/sum(freq$Exposure),2),"%"), "Frequence observee", "car-burst",  "coral") })
  output$vb_sev_moy   <- renderValueBox({ VBOX(paste0(round(mean(sev_ok$ClaimAmount),0)," EUR"),  "Severite moyenne", "coins",      "amber") })
  output$vb_prime_med <- renderValueBox({ VBOX(paste0(round(median(test_prime$prime_nb,na.rm=TRUE),0)," EUR"), "Prime pure med.", "euro-sign","violet") })
  output$vb_gini_xgb  <- renderValueBox({ VBOX(round(gini_fn(y_f,pred_xgb_f),4), "Gini XGBoost",   "chart-line",  "teal") })
  output$vb_auc_best  <- renderValueBox({ VBOX(max(roc_pois$auc,roc_nb$auc,roc_xgb$auc), "AUC meilleur", "trophy",   "teal") })
  output$vb_disp      <- renderValueBox({ VBOX(round(disp_pois,3), "Surdispersion Poisson", "triangle-exclamation", "coral") })
  output$vb_theta     <- renderValueBox({
    mod_nb_obj <- readRDS("outputs/mod_nb.rds")
    VBOX(round(mod_nb_obj$theta, 3), "Theta GLM BN", "star", "violet")
  })
  
  output$dash_freq_age <- renderPlotly({
    d <- freq[!is.na(DrivAge_group), .(
      f = sum(ClaimNb)/sum(Exposure), n = .N
    ), by = DrivAge_group][order(DrivAge_group)]
    pal <- c(COL_CORAL, COL_TEAL, COL_VIOLET, COL_AMBER)
    PT(plot_ly(d, x = ~DrivAge_group, y = ~f, type = "bar",
               marker = list(color = pal, line = list(color = "rgba(255,255,255,0.5)", width = 0.5)),
               text = ~round(f, 4), textposition = "outside",
               hovertemplate = "<b>%{x}</b><br>Frequence: %{y:.4f}<br>Polices: %{customdata}<extra></extra>",
               customdata = ~scales::comma(n)) %>%
         layout(xaxis = list(title = "Tranche d'age"),
                yaxis = list(title = "Frequence exposee"), showlegend = FALSE))
  })
  
  output$dash_donut <- renderPlotly({
    d <- freq[, .N, by = .(s = ifelse(ClaimNb == 0, "Sans sinistre", "Avec sinistre"))]
    pct_sin <- round(100 * d[s != "Sans sinistre", N] / sum(d$N), 2)
    PT(plot_ly(d, labels = ~s, values = ~N, type = "pie", hole = .62,
               textinfo = "none",
               marker = list(colors = c(COL_CORAL, "#EEEDFE"),
                             line = list(color = "#FFFFFF", width = 2))) %>%
         layout(showlegend = TRUE,
                annotations = list(list(
                  text = paste0("<b>", pct_sin, "%</b>"),
                  x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                  showarrow = FALSE,
                  font = list(size = 18, color = COL_CORAL)))))
  })
  
  output$dash_bm <- renderPlotly({
    d <- freq[!is.na(BM_group), .(f = round(sum(ClaimNb)/sum(Exposure), 4)), by = BM_group][order(BM_group)]
    pal <- c(COL_TEAL, "#5DCAA5", COL_AMBER, COL_CORAL, COL_DANGER)
    ref <- round(sum(freq$ClaimNb)/sum(freq$Exposure), 4)
    PT(plot_ly(d, x = ~BM_group, y = ~f, type = "bar",
               marker = list(color = pal[seq_len(nrow(d))]),
               text = ~f, textposition = "outside") %>%
         add_segments(x = 0.5, xend = nrow(d)+0.5, y = ref, yend = ref,
                      line = list(color = COL_VIOLET, dash = "dash", width = 1.5), showlegend = FALSE) %>%
         layout(xaxis = list(title = "Bonus-Malus"), yaxis = list(title = "Frequence"), showlegend = FALSE))
  })
  
  output$dash_zone <- renderPlotly({
    d <- freq[, .(f = round(sum(ClaimNb)/sum(Exposure), 4)), by = Area][order(Area)]
    pal <- brewer.pal(max(3, nrow(d)), "Purples")[seq_len(nrow(d))]
    PT(plot_ly(d, x = ~Area, y = ~f, type = "bar",
               marker = list(color = pal), text = ~f, textposition = "outside") %>%
         layout(xaxis = list(title = "Zone (A=rural -> F=metropole)"),
                yaxis = list(title = "Frequence"), showlegend = FALSE))
  })
  
  output$dash_lorenz <- renderPlotly({
    mk_l <- function(y, yh) {
      ord <- order(yh)
      lx  <- seq(0, 1, length.out = length(y) + 1)
      list(x = lx, y = c(0, cumsum(y[ord]) / sum(y)))
    }
    l1 <- mk_l(y_f, pred_pois_f)
    l2 <- mk_l(y_f, pred_nb_f)
    l3 <- mk_l(y_f, pred_xgb_f)
    PT(plot_ly() %>%
         add_lines(x=l1$x, y=l1$y, name="Poisson",  line=list(color=COL_GRAY,   width=1.5)) %>%
         add_lines(x=l2$x, y=l2$y, name="GLM BN",   line=list(color=COL_TEAL,   width=2)) %>%
         add_lines(x=l3$x, y=l3$y, name="XGBoost",  line=list(color=COL_VIOLET, width=2.5)) %>%
         add_lines(x=c(0,1), y=c(0,1), name="Aleatoire", line=list(color=COL_GRAY, dash="dash")) %>%
         layout(xaxis=list(title="Prop. polices"), yaxis=list(title="Prop. sinistres")))
  })
  
  # ============================================================================
  # 6. MODELES  -  COMPARAISON
  # ============================================================================
  
  output$tbl_comp_freq <- renderDT({
    df <- data.frame(
      Modele = c("GLM Poisson","GLM Bin. Negative","XGBoost"),
      RMSE   = round(c(rmse_fn(y_f,pred_pois_f), rmse_fn(y_f,pred_nb_f), rmse_fn(y_f,pred_xgb_f)), 5),
      MAE    = round(c(mae_fn(y_f,pred_pois_f),  mae_fn(y_f,pred_nb_f),  mae_fn(y_f,pred_xgb_f)),  5),
      Gini   = round(c(gini_fn(y_f,pred_pois_f), gini_fn(y_f,pred_nb_f), gini_fn(y_f,pred_xgb_f)), 4),
      AUC    = c(roc_pois$auc, roc_nb$auc, roc_xgb$auc),
      AIC    = c(round(AIC(mod_pois),0), round(AIC(mod_nb),0), NA)
    )
    datatable(df, options = list(dom = "t", pageLength = 4),
              rownames = FALSE, class = "compact hover") %>%
      formatStyle("Modele", fontWeight = "bold", color = COL_VIOLET) %>%
      formatStyle("Gini",   color = COL_TEAL,   fontWeight = "bold") %>%
      formatStyle("AUC",    color = COL_AMBER,   fontWeight = "bold")
  })
  
  output$tbl_comp_sev <- renderDT({
    df <- data.frame(
      Modele = c("GLM Gamma","GLM Log-Normal","XGBoost"),
      RMSE   = round(c(rmse_fn(y_s,pred_gamma), rmse_fn(y_s,pred_lognorm), rmse_fn(y_s,pred_xgb_sev)), 0),
      MAE    = round(c(mae_fn(y_s,pred_gamma),  mae_fn(y_s,pred_lognorm),  mae_fn(y_s,pred_xgb_sev)),  0),
      AIC    = c(round(AIC(mod_gamma),0), round(AIC(mod_lognorm),0), NA)
    )
    datatable(df, options = list(dom = "t", pageLength = 4),
              rownames = FALSE, class = "compact hover") %>%
      formatStyle("Modele", fontWeight = "bold", color = COL_TEAL)
  })
  
  output$lorenz_3 <- renderPlotly({
    mk_l <- function(y, yh) {
      ord <- order(yh); lx <- seq(0,1,length.out=length(y)+1)
      list(x=lx, y=c(0,cumsum(y[ord])/sum(y)))
    }
    l1<-mk_l(y_f,pred_pois_f); l2<-mk_l(y_f,pred_nb_f); l3<-mk_l(y_f,pred_xgb_f)
    PT(plot_ly() %>%
         add_lines(x=l1$x,y=l1$y,name="Poisson", line=list(color=COL_GRAY,  width=1.5)) %>%
         add_lines(x=l2$x,y=l2$y,name="GLM BN",  line=list(color=COL_TEAL,  width=2)) %>%
         add_lines(x=l3$x,y=l3$y,name="XGBoost", line=list(color=COL_VIOLET,width=2.5)) %>%
         add_lines(x=c(0,1),y=c(0,1),name="Aleatoire",line=list(color=COL_GRAY,dash="dash")) %>%
         layout(xaxis=list(title="Prop. polices"),yaxis=list(title="Prop. sinistres")))
  })
  
  output$roc_plot <- renderPlotly({
    PT(plot_ly() %>%
         add_lines(x=roc_pois$fpr, y=roc_pois$tpr,
                   name=paste0("Poisson (AUC=",roc_pois$auc,")"),
                   line=list(color=COL_GRAY, width=2)) %>%
         add_lines(x=roc_nb$fpr, y=roc_nb$tpr,
                   name=paste0("GLM BN (AUC=",roc_nb$auc,")"),
                   line=list(color=COL_TEAL, width=2.5)) %>%
         add_lines(x=roc_xgb$fpr, y=roc_xgb$tpr,
                   name=paste0("XGBoost (AUC=",roc_xgb$auc,")"),
                   line=list(color=COL_VIOLET, width=2.5)) %>%
         add_lines(x=c(0,1), y=c(0,1), name="Aleatoire",
                   line=list(color=COL_GRAY, dash="dash", width=1)) %>%
         layout(xaxis=list(title="FPR", range=c(0,1)),
                yaxis=list(title="TPR", range=c(0,1))))
  })
  
  output$scatter_obs_sev <- renderPlotly({
    idx <- sample(length(y_s), min(500, length(y_s)))
    PT(plot_ly(x=pred_gamma[idx], y=y_s[idx], type="scatter", mode="markers",
               marker=list(color=COL_VIOLET, size=4, opacity=.45)) %>%
         add_lines(x=c(0,max(pred_gamma[idx])), y=c(0,max(pred_gamma[idx])),
                   line=list(color=COL_CORAL, dash="dash", width=1.5), name="Droite y=x") %>%
         layout(xaxis=list(title="Predit (Gamma)"), yaxis=list(title="Observe")))
  })
  
  output$prime_bm_line <- renderPlotly({
    bms  <- seq(50, 230, 5)
    base <- as.data.frame(freq[1, .(
      DrivAge=45L, VehAge=5L, VehPower=7L, BonusMalus=100,
      Density=500, Area=Area[1], VehBrand=VehBrand[1], VehGas=VehGas[1],
      groupe_region=groupe_region[1], DrivAge_group=DrivAge_group[1], Exposure=1)])
    preds <- sapply(bms, function(b) {
      r <- base; r$BonusMalus <- b
      predict(mod_nb, newdata=r, type="response") * sev_globale
    })
    PT(plot_ly(x=bms, y=round(preds,0), type="scatter", mode="lines",
               fill="tozeroy", fillcolor="rgba(83,74,183,0.08)",
               line=list(color=COL_VIOLET, width=2.5)) %>%
         add_segments(x=100,xend=100,y=min(preds),yend=max(preds),
                      line=list(color=COL_TEAL,dash="dash",width=1.5),name="BM=100") %>%
         layout(xaxis=list(title="Bonus-Malus"),yaxis=list(title="Prime pure (EUR)")))
  })
  
  # ============================================================================
  # 7. XAI  -  SHAP + COEFFICIENTS + PDP + ICE
  # ============================================================================
  
  output$shap_bar <- renderPlotly({
    d <- shap_imp %>% mutate(pct = round(100*MeanAbsSHAP/sum(MeanAbsSHAP), 1))
    PT(plot_ly(d, x=~MeanAbsSHAP, y=~reorder(Feature,MeanAbsSHAP),
               type="bar", orientation="h",
               marker=list(color=~MeanAbsSHAP,
                           colorscale=list(c(0,"rgba(83,74,183,0.3)"),c(0.5,COL_VIOLET),c(1,COL_CORAL)),
                           showscale=FALSE),
               text=~paste0(pct,"%"), textposition="outside") %>%
         layout(xaxis=list(title="Mean |SHAP|"), yaxis=list(title="")))
  })
  
  output$coef_plot <- renderPlotly({
    co <- as.data.frame(summary(mod_nb)$coefficients)
    colnames(co) <- c("Beta","SE","z","p")
    co$Variable  <- rownames(co)
    d <- co[co$Variable != "(Intercept)" & abs(co$z) > 2, ] %>%
      arrange(Beta) %>% tail(20)
    d$expB <- round(exp(d$Beta), 3)
    d$lbl  <- paste0(ifelse(d$expB > 1, "+", ""), round((d$expB-1)*100, 1), "%")
    PT(plot_ly(d, x=~expB, y=~reorder(Variable,expB), type="bar", orientation="h",
               marker=list(color=~ifelse(expB>1, COL_CORAL, COL_TEAL)),
               text=~lbl, textposition="outside") %>%
         layout(xaxis=list(title="exp(beta) — Multiplicateur de frequence"), yaxis=list(title=""),
                shapes=list(list(type="line",x0=1,x1=1,y0=-.5,y1=nrow(d)-.5,
                                 line=list(color=COL_GRAY,dash="dash",width=1.5),xref="x",yref="y"))))
  })
  
  output$pdp_age <- renderPlotly({
    d <- pdp_ice$pdp_age
    PT(plot_ly(d, x=~DrivAge, y=~freq_pred, type="scatter", mode="lines",
               fill="tozeroy", fillcolor="rgba(83,74,183,0.06)",
               line=list(color=COL_VIOLET, width=2.5)) %>%
         add_segments(x=25,xend=25,y=min(d$freq_pred),yend=max(d$freq_pred),
                      line=list(color=COL_AMBER,dash="dash",width=1.5),name="25 ans") %>%
         add_segments(x=60,xend=60,y=min(d$freq_pred),yend=max(d$freq_pred),
                      line=list(color=COL_TEAL,dash="dash",width=1.5),name="60 ans") %>%
         layout(xaxis=list(title="Age conducteur"),yaxis=list(title="Frequence predite")))
  })
  
  output$pdp_bm <- renderPlotly({
    d <- pdp_ice$pdp_bm
    PT(plot_ly(d, x=~BonusMalus, y=~freq_pred, type="scatter", mode="lines",
               fill="tozeroy", fillcolor="rgba(186,117,23,0.08)",
               line=list(color=COL_AMBER, width=2.5)) %>%
         add_segments(x=100,xend=100,y=min(d$freq_pred),yend=max(d$freq_pred),
                      line=list(color=COL_TEAL,dash="dash",width=1.5),name="BM=100") %>%
         layout(xaxis=list(title="Bonus-Malus"),yaxis=list(title="Frequence predite")))
  })
  
  output$ice_age <- renderPlotly({
    s   <- pdp_ice$ice_age_summary
    mat <- pdp_ice$ice_age_matrix
    p   <- PT(plot_ly()) %>%
      add_ribbons(x=s$DrivAge, ymin=s$q10, ymax=s$q90,
                  fillcolor="rgba(83,74,183,0.1)", line=list(color="transparent"), name="P10-P90") %>%
      add_ribbons(x=s$DrivAge, ymin=s$q25, ymax=s$q75,
                  fillcolor="rgba(83,74,183,0.18)", line=list(color="transparent"), name="IQR 25-75")
    for (i in seq_len(min(nrow(mat), 50))) {
      p <- p %>% add_lines(x=18:80, y=mat[i,],
                           line=list(color="rgba(83,74,183,0.18)",width=0.7), showlegend=FALSE)
    }
    p %>%
      add_lines(x=s$DrivAge, y=s$median, name="Mediane",
                line=list(color=COL_VIOLET, width=2.5)) %>%
      layout(xaxis=list(title="Age conducteur"),yaxis=list(title="Frequence predite"))
  })
  
  output$ice_bm <- renderPlotly({
    s   <- pdp_ice$ice_bm_summary
    mat <- pdp_ice$ice_bm_matrix
    bms <- seq(50, 230, 5)
    p   <- PT(plot_ly()) %>%
      add_ribbons(x=s$BonusMalus, ymin=s$q10, ymax=s$q90,
                  fillcolor="rgba(186,117,23,0.1)", line=list(color="transparent"), name="P10-P90") %>%
      add_ribbons(x=s$BonusMalus, ymin=s$q25, ymax=s$q75,
                  fillcolor="rgba(186,117,23,0.18)", line=list(color="transparent"), name="IQR 25-75")
    for (i in seq_len(min(nrow(mat), 50))) {
      p <- p %>% add_lines(x=bms, y=mat[i,],
                           line=list(color="rgba(186,117,23,0.18)",width=0.7), showlegend=FALSE)
    }
    p %>%
      add_lines(x=s$BonusMalus, y=s$median, name="Mediane",
                line=list(color=COL_AMBER, width=2.5)) %>%
      add_segments(x=100,xend=100,y=min(s$q10),yend=max(s$q90),
                   line=list(color=COL_TEAL,dash="dash",width=1.5),name="BM=100") %>%
      layout(xaxis=list(title="Bonus-Malus"),yaxis=list(title="Frequence predite"))
  })
  
  # ============================================================================
  # 8. CALIBRATION
  # ============================================================================
  
  output$tbl_calib <- renderDT({
    datatable(calib_dt,
              options = list(dom="t", pageLength=12),
              rownames=FALSE, class="compact hover") %>%
      formatStyle("Ratio",
                  color = styleInterval(c(.9,1.1),
                                        c(COL_DANGER, COL_TEAL, COL_CORAL)),
                  fontWeight="bold")
  })
  
  output$plot_calib_bar <- renderPlotly({
    PT(plot_ly(calib_dt, x=~decile, y=~Ratio, type="bar",
               marker=list(color=~Ratio,
                           colorscale=list(c(0,COL_DANGER),c(0.5,COL_TEAL),c(1,COL_AMBER)),
                           showscale=FALSE)) %>%
         add_lines(x=~decile, y=rep(1,nrow(calib_dt)),
                   line=list(color=COL_VIOLET,dash="dash",width=1.5), name="Ratio=1") %>%
         add_lines(x=~decile, y=rep(0.9,nrow(calib_dt)),
                   line=list(color=COL_TEAL,dash="dot",width=1), name="0.90") %>%
         add_lines(x=~decile, y=rep(1.1,nrow(calib_dt)),
                   line=list(color=COL_TEAL,dash="dot",width=1), name="1.10") %>%
         layout(xaxis=list(title="Decile"),yaxis=list(title="Ratio O/P")))
  })
  
  output$lorenz_all <- renderPlotly({
    mk_l <- function(y,yh){ord<-order(yh);lx<-seq(0,1,length.out=length(y)+1);list(x=lx,y=c(0,cumsum(y[ord])/sum(y)))}
    l2<-mk_l(y_f,pred_nb_f); l3<-mk_l(y_f,pred_xgb_f)
    PT(plot_ly() %>%
         add_lines(x=l2$x,y=l2$y,name="GLM BN",  line=list(color=COL_TEAL,  width=2)) %>%
         add_lines(x=l3$x,y=l3$y,name="XGBoost", line=list(color=COL_VIOLET,width=2.5)) %>%
         add_lines(x=c(0,1),y=c(0,1),name="Aleatoire",line=list(color=COL_GRAY,dash="dash")) %>%
         layout(xaxis=list(title="Prop. polices"),yaxis=list(title="Prop. sinistres")))
  })
  
  output$double_lift <- renderPlotly({
    ratio <- pred_xgb_f / pmax(pred_nb_f, 1e-8)
    br    <- unique(quantile(ratio, seq(0,1,.1), na.rm=TRUE))
    if(length(br) < 3) return(PT(plot_ly()))
    grp   <- cut(ratio, br, include.lowest=TRUE, labels=FALSE)
    d     <- data.frame(grp=grp, obs=y_f, xgb=pred_xgb_f, nb=pred_nb_f) %>%
      filter(!is.na(grp)) %>%
      group_by(grp) %>%
      summarise(obs=mean(obs), xgb=mean(xgb), nb=mean(nb), .groups="drop")
    PT(plot_ly(d) %>%
         add_lines(x=~grp,y=~obs,name="Observe", line=list(color=COL_GRAY,  width=2)) %>%
         add_lines(x=~grp,y=~nb, name="GLM BN",  line=list(color=COL_TEAL,  width=2)) %>%
         add_lines(x=~grp,y=~xgb,name="XGBoost", line=list(color=COL_VIOLET,width=2.5)) %>%
         layout(xaxis=list(title="Decile ratio XGB/BN"),yaxis=list(title="Frequence")))
  })
  
  # ============================================================================
  # 9. CARTOGRAPHIE
  # ============================================================================
  
  output$vb_carto_max   <- renderValueBox({ VBOX(round(max(carto_data$f),4),  "Frequence max",  "arrow-up",   "coral")  })
  output$vb_carto_min   <- renderValueBox({ VBOX(round(min(carto_data$f),4),  "Frequence min",  "arrow-down", "teal")   })
  output$vb_carto_idf   <- renderValueBox({
    v <- carto_data[nouvelle_region=="Ile-de-France", f]
    VBOX(round(v,4), "Ile-de-France", "city", "amber")
  })
  output$vb_carto_corse <- renderValueBox({
    v <- carto_data[nouvelle_region=="Corse", f]
    VBOX(round(v,4), "Corse", "map", "coral")
  })
  
  output$map_regions <- renderLeaflet({
    pal <- colorNumeric(c("#EEEDFE","#534AB7","#E24B4A"), carto_data$f)
    leaflet(carto_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(2.5, 46.5, 5) %>%
      addCircleMarkers(~lon, ~lat,
                       radius      = ~sqrt(nb)/25,
                       color       = ~pal(f),
                       fillOpacity = .85,
                       popup = ~paste0(
                         "<b style='color:#534AB7;'>", nouvelle_region, "</b><br>",
                         "Frequence : <b>", round(f,4), "</b><br>",
                         "BM moyen : ", round(bm,1), "<br>",
                         "Polices : ", scales::comma(nb))) %>%
      addLegend("bottomright", pal=pal, values=~f,
                title="Freq. exposee", opacity=.9)
  })
  
  output$carto_bar <- renderPlotly({
    d <- carto_data[order(-f)]
    PT(plot_ly(d, x=~f, y=~reorder(nouvelle_region,f),
               type="bar", orientation="h",
               marker=list(color=~f,
                           colorscale=list(c(0,"#EEEDFE"),c(0.5,COL_VIOLET),c(1,COL_CORAL)),
                           showscale=FALSE),
               text=~round(f,4), textposition="outside") %>%
         layout(xaxis=list(title="Frequence"),yaxis=list(title="")))
  })
  
  output$carto_risk_table <- renderUI({
    rf <- region_freq[order(-freq_exposee)][1:8]
    tags$table(style="width:100%;font-size:0.8rem;",
               tags$thead(tags$tr(
                 tags$th(style=paste0("color:",COL_VIOLET,";padding:5px;"),"Region"),
                 tags$th(style=paste0("color:",COL_VIOLET,";padding:5px;"),"Freq."),
                 tags$th(style=paste0("color:",COL_VIOLET,";padding:5px;"),"Classe")
               )),
               tags$tbody(lapply(seq_len(nrow(rf)), function(i) {
                 row <- rf[i]
                 col <- if(row$groupe_region=="RisqueEleve") COL_DANGER else COL_TEAL
                 tags$tr(style="border-bottom:1px solid rgba(83,74,183,0.08);",
                         tags$td(style="padding:5px;", row$Region),
                         tags$td(style=paste0("padding:5px;color:",col,";font-weight:500;"), round(row$freq_exposee,4)),
                         tags$td(style=paste0("padding:5px;font-size:0.7rem;font-weight:600;color:",col,";"), row$groupe_region)
                 )
               }))
    )
  })
  
  # ============================================================================
  # 10. PROFILS TYPE
  # ============================================================================
  
  output$tbl_profils <- renderDT({
    df <- profils_type[, .(Profil, DrivAge, BonusMalus,
                           Freq.predite = round(freq_pred, 4),
                           Prime.pure   = prime_pure,
                           Prime.TTC    = prime_TTC)]
    datatable(df, options=list(dom="t",pageLength=6), rownames=FALSE, class="compact hover") %>%
      formatStyle("Prime.TTC", fontWeight="bold", color=COL_VIOLET) %>%
      formatStyle("Prime.pure", color=COL_TEAL) %>%
      formatStyle("BonusMalus",
                  color=styleInterval(c(80,100,150), c(COL_TEAL,COL_TEAL,COL_AMBER,COL_CORAL)))
  })
  
  output$profils_bar <- renderPlotly({
    pal <- c(COL_CORAL,COL_TEAL,COL_VIOLET,COL_AMBER,COL_DANGER)
    PT(plot_ly(profils_type, x=~Profil, y=~prime_pure, type="bar",
               marker=list(color=pal),
               text=~paste0(prime_pure," EUR"), textposition="outside") %>%
         layout(xaxis=list(title="",tickangle=-15), yaxis=list(title="Prime pure (EUR)")))
  })
  
  output$profils_scatter <- renderPlotly({
    PT(plot_ly(profils_type, x=~BonusMalus, y=~prime_pure,
               type="scatter", mode="markers+text",
               text=~Profil, textposition="top center",
               marker=list(size=~sqrt(freq_pred)*200,
                           color=c(COL_CORAL,COL_TEAL,COL_VIOLET,COL_AMBER,COL_DANGER),
                           opacity=.8)) %>%
         layout(xaxis=list(title="Bonus-Malus"), yaxis=list(title="Prime pure (EUR)")))
  })
  
  # ============================================================================
  # 11. RAPPORT
  # ============================================================================
  
  mk_kcard <- function(val, lbl, col=COL_VIOLET) {
    div(style=paste0(
      "background:rgba(83,74,183,0.05);border:1px solid rgba(83,74,183,0.12);",
      "border-radius:8px;padding:10px;text-align:center;margin-bottom:6px;"
    ),
    div(style=paste0("font-size:1.4rem;font-weight:600;color:",col,";"), val),
    div(style="font-size:0.65rem;color:#6B6A7A;text-transform:uppercase;letter-spacing:0.5px;", lbl)
    )
  }
  
  output$rapport_freq_summary <- renderUI({ div(
    mk_kcard(round(rmse_fn(y_f,pred_nb_f),5), "RMSE GLM BN",   COL_VIOLET),
    mk_kcard(round(gini_fn(y_f,pred_xgb_f),4),"Gini XGBoost",  COL_TEAL),
    mk_kcard(roc_xgb$auc,                     "AUC XGBoost",   COL_AMBER),
    mk_kcard(round(AIC(mod_nb),0),             "AIC GLM BN",    COL_VIOLET)
  )})
  
  output$rapport_sev_summary <- renderUI({ div(
    mk_kcard(paste0(round(rmse_fn(y_s,pred_gamma),0)," EUR"), "RMSE Gamma",     COL_TEAL),
    mk_kcard(paste0(round(mae_fn(y_s,pred_gamma),0)," EUR"),  "MAE Gamma",      COL_VIOLET),
    mk_kcard(paste0(round(sev_globale,0)," EUR"),              "Severite globale",COL_AMBER),
    mk_kcard(round(AIC(mod_gamma),0),                         "AIC Gamma",       COL_TEAL)
  )})
  
  output$rapport_prime_summary <- renderUI({ div(
    mk_kcard(paste0(round(mean(test_prime$prime_nb,na.rm=TRUE),0)," EUR"),   "Prime moy.",    COL_VIOLET),
    mk_kcard(paste0(round(median(test_prime$prime_nb,na.rm=TRUE),0)," EUR"), "Prime med.",    COL_TEAL),
    mk_kcard(paste0(round(quantile(test_prime$prime_nb,.1,na.rm=TRUE),0)," EUR"), "P10%",    COL_AMBER),
    mk_kcard(paste0(round(quantile(test_prime$prime_nb,.9,na.rm=TRUE),0)," EUR"), "P90%",    COL_CORAL)
  )})
  
  output$rapport_xai_summary <- renderUI({
    top5 <- head(shap_imp, 5)
    cols <- c(COL_VIOLET,COL_TEAL,COL_AMBER,COL_CORAL,COL_DANGER)
    div(lapply(seq_len(nrow(top5)), function(i) {
      pct <- round(100 * top5$MeanAbsSHAP[i] / top5$MeanAbsSHAP[1])
      div(style="margin-bottom:10px;",
          div(style="display:flex;justify-content:space-between;margin-bottom:3px;",
              div(style=paste0("font-size:0.83rem;font-weight:500;color:",cols[i],";"), top5$Feature[i]),
              div(style=paste0("font-size:0.78rem;color:",cols[i],";font-family:monospace;"),
                  round(top5$MeanAbsSHAP[i],5))
          ),
          div(style="height:5px;border-radius:3px;background:rgba(83,74,183,0.08);",
              div(style=paste0("height:100%;width:",pct,"%;background:",cols[i],";border-radius:3px;")))
      )
    }))
  })
  
  output$rapport_calib_summary <- renderUI({
    n_ok <- sum(abs(calib_dt$Ratio-1) <= .1)
    div(
      mk_kcard(paste0(n_ok,"/",nrow(calib_dt)), "Deciles calibres",    COL_TEAL),
      mk_kcard(calib_dt$Ratio[nrow(calib_dt)],  "Ratio D10 (eleve)",   COL_VIOLET),
      mk_kcard(calib_dt$Ratio[1],               "Ratio D1 (faible)",
               if(abs(calib_dt$Ratio[1]-1)>.1) COL_DANGER else COL_TEAL),
      INSIGHT(paste0(n_ok," deciles sur ",nrow(calib_dt)," bien calibres (ratio in [0.90;1.10])."),
              type="teal", title="Synthese calibration")
    )
  })
  
  # Telechargement rapport HTML
  output$dl_rapport <- downloadHandler(
    filename = function() paste0("rapport_actuariel_", format(Sys.Date(),"%Y%m%d"), ".html"),
    content  = function(file) {
      html <- paste0(
        '<!DOCTYPE html><html lang="fr"><head><meta charset="UTF-8">',
        '<title>Rapport Actuariel — AutoActuariat</title>',
        '<style>',
        'body{font-family:system-ui,sans-serif;background:#F8F7FF;color:#1A1A2E;margin:0;padding:0;}',
        '.hdr{background:linear-gradient(135deg,#3C3489,#534AB7);padding:48px;text-align:center;color:white;}',
        '.hdr h1{font-size:2rem;font-weight:600;margin:0 0 8px;}',
        '.hdr p{opacity:.8;margin:0;}',
        '.section{background:white;border:1px solid rgba(83,74,183,0.12);border-radius:12px;padding:24px;margin:16px auto;max-width:900px;}',
        '.section h2{font-size:1.1rem;font-weight:600;color:#534AB7;margin:0 0 14px;}',
        '.kgrid{display:grid;grid-template-columns:repeat(3,1fr);gap:10px;margin:12px 0;}',
        '.kcard{background:#EEEDFE;border-radius:8px;padding:12px;text-align:center;}',
        '.kval{font-size:1.5rem;font-weight:600;color:#3C3489;}',
        '.klbl{font-size:0.65rem;color:#534AB7;text-transform:uppercase;letter-spacing:0.5px;}',
        'table{width:100%;border-collapse:collapse;font-size:0.85rem;}',
        'th{background:#EEEDFE;padding:8px;text-align:left;color:#3C3489;font-size:0.7rem;text-transform:uppercase;letter-spacing:0.5px;}',
        'td{padding:7px 8px;border-bottom:1px solid rgba(83,74,183,0.08);}',
        '.footer{text-align:center;padding:20px;font-size:0.75rem;color:#888780;background:white;border-top:1px solid rgba(83,74,183,0.1);}',
        '</style></head><body>',
        
        '<div class="hdr">',
        '<h1>Rapport Actuariel</h1>',
        '<p>Tarification Automobile — freMTPL2 — ', format(Sys.Date(),"%d/%m/%Y"), '</p>',
        '</div>',
        
        '<div class="section"><h2>Donnees freMTPL2</h2>',
        '<div class="kgrid">',
        '<div class="kcard"><div class="kval">',scales::comma(nrow(freq)),'</div><div class="klbl">Polices</div></div>',
        '<div class="kcard"><div class="kval">',scales::comma(nrow(sev_ok)),'</div><div class="klbl">Sinistres</div></div>',
        '<div class="kcard"><div class="kval">',round(100*sum(freq$ClaimNb>0)/nrow(freq),2),'%</div><div class="klbl">Sinistralite</div></div>',
        '</div></div>',
        
        '<div class="section"><h2>Performances Frequence</h2>',
        '<table><tr><th>Modele</th><th>RMSE</th><th>Gini</th><th>AUC</th><th>AIC</th></tr>',
        '<tr><td>GLM Poisson</td><td>',round(rmse_fn(y_f,pred_pois_f),5),'</td>',
        '<td>',round(gini_fn(y_f,pred_pois_f),4),'</td><td>',roc_pois$auc,'</td><td>',round(AIC(mod_pois),0),'</td></tr>',
        '<tr style="background:#EEEDFE"><td><b>GLM BN *</b></td><td>',round(rmse_fn(y_f,pred_nb_f),5),'</td>',
        '<td>',round(gini_fn(y_f,pred_nb_f),4),'</td><td>',roc_nb$auc,'</td><td>',round(AIC(mod_nb),0),'</td></tr>',
        '<tr><td>XGBoost</td><td>',round(rmse_fn(y_f,pred_xgb_f),5),'</td>',
        '<td>',round(gini_fn(y_f,pred_xgb_f),4),'</td><td>',roc_xgb$auc,'</td><td>N/A</td></tr>',
        '</table></div>',
        
        '<div class="section"><h2>Performances Severite</h2>',
        '<table><tr><th>Modele</th><th>RMSE (EUR)</th><th>MAE (EUR)</th><th>AIC</th></tr>',
        '<tr style="background:#EEEDFE"><td><b>GLM Gamma *</b></td><td>',round(rmse_fn(y_s,pred_gamma),0),'</td>',
        '<td>',round(mae_fn(y_s,pred_gamma),0),'</td><td>',round(AIC(mod_gamma),0),'</td></tr>',
        '<tr><td>Log-Normal</td><td>',round(rmse_fn(y_s,pred_lognorm),0),'</td>',
        '<td>',round(mae_fn(y_s,pred_lognorm),0),'</td><td>',round(AIC(mod_lognorm),0),'</td></tr>',
        '</table></div>',
        
        '<div class="section"><h2>Profils Type</h2>',
        '<table><tr><th>Profil</th><th>Age</th><th>BM</th><th>Freq. predite</th><th>Prime pure</th><th>Prime TTC</th></tr>',
        paste0(sapply(seq_len(nrow(profils_type)), function(i) {
          r <- profils_type[i,]
          paste0('<tr><td><b>',r$Profil,'</b></td><td>',r$DrivAge,'</td><td>',r$BonusMalus,'</td>',
                 '<td>',round(r$freq_pred,4),'</td>',
                 '<td style="color:#534AB7;font-weight:500;">',r$prime_pure,' EUR</td>',
                 '<td style="color:#1D9E75;font-weight:600;">',r$prime_TTC,' EUR</td></tr>')
        }), collapse=""),
        '</table></div>',
        
        '<div class="footer">AutoActuariat — Tarification Automobile Explicable — freMTPL2 OpenML</div>',
        '</body></html>'
      )
      writeLines(html, file, useBytes=FALSE)
    }
  )
  
  # Telechargement devis client
  output$btn_dl <- downloadHandler(
    filename = function() paste0("devis_", format(Sys.Date(),"%Y%m%d"), ".html"),
    content  = function(file) {
      r <- sim_res()
      html <- paste0(
        '<!DOCTYPE html><html><head><meta charset="UTF-8">',
        '<title>Mon Devis — AutoActuariat</title>',
        '<style>',
        'body{font-family:system-ui;background:#F8F7FF;color:#1A1A2E;margin:0;padding:0;}',
        '.hdr{background:linear-gradient(135deg,#3C3489,#534AB7);padding:40px;text-align:center;color:white;}',
        '.big{font-size:3rem;font-weight:600;}',
        '.section{background:white;border:1px solid rgba(83,74,183,0.12);border-radius:12px;padding:20px;margin:14px auto;max-width:540px;}',
        '.section h2{font-size:1rem;font-weight:600;color:#534AB7;margin:0 0 12px;}',
        'table{width:100%;font-size:0.85rem;border-collapse:collapse;}',
        'td{padding:7px 0;border-bottom:1px solid rgba(83,74,183,0.08);}',
        'td:last-child{text-align:right;font-weight:500;}',
        '.footer{text-align:center;padding:16px;font-size:0.72rem;color:#888780;}',
        '</style></head><body>',
        '<div class="hdr">',
        '<p style="opacity:.7;font-size:.8rem;margin:0 0 8px;">Prime commerciale annuelle</p>',
        '<div class="big">', r$ttc, ' EUR</div>',
        '<p style="opacity:.6;font-size:.75rem;margin:8px 0 0;">',format(Sys.Date(),"%d/%m/%Y"),'</p>',
        '</div>',
        '<div class="section"><h2>Votre profil</h2><table>',
        '<tr><td>Age conducteur</td><td>',input$s_age,' ans</td></tr>',
        '<tr><td>Bonus-Malus</td><td>',input$s_bm,'</td></tr>',
        '<tr><td>Carburant</td><td>',input$s_gas,'</td></tr>',
        '<tr><td>Puissance fiscale</td><td>',input$s_power,' CV</td></tr>',
        '<tr><td>Age vehicule</td><td>',input$s_vehage,' ans</td></tr>',
        '<tr><td>Zone geographique</td><td>',input$s_area,'</td></tr>',
        '</table></div>',
        '<div class="section"><h2>Decomposition</h2><table>',
        '<tr><td>Frequence predite</td><td>',round(r$freq*100,3),'%</td></tr>',
        '<tr><td>Severite estimee</td><td>',r$sev,' EUR</td></tr>',
        '<tr><td>Prime pure</td><td>',r$pp,' EUR</td></tr>',
        '<tr><td>Chargements (',input$s_charg,'%)</td><td>',r$ch,' EUR</td></tr>',
        '<tr><td><b>Prime commerciale</b></td><td><b>',r$ttc,' EUR</b></td></tr>',
        '</table></div>',
        '<div class="section"><h2>Niveau de risque</h2>',
        '<p style="text-align:center;font-size:1.2rem;font-weight:600;color:',
        switch(r$risk,"FAIBLE"="#1D9E75","MOYEN"="#BA7517","ELEVE"="#E24B4A"),';">',
        r$risk,' — Percentile ',r$pct,'%</p>',
        '</div>',
        '<div class="footer">AutoActuariat — Tarification Automobile — freMTPL2 OpenML</div>',
        '</body></html>'
      )
      writeLines(html, file, useBytes=FALSE)
    }
  )
  
} # fin build_server