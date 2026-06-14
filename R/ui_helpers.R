# ==============================================================================
# ui_helpers.R
# Définition complète de l'interface utilisateur
# Palette : Violet + Blanc | bs4Dash | Plotly | Leaflet
# ==============================================================================
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(plotly)
library(DT)
library(leaflet)

# ==============================================================================
# 1. COULEURS
# ==============================================================================

COL_VIOLET      <- "#534AB7"
COL_VIOLET_DARK <- "#3C3489"
COL_VIOLET_LITE <- "#EEEDFE"
COL_VIOLET_MID  <- "#7F77DD"
COL_TEAL        <- "#1D9E75"
COL_CORAL       <- "#D85A30"
COL_AMBER       <- "#BA7517"
COL_GRAY        <- "#888780"
COL_SUCCESS     <- "#639922"
COL_DANGER      <- "#E24B4A"

# ==============================================================================
# 2. CSS
# ==============================================================================

APP_CSS <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600&display=swap');

:root {
  --violet:       #534AB7;
  --violet-dark:  #3C3489;
  --violet-lite:  #EEEDFE;
  --violet-mid:   #7F77DD;
  --teal:         #1D9E75;
  --coral:        #D85A30;
  --amber:        #BA7517;
  --gray:         #888780;
  --bg:           #F8F7FF;
  --card-bg:      #FFFFFF;
  --text:         #1A1A2E;
  --text-muted:   #6B6A7A;
  --border:       rgba(83,74,183,0.12);
  --radius:       12px;
  --radius-sm:    8px;
}

body, .wrapper {
  background: var(--bg) !important;
  font-family: 'Inter', sans-serif !important;
  color: var(--text) !important;
}

.main-header .navbar {
  background: var(--card-bg) !important;
  border-bottom: 1px solid var(--border) !important;
  box-shadow: none !important;
}
.navbar-brand, .brand-link {
  background: var(--violet-dark) !important;
  border-bottom: 1px solid rgba(255,255,255,0.1) !important;
}
.brand-text {
  color: #FFFFFF !important;
  font-size: 0.95rem !important;
  font-weight: 600 !important;
}

.main-sidebar { background: var(--violet-dark) !important; border-right: none !important; }
.nav-sidebar .nav-link { color: #AFA9EC !important; font-size: 0.82rem !important; font-weight: 400 !important; border-radius: var(--radius-sm) !important; margin: 2px 8px !important; padding: 8px 12px !important; transition: all 0.15s ease !important; }
.nav-sidebar .nav-link:hover { background: rgba(255,255,255,0.08) !important; color: #EEEDFE !important; }
.nav-sidebar .nav-link.active { background: rgba(255,255,255,0.15) !important; color: #FFFFFF !important; font-weight: 500 !important; }
.nav-sidebar .nav-link .nav-icon { color: inherit !important; width: 18px !important; }
.nav-header { color: var(--violet-mid) !important; font-size: 0.65rem !important; letter-spacing: 1.5px !important; font-weight: 600 !important; padding: 12px 16px 4px !important; text-transform: uppercase !important; }

.content-wrapper, .main-content { background: var(--bg) !important; }
.content { padding: 16px !important; }
.content-header { background: var(--card-bg) !important; border-bottom: 1px solid var(--border) !important; padding: 10px 20px !important; }
.content-header h1 { font-size: 0.95rem !important; font-weight: 500 !important; color: var(--violet-dark) !important; }

.card, .box { background: var(--card-bg) !important; border: 1px solid var(--border) !important; border-radius: var(--radius) !important; box-shadow: 0 1px 4px rgba(83,74,183,0.06) !important; }
.card-header, .box-header { background: var(--card-bg) !important; border-bottom: 1px solid var(--border) !important; padding: 10px 14px !important; }
.box.box-primary  > .box-header { border-left: 3px solid var(--violet) !important; }
.box.box-info     > .box-header { border-left: 3px solid var(--teal) !important; }
.box.box-warning  > .box-header { border-left: 3px solid var(--amber) !important; }
.box.box-danger   > .box-header { border-left: 3px solid var(--coral) !important; }
.box.box-success  > .box-header { border-left: 3px solid var(--teal) !important; }

.small-box { border-radius: var(--radius) !important; box-shadow: 0 1px 4px rgba(83,74,183,0.08) !important; }
.small-box .inner h3 { font-size: 1.6rem !important; font-weight: 600 !important; }
.small-box .inner p  { font-size: 0.72rem !important; letter-spacing: 0.5px !important; }

.form-control, .form-select { border: 1px solid var(--border) !important; border-radius: var(--radius-sm) !important; font-size: 0.85rem !important; color: var(--text) !important; }
.form-control:focus { border-color: var(--violet) !important; box-shadow: 0 0 0 3px rgba(83,74,183,0.12) !important; }
.control-label, label { font-size: 0.75rem !important; font-weight: 500 !important; color: var(--text-muted) !important; text-transform: uppercase !important; letter-spacing: 0.4px !important; }
.irs--shiny .irs-bar     { background: var(--violet) !important; }
.irs--shiny .irs-single  { background: var(--violet) !important; color: #fff !important; }
.irs--shiny .irs-handle > i:first-child { background: var(--violet) !important; }

.btn-primary { background: var(--violet) !important; border-color: var(--violet) !important; color: #fff !important; font-weight: 500 !important; border-radius: var(--radius-sm) !important; }
.btn-primary:hover { background: var(--violet-dark) !important; border-color: var(--violet-dark) !important; }
.btn-success { background: var(--teal) !important; border-color: var(--teal) !important; font-weight: 500 !important; border-radius: var(--radius-sm) !important; }

.nav-tabs .nav-link { color: var(--text-muted) !important; font-size: 0.82rem !important; border: none !important; }
.nav-tabs .nav-link.active { color: var(--violet) !important; border-bottom: 2px solid var(--violet) !important; background: transparent !important; font-weight: 500 !important; }
.nav-tabs { border-bottom: 1px solid var(--border) !important; }

table.dataTable thead th { background: var(--violet-lite) !important; color: var(--violet-dark) !important; font-size: 0.72rem !important; font-weight: 600 !important; letter-spacing: 0.5px !important; text-transform: uppercase !important; border-bottom: 2px solid var(--violet) !important; }
table.dataTable tbody tr:hover td { background: var(--violet-lite) !important; }
.dataTables_filter input, .dataTables_length select { border: 1px solid var(--border) !important; border-radius: var(--radius-sm) !important; font-size: 0.82rem !important; }
.paginate_button.current { background: var(--violet) !important; color: #fff !important; border: none !important; border-radius: var(--radius-sm) !important; }

.prime-result-card { background: var(--violet-lite); border: 1px solid rgba(83,74,183,0.2); border-radius: var(--radius); padding: 24px; text-align: center; }
.prime-big-label { font-size: 0.72rem; color: var(--violet); text-transform: uppercase; letter-spacing: 1px; font-weight: 500; margin-bottom: 6px; }
.prime-big-value { font-size: 2.8rem; font-weight: 600; color: var(--violet-dark); line-height: 1; margin-bottom: 8px; }
.prime-detail-row { display: flex; justify-content: space-between; font-size: 0.82rem; padding: 5px 0; border-bottom: 1px solid rgba(83,74,183,0.1); color: var(--text-muted); }
.prime-detail-row span:last-child { font-weight: 500; color: var(--text); }

.risk-badge { display: inline-flex; align-items: center; gap: 6px; padding: 5px 14px; border-radius: 20px; font-size: 0.78rem; font-weight: 500; margin-top: 10px; }
.risk-low    { background: #EAF3DE; color: #3B6D11; }
.risk-medium { background: #FAEEDA; color: #854F0B; }
.risk-high   { background: #FCEBEB; color: #A32D2D; }

.insight-panel { background: var(--violet-lite); border-left: 3px solid var(--violet); border-radius: 0 var(--radius-sm) var(--radius-sm) 0; padding: 12px 14px; margin-top: 10px; font-size: 0.83rem; color: var(--text-muted); line-height: 1.7; }
.insight-panel.teal  { background: #E1F5EE; border-left-color: var(--teal); }
.insight-panel.coral { background: #FAECE7; border-left-color: var(--coral); }
.insight-panel strong { color: var(--text); }

.section-badge { display: inline-flex; align-items: center; gap: 6px; background: var(--violet-lite); border: 1px solid rgba(83,74,183,0.2); border-radius: var(--radius-sm); padding: 3px 10px; font-size: 0.68rem; font-weight: 600; color: var(--violet); text-transform: uppercase; letter-spacing: 1px; }

::-webkit-scrollbar { width: 5px; height: 5px; }
::-webkit-scrollbar-track { background: var(--bg); }
::-webkit-scrollbar-thumb { background: rgba(83,74,183,0.25); border-radius: 3px; }
::-webkit-scrollbar-thumb:hover { background: var(--violet); }

.tab-pane { animation: fadeUp 0.3s ease both; }
@keyframes fadeUp { from { opacity: 0; transform: translateY(8px); } to { opacity: 1; transform: translateY(0); } }
"

# ==============================================================================
# 3. HELPERS
# ==============================================================================

DBOX <- function(..., title, icon_fa, status = "primary", width = 6,
                 collapsible = TRUE, maximizable = TRUE) {
  pal <- list(primary=COL_VIOLET, info=COL_TEAL, success=COL_SUCCESS,
              warning=COL_AMBER, danger=COL_CORAL)
  col <- pal[[status]] %||% COL_VIOLET
  title_ui <- span(
    style = paste0("display:inline-flex;align-items:center;gap:6px;",
                   "font-size:0.72rem;font-weight:600;letter-spacing:0.5px;",
                   "text-transform:uppercase;color:", col, ";"),
    tags$i(class = paste0("fas fa-", icon_fa),
           style = paste0("font-size:0.78rem;color:", col, ";")),
    title)
  box(title = title_ui, status = status, solidHeader = FALSE,
      width = width, collapsible = collapsible, maximizable = maximizable, ...)
}

VBOX <- function(value, subtitle, icon_fa, color = "primary") {
  col_map <- c(primary="primary", violet="primary", teal="success",
               coral="danger", amber="warning", gray="secondary")
  bs4_col <- unname(col_map[color])
  if (is.na(bs4_col)) bs4_col <- "primary"
  valueBox(value=value, subtitle=subtitle,
           icon=icon(icon_fa, lib="font-awesome"),
           color=bs4_col, width=NULL)
}

INSIGHT <- function(text, type = "violet", title = "Interpretation") {
  cls <- switch(type, teal="teal", coral="coral", "")
  div(class = paste("insight-panel", cls),
      tags$strong(style="display:block;margin-bottom:5px;font-size:0.78rem;",
                  paste0("# ", toupper(title))),
      HTML(text))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ==============================================================================
# 4. THEME PLOTLY
# ==============================================================================

PT <- function(p, ...) {
  p %>% plotly::layout(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font = list(family="Inter", color="#6B6A7A", size=11),
    xaxis = list(gridcolor="rgba(83,74,183,0.08)", zerolinecolor="rgba(83,74,183,0.15)",
                 tickfont=list(color="#6B6A7A"), linecolor="rgba(83,74,183,0.1)"),
    yaxis = list(gridcolor="rgba(83,74,183,0.08)", zerolinecolor="rgba(83,74,183,0.15)",
                 tickfont=list(color="#6B6A7A"), linecolor="rgba(83,74,183,0.1)"),
    legend = list(font=list(color="#6B6A7A",size=11), bgcolor="rgba(255,255,255,0.9)",
                  bordercolor="rgba(83,74,183,0.15)", borderwidth=1,
                  orientation="h", y=-0.22, x=0.5, xanchor="center"),
    margin    = list(t=20, b=55, l=55, r=15),
    hoverlabel = list(bgcolor="#3C3489", bordercolor="rgba(83,74,183,0.5)",
                      font=list(family="Inter", color="#FFFFFF", size=12)),
    hovermode = "x unified", ...
  ) %>% plotly::config(
    displayModeBar=TRUE, displaylogo=FALSE,
    modeBarButtonsToRemove=c("pan2d","select2d","lasso2d","autoScale2d"),
    toImageButtonOptions=list(format="png", filename="autoActuariat_graph",
                              height=500, width=900, scale=2))
}

# ==============================================================================
# 5-15. PAGES UI
# ==============================================================================

page_accueil_client <- bs4TabItem("accueil_client",
                                  fluidRow(column(12,
                                                  div(style=paste0("background:linear-gradient(135deg,#3C3489 0%,#534AB7 60%,#7F77DD 100%);",
                                                                   "border-radius:16px;padding:48px 40px;text-align:center;color:white;margin-bottom:20px;"),
                                                      tags$i(class="fas fa-car", style="font-size:3rem;margin-bottom:16px;display:block;opacity:0.9;"),
                                                      h2(style="font-size:2.2rem;font-weight:600;margin:0 0 8px;color:#fff;","Bienvenue dans votre espace"),
                                                      p(style="font-size:1rem;opacity:0.8;margin:0 0 28px;max-width:500px;margin-left:auto;margin-right:auto;",
                                                        "Simulez votre prime d'assurance auto en quelques secondes."),
                                                      div(style="display:flex;gap:12px;justify-content:center;flex-wrap:wrap;",
                                                          actionButton("go_simulateur","Simuler ma prime",class="btn btn-light",
                                                                       style="font-weight:500;padding:10px 24px;border-radius:8px;",icon=icon("calculator")),
                                                          actionButton("go_position","Ma position",
                                                                       style=paste0("background:rgba(255,255,255,0.15);color:white;",
                                                                                    "border:1px solid rgba(255,255,255,0.3);font-weight:500;",
                                                                                    "padding:10px 24px;border-radius:8px;"),icon=icon("chart-pie")))
                                                  )
                                  )),
                                  fluidRow(
                                    column(4, div(style=paste0("background:white;border:1px solid rgba(83,74,183,0.12);",
                                                               "border-radius:12px;padding:20px;text-align:center;cursor:pointer;"),
                                                  onclick="Shiny.setInputValue('go_to_tab','simulateur',{priority:'event'})",
                                                  tags$i(class="fas fa-sliders",style="font-size:2rem;color:#534AB7;margin-bottom:12px;display:block;"),
                                                  h4(style="font-weight:600;margin:0 0 6px;font-size:1rem;","Mon devis"),
                                                  p(style="font-size:0.82rem;color:#6B6A7A;margin:0;","Calculez votre prime personnalisee."))),
                                    column(4, div(style=paste0("background:white;border:1px solid rgba(83,74,183,0.12);",
                                                               "border-radius:12px;padding:20px;text-align:center;cursor:pointer;"),
                                                  onclick="Shiny.setInputValue('go_to_tab','client_pos',{priority:'event'})",
                                                  tags$i(class="fas fa-chart-pie",style="font-size:2rem;color:#1D9E75;margin-bottom:12px;display:block;"),
                                                  h4(style="font-weight:600;margin:0 0 6px;font-size:1rem;","Ma position"),
                                                  p(style="font-size:0.82rem;color:#6B6A7A;margin:0;","Comparez votre profil au portefeuille."))),
                                    column(4, div(style=paste0("background:white;border:1px solid rgba(83,74,183,0.12);",
                                                               "border-radius:12px;padding:20px;text-align:center;"),
                                                  tags$i(class="fas fa-shield-halved",style="font-size:2rem;color:#BA7517;margin-bottom:12px;display:block;"),
                                                  h4(style="font-weight:600;margin:0 0 6px;font-size:1rem;","Protection"),
                                                  p(style="font-size:0.82rem;color:#6B6A7A;margin:0;","Vos donnees sont securisees.")))
                                  )
)

page_simulateur <- bs4TabItem("simulateur",
                              fluidRow(
                                column(4,
                                       box(width=12, collapsible=FALSE,
                                           title=span(tags$i(class="fas fa-sliders",style="color:#534AB7;margin-right:6px;"),
                                                      span("Votre profil",style="font-size:0.82rem;font-weight:600;color:#534AB7;text-transform:uppercase;letter-spacing:0.5px;")),
                                           div(class="section-badge",style="margin-bottom:12px;",tags$i(class="fas fa-user"),"Conducteur"),
                                           sliderInput("s_age","Age du conducteur",18,85,35,1),
                                           sliderInput("s_bm","Bonus-Malus",50,230,100,5),
                                           tags$hr(style="border-color:rgba(83,74,183,0.1);margin:10px 0;"),
                                           div(class="section-badge",style="margin-bottom:12px;",tags$i(class="fas fa-car"),"Vehicule"),
                                           sliderInput("s_vehage","Age du vehicule (ans)",0,20,5,1),
                                           sliderInput("s_power","Puissance fiscale (CV)",4,15,7,1),
                                           selectInput("s_gas","Carburant",choices=c("Diesel"="Diesel","Essence"="Regular"),selected="Regular"),
                                           tags$hr(style="border-color:rgba(83,74,183,0.1);margin:10px 0;"),
                                           div(class="section-badge",style="margin-bottom:12px;",tags$i(class="fas fa-map-pin"),"Localisation"),
                                           selectInput("s_area","Zone geographique",
                                                       choices=c("A — Rural isole"="A","B — Rural"="B","C — Peri-urbain"="C",
                                                                 "D — Urbain"="D","E — Urbain dense"="E","F — Metropole"="F"),selected="C"),
                                           numericInput("s_density","Densite (hab/km2)",500,1,50000,100),
                                           tags$hr(style="border-color:rgba(83,74,183,0.1);margin:10px 0;"),
                                           sliderInput("s_charg","Chargement (%)",10,60,33,1),
                                           br(),
                                           actionButton("btn_calc","Calculer ma prime",class="btn-primary w-100",icon=icon("bolt")),
                                           br(),br(),
                                           downloadButton("btn_dl","Telecharger mon devis",class="btn-success w-100")
                                       )
                                ),
                                column(8,
                                       uiOutput("sim_result_card"), br(),
                                       fluidRow(
                                         DBOX(plotlyOutput("sim_position",height="210px"),title="Votre position vs portefeuille",
                                              icon_fa="users",status="primary",width=6,collapsible=FALSE,maximizable=FALSE),
                                         DBOX(plotlyOutput("sim_decomp",height="210px"),title="Decomposition de la prime",
                                              icon_fa="chart-pie",status="info",width=6,collapsible=FALSE,maximizable=FALSE)),
                                       fluidRow(column(12,uiOutput("sim_conseil")))
                                )
                              )
)

page_client_pos <- bs4TabItem("client_pos",
                              fluidRow(
                                DBOX(plotlyOutput("client_bm_pos",height="270px"),title="Votre BM vs portefeuille",icon_fa="star",status="primary",width=6),
                                DBOX(plotlyOutput("client_age_pos",height="270px"),title="Sinistralite selon votre age",icon_fa="user",status="info",width=6)),
                              fluidRow(
                                DBOX(plotlyOutput("client_zone_pos",height="270px"),title="Prime par zone geographique",icon_fa="map",status="warning",width=6),
                                DBOX(uiOutput("client_conseil_full"),title="Vos conseils personnalises",icon_fa="lightbulb",status="success",width=6,collapsible=FALSE))
)

page_accueil_assureur <- bs4TabItem("accueil_assureur",
                                    fluidRow(column(12,
                                                    div(style=paste0("background:linear-gradient(135deg,#26215C 0%,#3C3489 50%,#534AB7 100%);",
                                                                     "border-radius:16px;padding:48px 40px;text-align:center;color:white;margin-bottom:20px;"),
                                                        tags$i(class="fas fa-building",style="font-size:3rem;margin-bottom:16px;display:block;opacity:0.9;"),
                                                        h2(style="font-size:2.2rem;font-weight:600;margin:0 0 8px;color:#fff;","Espace Assureur"),
                                                        p(style="font-size:1rem;opacity:0.8;margin:0 0 24px;max-width:560px;margin-left:auto;margin-right:auto;",
                                                          "Acces complet aux modeles GLM & XGBoost, XAI, calibration, cartographie et rapports."),
                                                        fluidRow(style="max-width:640px;margin:0 auto;",
                                                                 column(3,div(style="background:rgba(255,255,255,0.1);border-radius:10px;padding:14px 8px;text-align:center;",
                                                                              div(style="font-size:1.6rem;font-weight:600;",textOutput("kpi_polices_acc",inline=TRUE)),
                                                                              div(style="font-size:0.65rem;opacity:0.7;text-transform:uppercase;letter-spacing:1px;margin-top:3px;","Polices"))),
                                                                 column(3,div(style="background:rgba(255,255,255,0.1);border-radius:10px;padding:14px 8px;text-align:center;",
                                                                              div(style="font-size:1.6rem;font-weight:600;",textOutput("kpi_sinistres_acc",inline=TRUE)),
                                                                              div(style="font-size:0.65rem;opacity:0.7;text-transform:uppercase;letter-spacing:1px;margin-top:3px;","Sinistres"))),
                                                                 column(3,div(style="background:rgba(255,255,255,0.1);border-radius:10px;padding:14px 8px;text-align:center;",
                                                                              div(style="font-size:1.6rem;font-weight:600;",textOutput("kpi_freq_acc",inline=TRUE)),
                                                                              div(style="font-size:0.65rem;opacity:0.7;text-transform:uppercase;letter-spacing:1px;margin-top:3px;","Frequence"))),
                                                                 column(3,div(style="background:rgba(255,255,255,0.1);border-radius:10px;padding:14px 8px;text-align:center;",
                                                                              div(style="font-size:1.6rem;font-weight:600;",textOutput("kpi_prime_acc",inline=TRUE)),
                                                                              div(style="font-size:0.65rem;opacity:0.7;text-transform:uppercase;letter-spacing:1px;margin-top:3px;","Prime med.")))
                                                        )
                                                    )
                                    ))
)

page_dashboard <- bs4TabItem("dashboard",
                             fluidRow(
                               valueBoxOutput("vb_polices",width=3), valueBoxOutput("vb_freq_obs",width=3),
                               valueBoxOutput("vb_sev_moy",width=3), valueBoxOutput("vb_prime_med",width=3)),
                             fluidRow(
                               valueBoxOutput("vb_gini_xgb",width=3), valueBoxOutput("vb_auc_best",width=3),
                               valueBoxOutput("vb_disp",width=3),     valueBoxOutput("vb_theta",width=3)),
                             fluidRow(
                               DBOX(plotlyOutput("dash_freq_age",height="280px"),title="Frequence exposee par classe d'age",icon_fa="user",status="primary",width=8),
                               DBOX(plotlyOutput("dash_donut",height="280px"),title="Sinistralite globale",icon_fa="chart-pie",status="warning",width=4)),
                             fluidRow(
                               DBOX(plotlyOutput("dash_bm",height="250px"),title="Frequence vs Bonus-Malus",icon_fa="star",status="info",width=4),
                               DBOX(plotlyOutput("dash_zone",height="250px"),title="Frequence par zone",icon_fa="map",status="warning",width=4),
                               DBOX(plotlyOutput("dash_lorenz",height="250px"),title="Courbe de Lorenz",icon_fa="chart-line",status="success",width=4))
)

page_modeles <- bs4TabItem("modeles",
                           fluidRow(DBOX(DTOutput("tbl_comp_freq"),title="Comparaison modeles frequence",icon_fa="trophy",status="primary",width=12)),
                           fluidRow(
                             DBOX(plotlyOutput("lorenz_3",height="280px"),title="Courbes de Lorenz",icon_fa="chart-line",status="primary",width=6),
                             DBOX(plotlyOutput("roc_plot",height="280px"),title="Courbes ROC",icon_fa="chart-area",status="info",width=6)),
                           fluidRow(DBOX(DTOutput("tbl_comp_sev"),title="Comparaison modeles severite",icon_fa="coins",status="info",width=12)),
                           fluidRow(
                             DBOX(plotlyOutput("scatter_obs_sev",height="260px"),title="Observe vs Predit (Gamma)",icon_fa="chart-scatter",status="success",width=6),
                             DBOX(plotlyOutput("prime_bm_line",height="260px"),title="Prime pure vs Bonus-Malus",icon_fa="chart-line",status="warning",width=6))
)

page_xai <- bs4TabItem("xai",
                       fluidRow(
                         DBOX(plotlyOutput("shap_bar",height="320px"),title="SHAP — Importance globale",icon_fa="eye",status="primary",width=6),
                         DBOX(plotlyOutput("coef_plot",height="320px"),title="Coefficients GLM BN",icon_fa="list-check",status="info",width=6)),
                       fluidRow(
                         DBOX(plotlyOutput("pdp_age",height="260px"),title="PDP — Age conducteur",icon_fa="user",status="primary",width=6),
                         DBOX(plotlyOutput("pdp_bm",height="260px"),title="PDP — Bonus-Malus",icon_fa="star",status="warning",width=6)),
                       fluidRow(
                         DBOX(plotlyOutput("ice_age",height="280px"),title="ICE — Age (50 polices)",icon_fa="chart-line",status="info",width=6),
                         DBOX(plotlyOutput("ice_bm",height="280px"),title="ICE — Bonus-Malus",icon_fa="star",status="primary",width=6))
)

page_calibration <- bs4TabItem("calibration",
                               fluidRow(
                                 DBOX(DTOutput("tbl_calib"),title="Calibration par decile",icon_fa="bullseye",status="success",width=7),
                                 DBOX(plotlyOutput("plot_calib_bar",height="300px"),title="Ratio O/P par decile",icon_fa="chart-bar",status="success",width=5)),
                               fluidRow(
                                 DBOX(plotlyOutput("lorenz_all",height="270px"),title="Courbe de Lorenz",icon_fa="chart-line",status="primary",width=6),
                                 DBOX(plotlyOutput("double_lift",height="270px"),title="Double Lift",icon_fa="arrows-left-right",status="info",width=6))
)

page_carto <- bs4TabItem("carto",
                         fluidRow(
                           valueBoxOutput("vb_carto_max",width=3), valueBoxOutput("vb_carto_min",width=3),
                           valueBoxOutput("vb_carto_idf",width=3), valueBoxOutput("vb_carto_corse",width=3)),
                         fluidRow(
                           column(8, DBOX(leafletOutput("map_regions",height="440px"),title="Carte interactive — Frequence par region",icon_fa="map-location-dot",status="primary",width=12)),
                           column(4,
                                  DBOX(plotlyOutput("carto_bar",height="220px"),title="Frequence par region",icon_fa="chart-bar",status="primary",width=12),
                                  DBOX(uiOutput("carto_risk_table"),title="Classes de risque",icon_fa="table",status="info",width=12)))
)

page_profils <- bs4TabItem("profils",
                           fluidRow(DBOX(DTOutput("tbl_profils"),title="Tarification par profil type",icon_fa="users",status="primary",width=12)),
                           fluidRow(
                             DBOX(plotlyOutput("profils_bar",height="280px"),title="Prime pure par profil",icon_fa="euro-sign",status="primary",width=6),
                             DBOX(plotlyOutput("profils_scatter",height="280px"),title="BonusMalus vs Prime pure",icon_fa="chart-scatter",status="info",width=6))
)

page_rapport <- bs4TabItem("rapport",
                           fluidRow(column(12,
                                           div(style=paste0("background:linear-gradient(135deg,#3C3489 0%,#534AB7 100%);",
                                                            "border-radius:16px;padding:48px;text-align:center;color:white;margin-bottom:20px;"),
                                               tags$i(class="fas fa-file-code",style="font-size:3.5rem;margin-bottom:16px;display:block;opacity:0.9;"),
                                               h2(style="font-size:2rem;font-weight:600;margin:0 0 10px;color:#fff;","Rapport Actuariel Complet"),
                                               p(style="font-size:0.9rem;opacity:0.8;max-width:520px;margin:0 auto 24px;",
                                                 "Telechargez le rapport HTML : resultats, modeles, metriques, profils et calibration."),
                                               downloadButton("dl_rapport","Telecharger le rapport",icon=icon("download"),
                                                              style=paste0("background:white;color:#3C3489;border:none;font-weight:600;",
                                                                           "padding:12px 28px;border-radius:8px;font-size:0.95rem;")))
                           )),
                           fluidRow(
                             DBOX(uiOutput("rapport_freq_summary"),title="Synthese Frequence",icon_fa="chart-bar",status="primary",width=4,collapsible=FALSE),
                             DBOX(uiOutput("rapport_sev_summary"),title="Synthese Severite",icon_fa="coins",status="info",width=4,collapsible=FALSE),
                             DBOX(uiOutput("rapport_prime_summary"),title="Synthese Prime Pure",icon_fa="euro-sign",status="warning",width=4,collapsible=FALSE)),
                           fluidRow(
                             DBOX(uiOutput("rapport_xai_summary"),title="Top Variables SHAP",icon_fa="eye",status="primary",width=6,collapsible=FALSE),
                             DBOX(uiOutput("rapport_calib_summary"),title="Calibration",icon_fa="bullseye",status="success",width=6,collapsible=FALSE))
)

# ==============================================================================
# 16. BUILD_UI — STRUCTURE CONFORME A L'ORIGINAL
# ==============================================================================

build_ui <- function() {
  tagList(
    tags$head(
      tags$link(rel="stylesheet",
                href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
      tags$style(HTML(APP_CSS))
    ),
    bs4DashPage(
      dark        = FALSE,
      help        = FALSE,
      scrollToTop = TRUE,
      
      header = bs4DashNavbar(
        title = bs4DashBrand(title="AutoActuariat", color="white", href="#"),
        skin  = "light",
        # Structure exacte de l'original :
        # tags$li(class="nav-item dropdown") + tags$a(class="nav-link") + tags$button
        rightUi = tagList(
          tags$li(class="nav-item dropdown", style="list-style:none;",
                  tags$a(class="nav-link", href="#",
                         uiOutput("space_badge_nav")
                  )
          ),
          tags$li(class="nav-item dropdown", style="list-style:none;",
                  tags$a(class="nav-link", href="#",
                         tags$button(
                           onclick="Shiny.setInputValue('btn_home', Math.random(), {priority:'event'}); return false;",
                           style=paste0(
                             "background:rgba(83,74,183,0.12);",
                             "border:1px solid rgba(83,74,183,0.3);",
                             "color:#534AB7;border-radius:20px;",
                             "padding:5px 14px;font-size:0.72rem;",
                             "font-weight:700;letter-spacing:1px;cursor:pointer;"),
                           tags$i(class="fas fa-home", style="margin-right:4px;"),
                           "ACCUEIL"
                         )
                  )
          )
        )
      ),
      
      sidebar = bs4DashSidebar(
        skin   = "dark",
        status = "primary",
        bs4SidebarMenu(id="sidebar_menu",
                       bs4SidebarHeader("Espace Client"),
                       bs4SidebarMenuItem("Accueil Client",  tabName="accueil_client",  icon=icon("home")),
                       bs4SidebarMenuItem("Mon Devis",       tabName="simulateur",      icon=icon("calculator")),
                       bs4SidebarMenuItem("Ma Position",     tabName="client_pos",      icon=icon("chart-pie")),
                       bs4SidebarHeader("Espace Assureur"),
                       bs4SidebarMenuItem("Accueil Assureur",tabName="accueil_assureur",icon=icon("building")),
                       bs4SidebarMenuItem("Dashboard",       tabName="dashboard",       icon=icon("tachometer-alt")),
                       bs4SidebarMenuItem("Modeles",         tabName="modeles",         icon=icon("code-branch")),
                       bs4SidebarMenuItem("XAI & SHAP",      tabName="xai",             icon=icon("eye")),
                       bs4SidebarMenuItem("Calibration",     tabName="calibration",     icon=icon("bullseye")),
                       bs4SidebarMenuItem("Cartographie",    tabName="carto",           icon=icon("map-location-dot")),
                       bs4SidebarMenuItem("Profils Type",    tabName="profils",         icon=icon("users")),
                       bs4SidebarMenuItem("Rapport",         tabName="rapport",         icon=icon("file-code"))
        )
      ),
      
      body = bs4DashBody(
        bs4TabItems(
          page_accueil_client,
          page_simulateur,
          page_client_pos,
          page_accueil_assureur,
          page_dashboard,
          page_modeles,
          page_xai,
          page_calibration,
          page_carto,
          page_profils,
          page_rapport
        )
      )
    )
  )
}
