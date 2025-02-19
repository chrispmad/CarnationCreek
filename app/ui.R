library(shiny)
library(bslib)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(shinymanager)

credentials <- data.frame(
  user = c("research"),
  password = c("fish")
)

# ==========================================
# Sidebar

sa_focus_select = pickerInput(
  'sa_focus',
  label = 'SA Focus (limits abiotic variable selection below)',
  choices = c('mean',as.character(c(2:9)))
)

perc_mad_select = pickerInput(
  'perc_mad',
  label = '% MAD Scenario (limits abiotic variable selection below)',
  choices = c('All','3_mad','5_mad',
              '10_mad','20_mad','40_mad',
              '100_mad','400_mad','7_q_10','95_p','1'),
  selected = 'All'
)

y_var_select = pickerInput(
  'y_var',
  label = "Dependent (Fish) Variable",
  choices = NA,
  options = list(
    `live-search` = TRUE
  ),
  multiple = TRUE
)

x_var_select = pickerInput(
  'x_var',
  label = "Explanatory (Abiotic) Variable",
  choices = NA,
  options = list(
    `live-search` = TRUE
  ),
  multiple = TRUE
)

sidebar = sidebar(
  h4("Controls and Settings"),
  width = '30%',
  sa_focus_select,
  perc_mad_select,
  y_var_select,
  x_var_select
)

# =======================================
# Panels

# Correlation Panel
correlation_panel = bslib::nav_panel(
  title = 'Correlations',
  value = 'corrs',
  card(
    numericInput(
      inputId = 'max_NA_values',
      label = 'Max NA Values in Yearly Summaries Allowed',
      min = 0,
      max = 40,
      value = 5
    ),
    uiOutput('corr_num_years_compl_cases'),
    DT::DTOutput('correlation_table')
  )
)

# XY Scatter Panel
xy_scatter_panel = bslib::nav_panel(
  title = "XY Scatter",
  value = 'XY',
  card(
    plotlyOutput('scatter_plot')
  )
)

# PCA Panel
pca_plot_options = navset_tab(
  nav_panel(
    title = "EIG Plot",
    plotOutput('pca_eig_plot',
               height = '75vh')
  ),
  nav_panel(
    title = "Variable Plot",
    plotOutput('pca_var_plot',
               height = '75vh')
  ),
  nav_panel(
    title = "Cos2 Plot",
    plotOutput('pca_cos2_plot',
               height = '75vh')
  ),
  nav_panel(
    title = "Combo Plot",
    plotOutput('pca_combo_plot',
               height = '75vh')
  )
)

pca_panel = bslib::nav_panel(
  title = "PCA",
  value = "PCA",
  card(
    pca_plot_options
  )
)

# ANOVA Panel
anova_panel = bslib::nav_panel(
  title = 'ANOVA',
  value = 'anova',
  bslib::card(
    h4("ANOVA Formula", style = 'text-align:center;'),
    uiOutput('anova_call')
  ),
  HTML("<br><br>"),
  bslib::card(
    tableOutput('anova_results')
  )
)
# ========================================
# Put it all together

plot_tabs = navset_tab(
  id = 'plot_tab_input',
  correlation_panel,
  xy_scatter_panel,
  pca_panel,
  anova_panel
)

ui <- bslib::page_navbar(
  theme = 'Shiny',
  title = "Carnation Creek",
  sidebar = sidebar,
  plot_tabs
)

ui <- secure_app(ui)