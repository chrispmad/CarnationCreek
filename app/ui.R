library(shiny)
library(bslib)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)

# ==========================================
# XY-scatter panel

sa_focus_select = pickerInput(
  'xy_sa_focus',
  label = 'SA Focus (limits abiotic variable selection below)',
  choices = c(2:9)
)

y_var_select = pickerInput(
  'xy_y_var',
  label = "Dependent (Fish) Variable",
  choices = NA,
  options = list(
    `live-search` = TRUE
  )
)

x_var_select = pickerInput(
  'xy_x_var',
  label = "Explanatory (Abiotic) Variable",
  choices = NA,
  options = list(
    `live-search` = TRUE
  )
)

xy_sidebar = sidebar(
  width = '30%',
  sa_focus_select,
  y_var_select,
  x_var_select
)

xy_scatter_panel = bslib::nav_panel(
  title = "X-Y Scatter",
  bslib::layout_sidebar(
    sidebar = xy_sidebar,
    card(
      plotlyOutput('xy_plot')
    )
  )
)

# =========================================
# PCA panel

dep_pca_var_selection = pickerInput(
  'dep_pca_vars',
  label = "Dependent (Fish) Variables for PCA",
  choices = NA,
  options = list(
    `live-search` = TRUE
  ),
  multiple = TRUE
)

expl_pca_var_selection = pickerInput(
  'expl_pca_vars',
  label = "Explanatory (Abiotic) Variables for PCA",
  choices = NA,
  options = list(
    `live-search` = TRUE
  ),
  multiple = TRUE
)


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
  bslib::layout_sidebar(
    sidebar = sidebar(
      width = '30%',
      dep_pca_var_selection,
      expl_pca_var_selection
    ),
    card(
      pca_plot_options
    )
  )
)

# ========================================
# Put it all together

ui <- bslib::page_navbar(
  title = "Carnation Creek",
  xy_scatter_panel,
  pca_panel
)