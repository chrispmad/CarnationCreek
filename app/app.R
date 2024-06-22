source('UI.R')

server <- function(input, output, session) {
  
  if(!grepl('www/',getwd())) setwd(paste0(getwd(),"/www"))
  
  # Read in data
  hydro_dat = vroom::vroom('all_hydro_data_by_year.csv')
  
  fish_dat = vroom::vroom('all_fish_data_by_year.csv')
  
  hydro_dat_sa = reactive({
    req(!is.null(input$xy_sa_focus))
    hydro_dat |> 
      dplyr::select(year,contains(paste0("sa_",input$xy_sa_focus)))
  })
  
  xy_dat = reactive({
    dplyr::full_join(hydro_dat_sa(), fish_dat)
  })
  
  # Update the x and y input selectors.
  
  updatePickerInput(
    session = session,
    'xy_y_var',
    choices = names(fish_dat)[-1]
  )
  
  observe({
    updatePickerInput(
      session = session,
      'xy_x_var',
      choices = names(hydro_dat_sa())[-1]
    )
  })
  
  output$xy_plot = renderPlotly({
    req(!is.na(input$xy_y_var) & !is.na(input$xy_x_var))
    req(input$xy_x_var != 'NA' & input$xy_y_var != 'NA')

    cor_test = broom::tidy(cor.test(xy_dat()[[input$xy_x_var]], xy_dat()[[input$xy_y_var]]))
    
    g = ggplot(xy_dat(), aes(x = xy_dat()[[input$xy_x_var]], y = xy_dat()[[input$xy_y_var]])) + 
      geom_point() + 
      geom_smooth() +
      labs(x = stringr::str_to_title(input$xy_x_var),
           y = stringr::str_to_title(input$xy_y_var),
           title = "X-Y Scatter plot (two-sided Pearson's product-moment correlation test)")
      
    ggplotly(g) |> 
      add_annotations(
        x = 1, 
        y = 1, 
        showarrow = FALSE,
        xref = 'paper',
        yref = 'paper',
        text = paste0("R: ",round(cor_test$estimate,4),", p-value: ",
                      round(cor_test$p.value,4))
      )
  })
  
  # ==============================================
  # PCA stuff
  
  # Update the PCA variable selection
  updatePickerInput(
    session = session,
    'dep_pca_vars',
    choices = names(fish_dat)[-1],
    selected = 'juv_pop_in_section_2'
  )
  
  updatePickerInput(
    session = session,
    'expl_pca_vars',
    choices = names(hydro_dat)[-1],
    selected = c('storage_sa_2','topo_sa_2','sa_2_volume')
  )
  
  pca_vars = reactive({
    c(input$dep_pca_vars,input$expl_pca_vars)
  })
  
  cca_res = reactive({
    req(!is.null(pca_vars()))
    req(length(pca_vars()) > 1)
    # browser()
    # Normalize data
    pca_dat = scale(dat[,c(pca_vars())])
    # Only include complete cases?
    pca_dat_c = pca_dat[complete.cases(pca_dat),]
    cca_res = princomp(pca_dat_c)
  })
  
  # CCA results plots
  output$pca_eig_plot = renderPlot({
    req(!is.null(cca_res()))
    fviz_eig(cca_res(), addlabels = TRUE) + 
      theme(text = element_text(size = 16))
  })
  
  output$pca_var_plot = renderPlot({
    req(!is.null(cca_res()))
    fviz_pca_var(cca_res(), col.var = "black") + 
      theme(text = element_text(size = 16))
  })
  
  output$pca_cos2_plot = renderPlot({
    req(!is.null(cca_res()))
    fviz_cos2(cca_res(), choice = "var", axes = 1:2) + 
      theme(text = element_text(size = 16))
  })
  
  output$pca_combo_plot = renderPlot({
    req(!is.null(cca_res()))
    fviz_pca_var(cca_res(), col.var = "cos2",
                 gradient.cols = c("black", "orange", "green"),
                 repel = TRUE) + 
      theme(text = element_text(size = 16))
  })
}

shinyApp(ui, server)