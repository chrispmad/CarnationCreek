source('ui.R')

server <- function(input, output, session) {
  
  # Results of attempted authorization.
  result_auth <- secure_server(check_credentials = check_credentials(credentials))

  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })

  observe({
    print(input$shinymanager_where)
  })

  observeEvent(input$shinymanager_where, {
  if(input$shinymanager_where == 'application'){
      
      print(getwd())
      if(!grepl('www/',getwd())) setwd(paste0(getwd(),"/www"))
      
      print('set working directory')
      
      # Read in data
      hydro_dat = vroom::vroom('all_hydro_data_by_year.csv')
      
      fish_dat = vroom::vroom('all_fish_data_by_year.csv')
      
      dat = dplyr::full_join(
        fish_dat, hydro_dat
      )
      
      # Update pickerInputs
      updatePickerInput(
        session = session,
        'y_var',
        choices = names(fish_dat)[-1],
        selected = names(fish_dat)[2]
      )
      
      observe({
        
        updatePickerInput(
          session = session,
          'x_var',
          choices = names(hydro_dat_sa())[-1],
          selected = names(hydro_dat_sa())[2]
        )
      })
      
      # Make reactive forms of dataset(s)
      hydro_dat_sa = reactive({
        
        req(!is.null(input$sa_focus))
        if(input$sa_focus == 'mean'){
          d = hydro_dat |> 
            dplyr::select(year,starts_with('mean'))
        } else {
          d = hydro_dat |> 
            dplyr::select(year,contains(paste0("sa_",input$sa_focus)))
        }
        if(input$perc_mad != 'All'){
          d = d |> 
            select(year,dplyr::ends_with('topo'),dplyr::ends_with('storage'),
                   dplyr::ends_with(input$perc_mad))
        }
        d
      })
      
      dat_r = reactive({
        # browser()
        dplyr::full_join(hydro_dat_sa(), fish_dat)
      })
      
      # Snag the current selection of variables; 
      
      selected_x_vars = reactiveVal()
      
      selected_y_vars = reactiveVal()
      
      observeEvent(input$x_var, {
        selected_x_vars(input$x_var)
      })
      
      observeEvent(input$y_var, {
        selected_y_vars(input$y_var)
      })
      
      # If we are on the XY Scatter plot, we can have 1 x variable and 1 y variable max.
      observe({
        
        if(input$plot_tab_input == 'XY'){
          if(length(selected_x_vars()) > 1){
            updatePickerInput(
              session = session,
              'x_var',
              selected = selected_x_vars()[length(selected_x_vars())]
            )
          }
          if(length(selected_y_vars()) > 1){
            updatePickerInput(
              session = session,
              'y_var',
              selected = selected_y_vars()[length(selected_y_vars())]
            )
          }
        }
      })
      
      vars_to_keep = reactive({
        dat_r() |> 
        dplyr::select_if(is.numeric) |> 
        summarise(across(everything(), \(x) sum(is.na(x)))) |> 
        tidyr::pivot_longer(cols = -year) |> 
        dplyr::select(-year) |> 
        dplyr::filter(value <= input$max_NA_values) |> 
        dplyr::pull(name)
      })
      
      dat_c = reactive({
        # Identify variables that have 5 NA values at most. Retain only these for
        # correlation matrix.
        
        dat_c = dat_r() |> dplyr::select(all_of(vars_to_keep()))
        
        # Also make sure there are no rows with NAs for the variables selected.
        dat_c = dat_c[complete.cases(dat_c),]
      })
      
      output$correlation_table = DT::renderDT({
        dat_c() |> 
          dplyr::select_if(is.numeric) |> 
          purrr::set_names(stringr::str_replace_all, '_', '\n') |> 
          cor() |> 
          tidyr::as_tibble() |> 
          dplyr::mutate(across(everything(), \(x) round(x, 5))) |> 
          dplyr::mutate(`Correlation Matrix` = vars_to_keep()) |> 
          # dplyr::mutate(`Correlation Matrix` = stringr::str_replace_all(`Correlation Matrix`,'_','\n')) |> 
          dplyr::select(`Correlation Matrix`,everything()) |> 
          DT::datatable(
            rownames=FALSE,
            extensions = c('FixedColumns',"FixedHeader"), 
            options = list(dom = 't', 
                           scrollX = TRUE, 
                           paging=FALSE,
                           fixedHeader=TRUE,
                           fixedColumns = list(leftColumns = 1, rightColumns = 0)
            )
          )
      })
      
      output$corr_num_years_compl_cases = renderUI({
        
        number_rows = nrow(dat_c())
        
        shiny::HTML(
          paste0("<span>After filtering the data for only rows where no variable is NA, <b>",
                 number_rows,
                 "</b> rows of yearly summary data used for correlation matrix</span>")
        )
      })
      
      output$scatter_plot = renderPlotly({
        req(!is.na(input$y_var) & !is.na(input$x_var))
        req(length(input$x_var) == 1 & length(input$y_var) == 1)
        req(input$x_var != 'NA' & input$y_var != 'NA')
        
        cor_test = broom::tidy(cor.test(dat_r()[[input$x_var]], dat_r()[[input$y_var]]))
        
        g = ggplot(dat_r(), aes(x = !!rlang::sym(input$x_var), y = !!rlang::sym(input$y_var))) + 
          geom_point() + 
          geom_smooth() +
          labs(x = stringr::str_to_title(input$x_var),
               y = stringr::str_to_title(input$y_var),
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
      pca_vars = reactive({
        c(input$x_var,input$y_var)
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
      
      # =============================================
      # ANOVA stuff
      
      output$anova_call = renderUI({
        req(!is.null(pca_vars()))
        req(length(pca_vars()) > 1)
        
        shiny::HTML(paste0("<b style='text-align:center;'>",input$y_var, ' ~ ',paste0(input$x_var, collapse = ' + '),"</b>"))
      })
      
      output$anova_results = renderTable({
        req(!is.null(pca_vars()))
        req(length(pca_vars()) > 1)
        
        aov_dat = as.data.frame(scale(dat[,c(pca_vars())]))
        
        aov_res = aov(as.formula(paste0(input$y_var, ' ~ ',paste0(input$x_var, collapse = ' + '))), data = aov_dat) |> broom::tidy()
        
        aov_res
      })
    }
  })
}

shinyApp(ui, server)