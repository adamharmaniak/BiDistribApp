source("global.R")
source("funkcie.R")

server <- function(input, output, session) {
  # Reaktivny objekt na ulozenie dat
  loaded_data <- reactiveVal(NULL)
  
  # Nacitanie dat po kliknuti na tlacidlo
  observeEvent(input$load_data, {
    if (input$data_mode == "builtin") {
      # Zabudovany dataset
      dataset_name <- input$builtin_dataset
      data <- get(dataset_name, "package:datasets")
      loaded_data(data)
      
    } else if (input$data_mode == "upload") {
      # CSV subor
      file <- input$datafile
      if (is.null(file)) {
        showNotification("First load the CSV file", type = "error")
        return(NULL)
      }
      
      data <- tryCatch({
        read.csv(file$datapath, stringsAsFactors = FALSE)
      }, error = function(e) {
        showNotification("Error when loading CSV file", type = "error")
        return(NULL)
      })
      
      loaded_data(data)
    }
  })
  
  # Vypis premennych do tabulky po nacitani dat
  output$summaryVars <- renderTable({
    data <- loaded_data()
    if (is.null(data)) return(NULL)
    
    # Zavolanie printVariables()
    variables <- identify_variables(data)
    all_variables <- c(variables$Diskretne, variables$Spojite)
    
    variable_types <- c(rep("Diskrétna", length(variables$Diskretne)),
                        rep("Spojitá", length(variables$Spojite)))
    
    counts <- sapply(all_variables, function(var) sum(!is.na(data[[var]])))
    
    tibble(
      Index = seq_along(all_variables),
      Variable_Name = all_variables,
      Variable_Type = variable_types,
      Pocet_pozorovani = counts
    )
  })
  
  output$variable_info_ui <- renderUI({
    data <- loaded_data()
    if (is.null(data)) return(NULL)
    
    tagList(
      h4("Variable information"),
      tableOutput("summaryVars")
    )
  })
  
  output$copula_type_ui <- renderUI({
    model_type <- input$density_model_type
    if (is.null(model_type)) return(NULL)
    
    choices <- switch(model_type,
                      "parametric" = c("Clayton", "Gumbel", "Frank"),
                      "t" = c("Clayton", "Gumbel", "Joe"),
                      "kernel" = c("empirical"),
                      NULL
    )
    
    selectInput("copula_type", "Copula type:", choices = choices)
  })
  
  output$tools_sidebar <- renderUI({
    tab <- input$main_tabs
    
    if (tab == "data") {
      # Panel pre vyber dat
      tagList(
        radioButtons("data_mode", "Work with data:",
                     choices = c("Use built-in dataset" = "builtin",
                                 "Upload data" = "upload")),
        
        conditionalPanel(
          condition = "input.data_mode == 'builtin'",
          selectInput("builtin_dataset", "Choose dataset:",
                      choices = c("mtcars", "iris", "ToothGrowth"))
        ),
        
        conditionalPanel(
          condition = "input.data_mode == 'upload'",
          fileInput("datafile", "Choose CSV file", accept = ".csv")
        ),
        
        actionButton("load_data", "Load")
      )
    } else if (tab == "joint_density") {
      
      req(loaded_data())  # zobrazi sa len ak su data nacitane
      data <- loaded_data()
      var_names <- names(data)
      
      tagList(
        h4("Density model settings"),
        
        # Vyber premennych
        checkboxGroupInput(
          "density_vars", "Select variables:",
          choices = var_names,
          selected = isolate(input$density_vars)
        ),
        
        # Typ modelu
        selectInput(
          "density_model_type", "Select model type:",
          choices = c("Parametric" = "parametric",
                      "Student-t" = "t",
                      "Kernel density" = "kernel"),
          selected = isolate(input$density_model_type)
        ),
        
        # Bandwidth – rucne nastavitelny
        checkboxInput(
          "set_bw", "Set manual bandwidth (bw)", 
          value = isolate(input$set_bw)
        ),
        
        conditionalPanel(
          condition = "input.set_bw == true",
          numericInput(
            "bw_value", "Bandwidth (0.10 – 6.00):", 
            value = isolate(ifelse(is.null(input$bw_value), 1.0, input$bw_value)),
            min = 0.10, max = 6.00, step = 0.1
          )
        ),
        
        # Vyber typu grafu
        checkboxGroupInput(
          "plot_type", "Plot type:",
          choices = c("2D", "3D"),
          selected = isolate(input$plot_type)
        ),
        
        # Pouzitie kopuly – Yes/No
        radioButtons(
          "use_copula", "Use copula:",
          choices = c("No" = "false", "Yes" = "true"),
          selected = isolate(input$use_copula)
        ),
        
        # Zobrazenie copula_type a marginal_densities len ak je zapnuta kopula
        conditionalPanel(
          condition = "input.use_copula == 'true'",
          
          # Vyber typu copuly podla modelu
          uiOutput("copula_type_ui"),
          
          # Vyber marginalnych hustot len ak model = parametric
          conditionalPanel(
            condition = "input.density_model_type == 'parametric'",
            selectInput(
              "marginal_densities", "Marginal densities:",
              choices = list(
                "log_dnorm & dnorm" = "log_dnorm, dnorm",
                "dnorm & dnorm" = "dnorm, dnorm",
                "log_dnorm & log_dnorm" = "log_dnorm, log_dnorm"
              ),
              selected = isolate(input$marginal_densities)
            )
          )
        ),
        
        # Spustacie tlacidlo
        actionButton("run_density_model", "Model")
      )
      
    } else if (tab == "gam") {
      h4("Funkcionalita pre GAM modely")
    } else if (tab == "rf") {
      h4("Funkcionalita pre Random Forest modely")
    }
  })
  
  observeEvent(input$run_density_model, {
    req(input$density_vars)
    
    selected_vars <- input$density_vars
    data <- loaded_data()
    model_type <- input$density_model_type
    plot_type <- if (length(input$plot_type) > 0) input$plot_type[1] else NULL
    use_copula <- input$use_copula == "true"
    
    bw_value <- NULL
    if (!is.null(input$set_bw) && input$set_bw) {
      bw_value <- input$bw_value
    }
    
    copula_type <- if (use_copula) input$copula_type else NULL
    
    marginal_densities <- NULL
    if (use_copula && model_type == "parametric") {
      marginal_densities <- strsplit(input$marginal_densities, ",\\s*")[[1]]
    }
    
    tryCatch({
      result <- model_joint_distribution_density(
        data = data,
        selected_variables = selected_vars,
        model_type = model_type,
        bw = bw_value,
        use_copula = use_copula,
        copula_type = copula_type,
        marginal_densities = marginal_densities,
        plot_type = plot_type
      )
      
      if (plot_type == "3D") {
        output$model_outputs_plot3d <- renderPlotly({ result })
      } else {
        output$model_outputs_plot2d <- renderPlot({ result })
      }
      
    }, error = function(e) {
      showNotification(paste("Chyba:", e$message), type = "error")
    })
  })
}