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
      
      req(loaded_data())
      data <- loaded_data()
      var_names <- names(data)
      
      tagList(
        h4("Density model settings"),
        
        checkboxGroupInput(
          "density_vars", "Select variables:",
          choices = var_names,
          selected = isolate(input$density_vars)
        ),
        
        selectInput(
          "density_model_type", "Select model type:",
          choices = c("Parametric" = "parametric",
                      "Student-t" = "t",
                      "Kernel density" = "kernel"),
          selected = isolate(input$density_model_type)
        ),
        
        checkboxInput(
          "set_bw", "Set manual bandwidth (bw)",
          value = isolate(input$set_bw) %||% FALSE
        ),
        
        conditionalPanel(
          condition = "input.set_bw == true",
          numericInput(
            "bw_value", "Bandwidth (0.10 – 6.00):",
            value = isolate(input$bw_value) %||% NULL,
            min = 0.10, max = 6.00, step = 0.1
          )
        ),
        
        checkboxGroupInput(
          "plot_type", "Plot type:",
          choices = c("2D", "3D"),
          selected = isolate(input$plot_type)
        ),
        
        radioButtons(
          "use_copula", "Use copula:",
          choices = c("No" = "false", "Yes" = "true"),
          selected = isolate(input$use_copula)
        ),
        
        conditionalPanel(
          condition = "input.use_copula == 'true'",
          
          uiOutput("copula_type_ui"),
          
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
    selected_plot_types <- input$plot_type
    use_copula <- input$use_copula == "true"
    
    # Bandwidth
    bw_value <- NULL
    if (!is.null(input$set_bw) && input$set_bw) {
      bw_value <- input$bw_value
    }
    
    # Copula type
    copula_type <- if (use_copula) input$copula_type else NULL
    
    # Marginal densities
    marginal_densities <- NULL
    if (use_copula && model_type == "parametric") {
      marginal_densities <- strsplit(input$marginal_densities, ",\\s*")[[1]]
    }
    
    # Vymazanie predchádzajúcich grafov a tabuliek
    output$model_outputs_plot2d <- renderPlot({ NULL })
    output$model_outputs_plot3d <- renderPlotly({ NULL })
    output$model_outputs_table <- renderTable({ NULL })
    
    tryCatch({
      result <- model_joint_distribution_density(
        data = data,
        selected_variables = selected_vars,
        model_type = model_type,
        bw = bw_value,
        use_copula = use_copula,
        copula_type = copula_type,
        marginal_densities = marginal_densities,
        plot_type = if (length(selected_plot_types) > 0) selected_plot_types[1] else NULL
      )
      
      # Rozhodnutie podľa typu výstupu
      if (is.data.frame(result)) {
        output$model_outputs_combined <- renderUI({
          tagList(
            h4("Joint distribution table:"),
            tableOutput("model_outputs_table")
          )
        })
        
        output$model_outputs_table <- renderTable({
          result
        })
        
      } else {
        rendered_outputs <- list()
        
        for (plot_type in selected_plot_types) {
          if (plot_type == "2D") {
            result2D <- model_joint_distribution_density(
              data = data,
              selected_variables = selected_vars,
              model_type = model_type,
              bw = bw_value,
              use_copula = use_copula,
              copula_type = copula_type,
              marginal_densities = marginal_densities,
              plot_type = "2D"
            )
            output$model_outputs_plot2d <- renderPlot({ result2D })
            rendered_outputs <- append(rendered_outputs, list(plotOutput("model_outputs_plot2d")))
          }
          
          if (plot_type == "3D") {
            result3D <- model_joint_distribution_density(
              data = data,
              selected_variables = selected_vars,
              model_type = model_type,
              bw = bw_value,
              use_copula = use_copula,
              copula_type = copula_type,
              marginal_densities = marginal_densities,
              plot_type = "3D"
            )
            output$model_outputs_plot3d <- renderPlotly({ result3D })
            rendered_outputs <- append(rendered_outputs, list(plotlyOutput("model_outputs_plot3d")))
          }
        }
        
        output$model_outputs_combined <- renderUI({
          tagList(rendered_outputs)
        })
      }
      
    }, error = function(e) {
      showNotification(paste("Chyba:", e$message), type = "error")
    })
  })
}