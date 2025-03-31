source("global.R")
source("funkcie.R")

server <- function(input, output, session) {
  quantile_inputs <- reactiveVal()
  class_predictors <- reactiveVal(c())
  cond_quantile_inputs <- reactiveVal(list(0.5))
  cond_response_type <- reactiveVal(NULL)
  loaded_data <- reactiveVal(NULL)
  
  observe({
    if (input$main_tabs == "classification" &&
        !is.null(loaded_data()) &&
        is.null(input$class_response)) {
      
      available <- identify_variables(loaded_data())$Diskretne
      if (length(available) > 0) {
        updateSelectInput(session, "class_response", selected = available[1])
      }
    }
  })
  
  observe({
    if (input$main_tabs == "classification" &&
        !is.null(loaded_data()) &&
        !is.null(input$class_response) &&
        length(class_predictors()) == 0) {
      
      all_vars <- names(loaded_data())
      available_predictors <- setdiff(all_vars, input$class_response)
      
      if (length(available_predictors) > 0) {
        class_predictors(available_predictors[1])
      }
    }
  })
  
  observe({
    if (is.null(quantile_inputs())) {
      quantile_inputs(list(0.5))
    }
  })
  
  observeEvent(input$cond_response, {
    req(loaded_data())
    var_types <- identify_variables(loaded_data())
    if (input$cond_response %in% var_types$Diskretne) {
      cond_response_type("diskretna")
    } else if (input$cond_response %in% var_types$Spojite) {
      cond_response_type("spojita")
    } else {
      cond_response_type(NULL)
    }
  })
  
  selected_density_types <- reactive({
    req(input$density_vars, loaded_data())
    variable_types <- identify_variables(loaded_data())
    
    selected <- input$density_vars
    types <- sapply(selected, function(var) {
      if (var %in% variable_types$Diskretne) {
        "diskretna"
      } else if (var %in% variable_types$Spojite) {
        "spojita"
      } else {
        "neznamy"
      }
    })
    
    types
  })
  
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
  
  # Kontextove menu + rozlozenia nastaveni
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
        h4("Density Model Settings"),
        
        checkboxGroupInput(
          "density_vars", "Select variables:",
          choices = var_names,
          selected = isolate(input$density_vars)
        ),
        
        conditionalPanel(
          condition = "output.densityVarConfig == '2_spojite' || output.densityVarConfig == 'zmiesane'",
          selectInput(
            "density_model_type", "Select model type:",
            choices = c("Parametric" = "parametric",
                        "Student-t" = "t",
                        "Kernel density" = "kernel"),
            selected = isolate(input$density_model_type)
          )
        ),
        
        conditionalPanel(
          condition = "output.densityVarConfig == 'zmiesane' && input.density_model_type == 'kernel'",
          checkboxInput(
            "set_bw", "Set manual bandwidth (bw)",
            value = isolate(input$set_bw) %||% FALSE
          ),
          conditionalPanel(
            condition = "input.set_bw == true",
            numericInput(
              "bw_value", "Bandwidth (0.10 – 100.00):",
              value = isolate(input$bw_value) %||% NULL,
              min = 0.10, max = 100.00, step = 0.1
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.densityVarConfig == '2_spojite'",
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
                  "dnorm & log_dnorm" = "dnorm, log_dnorm",
                  "dnorm & dnorm" = "dnorm, dnorm",
                  "log_dnorm & log_dnorm" = "log_dnorm, log_dnorm"
                ),
                selected = isolate(input$marginal_densities)
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.densityVarConfig != 'none'",
          checkboxGroupInput(
            "plot_type", "Plot type:",
            choices = c("2D", "3D"),
            selected = isolate(input$plot_type)
          )
        ),
        
        actionButton("run_density_model", "Model")
      )
      
    } else if (tab == "regression") {
      req(loaded_data())
      data <- loaded_data()
      var_names <- names(data)
      selected_response <- input$reg_response
      
      # Vyber len spojite premenne (prediktory)
      variable_types <- identify_variables(data)
      continuous_vars <- variable_types$Spojite
      
      available_predictors <- if (!is.null(selected_response)) {
        setdiff(continuous_vars, selected_response)
      } else {
        continuous_vars
      }
      
      tagList(
        h4("Regression Model Settings"),
        
        selectInput("reg_response", "Select response variable:",
                    choices = var_names,
                    selected = isolate(input$reg_response)),
        
        selectInput("reg_predictor", "Select predictor variable:",
                    choices = available_predictors,
                    selected = isolate(input$reg_predictor)),
        
        selectInput("mean_method", "Mean Type:",
                    choices = c("linear", "poly", "exp", "loess", "gam", "spline"),
                    selected = isolate(input$mean_method)),
        
        conditionalPanel(
          condition = "input.mean_method == 'poly'",
          numericInput("poly_mean_degree", "Polynomial degree for mean:",
                       value = isolate(input$poly_mean_degree),
                       min = 1, max = 4, step = 1)
        ),
        
        selectInput("quantile_method", "Quantiles Type:",
                    choices = c("linear", "poly", "spline"),
                    selected = isolate(input$quantile_method)),
        
        conditionalPanel(
          condition = "input.quantile_method == 'poly'",
          numericInput("poly_quant_degree", "Polynomial degree for quantiles:",
                       value = isolate(input$poly_quant_degree),
                       min = 1, max = 4, step = 1)
        ),
        
        h5("Quantiles:"),
        uiOutput("quantiles_ui"),
        actionButton("add_quantile", "Add Quantile"),
        
        uiOutput("specific_x_ui"),
        
        actionButton("run_regression_model", "Model")
      )
      
    } else if (tab == "classification") {
      req(loaded_data())
      data <- loaded_data()
      var_names <- names(data)
      variable_types <- identify_variables(data)
      
      discrete_vars <- variable_types$Diskretne
      continuous_vars <- variable_types$Spojite
      
      selected_response <- input$class_response
      used_predictors <- class_predictors() %||% character(0)
      
      # Dynamicka filtracia dostupnych prediktorov
      available_predictors <- setdiff(continuous_vars, c(selected_response, used_predictors))
      
      tagList(
        h4("Classification Model Settings"),
        
        # Vyber odozvy
        selectInput("class_response", "Select response variable:",
                    choices = discrete_vars,
                    selected = isolate(input$class_response)),
        
        # Dynamicke selectInputy pre prediktory
        uiOutput("class_predictors_ui"),
        
        div(
          div(style = "margin-bottom: 10px;",
              actionButton("add_class_predictor", "Add Predictor", width = "100%")),
          actionButton("remove_class_predictor", "Delete Predictor", width = "100%")
        ),
        
        # Vyber metody klasifikacie
        selectInput("class_method", "Classification method:",
                    choices = c("Logistic regression" = "logistic",
                                "Linear discriminant analysis (LDA)" = "lda",
                                "Quadratic discriminant analysis (QDA)" = "qda",
                                "K-nearest neighbors (KNN)" = "knn"),
                    selected = isolate(input$class_method)),
        
        # Len pri knn: checkbox + ciselny input
        conditionalPanel(
          condition = "input.class_method == 'knn'",
          checkboxInput("manual_k", "Manual", value = FALSE),
          conditionalPanel(
            condition = "input.manual_k == true",
            numericInput("k_value", "Number of neighbors (k):",
                         value = isolate(input$k_value) %||% 5,
                         min = 5, max = 50, step = 1)
          )
        ),
        
        # Spustenie modelu
        actionButton("run_classification_model", "Model")
      )
    } else if (tab == "conditional_densities") {
      req(loaded_data())
      data <- loaded_data()
      var_names <- names(data)
      
      selected_response <- input$cond_response
      response_type <- cond_response_type()
      
      tagList(
        h4("Conditional Density Model Settings"),
        
        selectInput("cond_response", "Select response variable:",
                    choices = var_names,
                    selected = isolate(input$cond_response)),
        
        selectInput("cond_predictor", "Select predictor variable:",
                    choices = setdiff(var_names, selected_response),
                    selected = isolate(input$cond_predictor)),
        
        conditionalPanel(
          condition = "output.condResponseType == 'spojita'",
          checkboxInput("mean_curve", "Visualize Mean Curve", value = TRUE),
          checkboxInput("quantile_curve", "Show Quantile Curves", value = FALSE),
          
          conditionalPanel(
            condition = "input.quantile_curve == true",
            uiOutput("cond_quantiles_ui")
          ),
          
          selectInput("cond_mean_poly_degree", "Polynomial degree of mean:",
                      choices = 1:4,
                      selected = isolate(input$cond_mean_poly_degree) %||% 1),
          
          selectInput("cond_quantile_poly_degree", "Polynomial degree of quantiles:",
                      choices = 1:4,
                      selected = isolate(input$cond_quantile_poly_degree) %||% 1),
          
          checkboxInput("normal_density", "Show normal conditional density", value = TRUE),
          checkboxInput("empirical_density", "Show empirical conditional density", value = TRUE)
        ),
        
        conditionalPanel(
          condition = "output.condResponseType != null",
          numericInput("n_breaks", "Number of subwindows (n_breaks):",
                       value = 5, min = 1, max = 10, step = 1),
          numericInput("density_scaling", "Density scaling:",
                       value = 2000, min = 1, max = 5000, step = 100)
        ),
        
        conditionalPanel(
          condition = "output.condResponseType == 'diskretna'",
          checkboxInput("ordinal", "Ordinal (for discrete responses)", value = FALSE)
        ),
        
        actionButton("run_conditional_density", "Model")
      )
    }
  })
  
  outputOptions(output, "tools_sidebar", suspendWhenHidden = FALSE)
  
  output$condResponseType <- reactive({
    cond_response_type()
  })
  outputOptions(output, "condResponseType", suspendWhenHidden = FALSE)
  
  observeEvent(input$run_density_model, {
    req(input$density_vars)
    
    selected_vars <- input$density_vars
    data <- loaded_data()
    model_type <- input$density_model_type
    selected_plot_types <- input$plot_type
    use_copula <- input$use_copula == "true"
    
    bw_value <- NULL
    if (!is.null(input$set_bw) && input$set_bw) {
      bw_value <- input$bw_value
    }
    
    copula_type <- if (use_copula) input$copula_type else NULL
    
    # Marginal densities
    marginal_densities <- NULL
    if (use_copula && model_type == "parametric") {
      marginal_densities <- strsplit(input$marginal_densities, ",\\s*")[[1]]
    }
    
    # Vymazanie predchadzajucich grafov a tabuliek
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
      
      # Rozhodnutie podla typu vystupu
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
  
  densityVarConfig <- reactive({
    types <- selected_density_types()
    
    if (length(types) != 2) return("none")
    
    n_spojite <- sum(types == "spojita")
    n_diskretne <- sum(types == "diskretna")
    
    if (n_spojite == 2) return("2_spojite")
    if (n_diskretne == 2) return("2_diskretne")
    if (n_spojite == 1 && n_diskretne == 1) return("zmiesane")
    
    return("none")
  })
  output$densityVarConfig <- densityVarConfig
  outputOptions(output, "densityVarConfig", suspendWhenHidden = FALSE)
  
  observe({
    if (input$density_model_type != "kernel" || !isTRUE(input$set_bw) || densityVarConfig() != "zmiesane") {
      updateNumericInput(session, "bw_value", value = NULL)
    }
  })
  
  # Regresia
  output$specific_x_ui <- renderUI({
    req(input$reg_predictor)
    pred <- input$reg_predictor
    values <- loaded_data()[[pred]]
    
    if (is.numeric(values)) {
      numericInput("specific_x", "Specific X value:",
                   value = mean(values, na.rm = TRUE),
                   min = min(values, na.rm = TRUE),
                   max = max(values, na.rm = TRUE))
    } else {
      helpText("Predictor must be numeric for 'specific_x'.")
    }
  })
  
  quantile_inputs <- reactiveVal(list(0.5))
  
  observeEvent(input$add_quantile, {
    req(quantile_inputs())
    current <- quantile_inputs()
    new <- c(current, 0.5)
    quantile_inputs(new)
  })
  
  output$quantiles_ui <- renderUI({
    req(quantile_inputs())
    
    inputs <- quantile_inputs()
    
    tagList(
      lapply(seq_along(inputs), function(i) {
        numericInput(
          inputId = paste0("quant_", i),
          label = paste0("Quantile ", i),
          value = inputs[[i]],
          min = 0.01, max = 0.99, step = 0.01
        )
      })
    )
  })
  
  observeEvent(input$run_regression_model, {
    req(input$reg_response, input$reg_predictor)
    
    data <- loaded_data()
    selected_vars <- c(input$reg_response, input$reg_predictor)
    
    # Modelové voľby
    mean_method <- input$mean_method
    quantile_method <- input$quantile_method
    poly_mean_degree <- if (mean_method == "poly") input$poly_mean_degree else NULL
    poly_quant_degree <- if (quantile_method == "poly") input$poly_quant_degree else NULL
    
    # Kvantily
    quant_values <- sapply(seq_along(quantile_inputs()), function(i) {
      input[[paste0("quant_", i)]]
    })
    
    # Konkrétna hodnota prediktora
    specific_x <- input$specific_x
    
    # Vyčistenie výstupov
    output$model_outputs_plot2d <- renderPlot({ NULL })
    output$model_outputs_plot3d <- renderPlotly({ NULL })
    output$model_outputs_table <- renderTable({ NULL })
    output$model_outputs_combined <- renderUI({ NULL })
    
    tryCatch({
      result <- combine_conditional_models(
        data = data,
        selected_variables = selected_vars,
        mean_method = mean_method,
        quantile_method = quantile_method,
        poly_mean_degree = poly_mean_degree,
        poly_quant_degree = poly_quant_degree,
        quantiles = quant_values,
        specific_x = specific_x
      )
      
      output$model_outputs_regression <- renderPlot({
        result$combined_plot
      })
      
      output$model_outputs_combined <- renderUI({
        tagList(
          h4("Regression Plot:"),
          plotOutput("model_outputs_regression")
        )
      })
      
    }, error = function(e) {
      showNotification(paste("Chyba:", e$message), type = "error")
    })
    
  })
  
  # Klasifikacia
  output$class_predictors_ui <- renderUI({
    if (is.null(loaded_data()) || is.null(class_predictors())) return(NULL)
    
    predictors <- class_predictors()
    selected_response <- input$class_response %||% ""
    all_vars <- names(loaded_data())
    
    tagList(
      lapply(seq_along(predictors), function(i) {
        selectInput(
          inputId = paste0("class_predictor_", i),
          label = paste("Predictor", i),
          choices = setdiff(all_vars, c(selected_response, predictors[-i])),
          selected = predictors[i]
        )
      })
    )
  })
  
  observeEvent(input$add_class_predictor, {
    req(input$class_response)
    
    current <- class_predictors() %||% c()
    if (length(current) < 2) {
      class_predictors(c(current, NA))
    }
  })
  
  observeEvent(input$remove_class_predictor, {
    current <- class_predictors()
    if (length(current) > 1) {
      class_predictors(current[-length(current)])
    }
  })
  
  observe({
    preds <- class_predictors()
    updated <- sapply(seq_along(preds), function(i) {
      input[[paste0("class_predictor_", i)]] %||% NA
    })
    class_predictors(updated)
  })
  
  observeEvent(input$run_classification_model, {
    req(input$class_response)
    
    data <- loaded_data()
    response <- input$class_response
    
    # Aktualny zoznam prediktorov zo vstupov
    predictor_ids <- class_predictors()
    predictor_names <- sapply(seq_along(predictor_ids), function(i) {
      input[[paste0("class_predictor_", i)]]
    })
    predictor_names <- predictor_names[!is.na(predictor_names)]
    
    if (length(predictor_names) == 0) {
      showNotification("Zvoľ aspoň jeden prediktor.", type = "error")
      return(NULL)
    }
    
    method <- input$class_method
    k_val <- NULL
    if (method == "knn" && input$manual_k) {
      k_val <- input$k_value
    }
    
    # Vymazanie vystupu
    output$model_outputs_combined <- renderUI({ NULL })
    output$model_outputs_classification <- renderPlot({ NULL })
    
    tryCatch({
      result <- classification_model(
        data = data,
        response_name = response,
        predictor_names = predictor_names,
        method = method,
        k = k_val
      )
      
      output$model_outputs_classification <- renderPlot({
        result$decision_plot
      })
      
      output$model_outputs_combined <- renderUI({
        tagList(
          h4("Prediction Model"),
          plotOutput("model_outputs_classification"),
          h5(paste("Presnosť modelu:", round(result$accuracy * 100, 2), "%")),
          tableOutput("confusion_matrix_table")
        )
      })
      
      output$confusion_matrix_table <- renderTable({
        result$confusion_matrix
      }, rownames = TRUE)
      
    }, error = function(e) {
      showNotification(paste("Chyba:", e$message), type = "error")
    })
  })
  
  # Podmienene hustoty
  observeEvent(input$add_cond_quantile, {
    current <- cond_quantile_inputs()
    cond_quantile_inputs(c(current, 0.5))
  })
  
  observeEvent(input$remove_cond_quantile, {
    current <- cond_quantile_inputs()
    if (length(current) > 1) {
      cond_quantile_inputs(current[-length(current)])
    }
  })
  
  observeEvent(input$quantile_curve, {
    if (!isTRUE(input$quantile_curve)) {
      cond_quantile_inputs(list(0.5))
    }
  })
  
  output$cond_quantiles_ui <- renderUI({
    req(cond_quantile_inputs())
    inputs <- cond_quantile_inputs()
    
    tagList(
      lapply(seq_along(inputs), function(i) {
        numericInput(
          inputId = paste0("cond_quant_", i),
          label = paste0("Quantile ", i),
          value = inputs[[i]],
          min = 0.01, max = 0.99, step = 0.01
        )
      }),
      div(
        style = "margin-top: 10px;",
        actionButton("add_cond_quantile", "Add Quantile", width = "49%"),
        actionButton("remove_cond_quantile", "Delete Quantile", width = "49%")
      )
    )
  })
  
  observeEvent(input$run_conditional_density, {
    req(input$cond_response, input$cond_predictor)
    
    data <- loaded_data()
    selected_vars <- c(input$cond_response, input$cond_predictor)
    
    mean_curve <- input$mean_curve
    quantiles <- if (isTRUE(input$quantile_curve)) {
      sapply(seq_along(cond_quantile_inputs()), function(i) {
        input[[paste0("cond_quant_", i)]]
      })
    } else {
      NULL
    }
    
    mean_poly_degree <- input$cond_mean_poly_degree
    quant_poly_degree <- input$cond_quantile_poly_degree
    n_breaks <- input$n_breaks
    density_scaling <- input$density_scaling
    normal_density <- input$normal_density
    empirical_density <- input$empirical_density
    ordinal <- input$ordinal
    
    if (!normal_density && !empirical_density && cond_response_type() == "spojita") {
      showNotification("At least one density type (normal or empirical) must be selected.", type = "error")
      return(NULL)
    }
    
    tryCatch({
      plot <- plot_conditional_densities(
        data = data,
        selected_variables = selected_vars,
        n_breaks = n_breaks,
        density_scaling = density_scaling,
        ordinal = ordinal,
        mean_curve = mean_curve,
        quantiles = quantiles,
        mean_poly_degree = mean_poly_degree,
        quantile_poly_degree = quant_poly_degree,
        normal_density = normal_density,
        empirical_density = empirical_density
      )
      
      output$model_outputs_conditional <- renderPlot({ plot })
      output$model_outputs_combined <- renderUI({
        tagList(
          h4("Conditional Densities Plot"),
          plotOutput("model_outputs_conditional")
        )
      })
      
    }, error = function(e) {
      showNotification(paste("Chyba:", e$message), type = "error")
    })
  })
  
}