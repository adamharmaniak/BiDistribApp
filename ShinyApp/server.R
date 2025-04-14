source("global.R")
source("funkcie.R")

options(shiny.maxRequestSize = 100 * 1024^2)

server <- function(input, output, session) {
  quantile_inputs <- reactiveVal()
  class_predictors <- reactiveVal(c())
  cond_quantile_inputs <- reactiveVal(list(0.5))
  cond_response_type <- reactiveVal(NULL)
  loaded_data <- reactiveVal(NULL)
  abort_requested <- reactiveVal(FALSE)
  clicked_points <- reactiveVal(data.frame(x = numeric(), y = numeric()))
  
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
  
  output$regressionPredictorType <- renderText({
    req(input$reg_predictor, loaded_data())
    variable_types <- identify_variables(loaded_data())
    if (input$reg_predictor %in% variable_types$Spojite) {
      "continuous"
    } else if (input$reg_predictor %in% variable_types$Diskretne) {
      "discrete"
    } else {
      ""
    }
  })
  outputOptions(output, "regressionPredictorType", suspendWhenHidden = FALSE)
  
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
  
  observeEvent(input$abort_density_model, {
    abort_requested(TRUE)
    showNotification("Aborting model evaluation...", type = "warning")
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
                      "hybrid" = c("Clayton", "Gumbel", "Joe", "empirical"),
                      "nonparametric" = c("empirical"),
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
                      choices = c("mtcars", "iris", "ToothGrowth","faithful"))
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
          condition = "output.densityVarConfig == 'zmiesane'",
          selectInput(
            "density_model_type", "Select model type:",
            choices = c("Normal" = "normal",
                        "Student-t" = "t",
                        "Kernel smoothing" = "kernel"),
            selected = isolate(input$density_model_type)
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
            condition = "input.use_copula == 'false'",
            selectInput(
              "density_model_type", "Select model type:",
              choices = c("Normal" = "normal",
                          "Student-t" = "t",
                          "Kernel smoothing" = "kernel"),
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
            condition = "input.use_copula == 'true'",
            
            selectInput(
              "density_model_type", "Select model type:",
              choices = c("Parametric" = "parametric",
                          "Non-parametric" = "nonparametric",
                          "Hybrid" = "hybrid"),
              selected = isolate(input$density_model_type)
            ),
            
            uiOutput("copula_type_ui"),
            
            conditionalPanel(
              condition = "input.density_model_type == 'parametric'",
              selectInput(
                "marginal_density_1", "Marginal density (X):",
                choices = c("dnorm", "log_dnorm", "t"),
                selected = isolate(input$marginal_density_1)
              ),
              selectInput(
                "marginal_density_2", "Marginal density (Y):",
                choices = c("dnorm", "log_dnorm", "t"),
                selected = isolate(input$marginal_density_2)
              )
            ),
            
            conditionalPanel(
              condition = "input.density_model_type == 'hybrid'",
              selectInput(
                "marginal_density_1", "Marginal density (X):",
                choices = c("dnorm", "log_dnorm", "t", "kde"),
                selected = isolate(input$marginal_density_1)
              ),
              selectInput(
                "marginal_density_2", "Marginal density (Y):",
                choices = c("dnorm", "log_dnorm", "t", "kde"),
                selected = isolate(input$marginal_density_2)
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
        
        checkboxInput("density_cut", "Density Cut", value = FALSE),
        conditionalPanel(
          condition = "input.density_cut == true",
          numericInput("num_cuts", "Number of Cuts (1–3):", value = 1, min = 1, max = 3)
        ),
        
        div(
          style = "display: flex; gap: 10px;",
          actionButton("run_density_model", "Model"),
          actionButton("abort_density_model", "Abort")
        )
      )
      
    } else if (tab == "regression") {
      
      req(loaded_data())
      data <- loaded_data()
      var_names <- names(data)
      selected_response <- input$reg_response
      
      variable_types <- identify_variables(data)
      continuous_vars <- variable_types$Spojite
      discrete_vars <- variable_types$Diskretne
      
      available_responses <- continuous_vars
      
      if (!is.null(selected_response)) {
        available_predictors <- setdiff(c(continuous_vars, discrete_vars), selected_response)
      } else {
        available_predictors <- c(continuous_vars, discrete_vars)
      }
      
      tagList(
        h4("Regression Model Settings"),
        
        selectInput("reg_response", "Select response variable:",
                    choices = available_responses,
                    selected = isolate(input$reg_response)),
        
        selectInput("reg_predictor", "Select predictor variable:",
                    choices = available_predictors,
                    selected = isolate(input$reg_predictor)),
        
        conditionalPanel(
          condition = "output.regressionPredictorType == 'continuous'",
          tagList(
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
            div(
              style = "margin-top: 10px;",
              actionButton("add_quantile", "Add Quantile", width = "49%"),
              actionButton("remove_quantile", "Delete Quantile", width = "49%")
            ),
            
            uiOutput("specific_x_ui")
          )
        ),
        
        conditionalPanel(
          condition = "output.regressionPredictorType == 'discrete'",
          selectInput("discrete_model_type", "Model for discrete predictor:",
                      choices = c(
                        "Linear regression" = "lm",
                        "GLM (log link)" = "glm_log"
                      ),
                      selected = isolate(input$discrete_model_type)
                      )
        ),
        
        div(
          style = "display: flex; gap: 10px;",
          actionButton("run_regression_model", "Model"),
          actionButton("abort_regression_model", "Abort")
        )
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
        
        div(
          style = "display: flex; gap: 10px;",
          actionButton("run_classification_model", "Model"),
          actionButton("abort_classification_model", "Abort")
        )
        
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
          checkboxInput("mean_curve", "Visualize Mean Curve", value = FALSE),
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
          checkboxInput("empirical_density", "Show empirical conditional density", value = TRUE),
          checkboxInput("manual_bw_scale", "Manual Scaling of bw", value = FALSE),
          
          conditionalPanel(
            condition = "input.manual_bw_scale == true",
            tagList(
              numericInput("bw_scale", "Bandwidth scaling (bw_scale):",
                           value = 1, min = 0.1, max = 50, step = 0.1),
              helpText("Scaling factor for bandwidth used in estimating empirical density. 
              Higher values increase smoothing; lower values make the density curve sharper.")
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.condResponseType != null",
          numericInput("n_breaks", "Number of subwindows (n_breaks):",
                       value = 5, min = 1, max = 10, step = 1),
          numericInput("density_scaling", "Density scaling:",
                       value = 100, min = 0.1, max = 100000, step = 100)
        ),
        
        conditionalPanel(
          condition = "output.condResponseType == 'diskretna'",
          checkboxInput("ordinal", "Ordinal (for discrete responses)", value = FALSE)
        ),
        
        div(
          style = "display: flex; gap: 10px;",
          actionButton("run_conditional_density", "Model"),
          actionButton("abort_conditional_density", "Abort")
        )
        
      )
    }
  })
  
  outputOptions(output, "tools_sidebar", suspendWhenHidden = FALSE)
  
  output$condResponseType <- reactive({
    cond_response_type()
  })
  outputOptions(output, "condResponseType", suspendWhenHidden = FALSE)
  
  # Zdruzena hustota
  observeEvent(input$run_density_model, {
    abort_requested(FALSE)
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
    if (use_copula && (model_type == "parametric" || model_type == "hybrid")) {
      marginal_densities <- c(input$marginal_density_1, input$marginal_density_2)
    }
    
    # Vymazanie predchadzajucich grafov a tabuliek
    output$model_outputs_plot2d <- renderPlot({ NULL })
    output$model_outputs_plot3d <- renderPlotly({ NULL })
    output$model_outputs_table <- renderTable({ NULL })
    
    clicked_points(data.frame(x = numeric(), y = numeric()))
    tryCatch({
      result <- model_joint_distribution_density(
        data = data,
        selected_variables = selected_vars,
        model_type = model_type,
        bw = bw_value,
        use_copula = use_copula,
        copula_type = copula_type,
        marginal_densities = marginal_densities,
        plot_type = if (length(selected_plot_types) > 0) selected_plot_types[1] else NULL,
        abort_signal = abort_requested
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
              plot_type = "2D",
              abort_signal = abort_requested
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
              plot_type = "3D",
              abort_signal = abort_requested
            )
            
            if (isTRUE(input$density_cut) && is.list(result3D)) {
              assign("last_density_result", list(
                x_vals = result3D$x_vals,
                y_vals = result3D$y_vals,
                z_matrix = result3D$z_matrix
              ), envir = .GlobalEnv)
              
              output$model_outputs_plot3d <- renderPlotly({
                base_plot <- result3D$plot
                
                # Prierezove krivky
                if (isTRUE(input$density_cut)) {
                  cuts <- clicked_points()
                  if (nrow(cuts) > 0) {
                    for (i in seq_len(nrow(cuts))) {
                      x_idx <- which.min(abs(result3D$x_vals - cuts$x[i]))
                      y_idx <- which.min(abs(result3D$y_vals - cuts$y[i]))
                      
                      # Slice pri fixnom x
                      base_plot <- base_plot %>% add_trace(
                        x = rep(result3D$x_vals[x_idx], length(result3D$y_vals)),
                        y = result3D$y_vals,
                        z = result3D$z_matrix[, x_idx],
                        type = "scatter3d",
                        mode = "lines",
                        line = list(color = "black", width = 4),
                        name = paste0("Slice x = ", round(result3D$x_vals[x_idx], 2))
                      )
                      
                      # Slice pri fixnom y
                      base_plot <- base_plot %>% add_trace(
                        x = result3D$x_vals,
                        y = rep(result3D$y_vals[y_idx], length(result3D$x_vals)),
                        z = result3D$z_matrix[y_idx, ],
                        type = "scatter3d",
                        mode = "lines",
                        line = list(color = "snow", width = 4),
                        name = paste0("Slice y = ", round(result3D$y_vals[y_idx], 2))
                      )
                      
                      base_plot <- base_plot %>% add_trace(
                        x = result3D$x_vals[x_idx],
                        y = result3D$y_vals[y_idx],
                        z = result3D$z_matrix[y_idx, x_idx],
                        type = "scatter3d",
                        mode = "markers",
                        marker = list(size = 6, color = "magenta"),
                        hoverinfo = "text",
                        text = paste("x =", round(result3D$x_vals[x_idx], 2),
                                     "<br>y =", round(result3D$y_vals[y_idx], 2),
                                     "<br>z =", signif(result3D$z_matrix[x_idx, y_idx], 3)),
                        name = paste0("Intersection ", i)
                      )
                    }
                  }
                }
                
                base_plot
              })
              rendered_outputs <- append(rendered_outputs, list(plotlyOutput("model_outputs_plot3d")))
            } else {
              output$model_outputs_plot3d <- renderPlotly({
                if ("plotly" %in% class(result3D)) {
                  result3D
                } else if (is.list(result3D) && "plot" %in% names(result3D)) {
                  result3D$plot
                } else {
                  NULL
                }
              })
              rendered_outputs <- append(rendered_outputs, list(plotlyOutput("model_outputs_plot3d")))
            }
          }
        }
        
        output$model_outputs_combined <- renderUI({
          tagList(
            rendered_outputs,
            uiOutput("model_outputs_cuts")
            )
        })
      }
      
    }, error = function(e) {
      showNotification(paste("Chyba:", e$message), type = "error")
    })
  })
  
  observeEvent(input$density_vars, {
    if (!is.null(input$density_vars) && length(input$density_vars) > 2) {
      updateCheckboxGroupInput(
        session,
        inputId = "density_vars",
        selected = head(input$density_vars, 2)
      )
      showNotification(
        "Please select at most two variables. This model supports visualization of two variables only.",
        type = "error"
      )
    }
  })
  
  observeEvent(event_data("plotly_click"), {
    if (isTRUE(input$density_cut)) {
      click <- event_data("plotly_click")
      if (!is.null(click)) {
        new_point <- data.frame(x = click$x, y = click$y)
        current <- clicked_points()
        if (nrow(current) < input$num_cuts) {
          clicked_points(rbind(current, new_point))
        } else {
          showNotification("Maximum number of cuts reached", type = "warning")
        }
      }
    }
  })
  
  output$cut_slices_table <- renderTable({
    req(isTRUE(input$density_cut))
    points <- clicked_points()
    if (nrow(points) == 0) return(NULL)
    req(exists("last_density_result", envir = .GlobalEnv))
    
    density_data <- get("last_density_result", envir = .GlobalEnv)
    
    slices <- lapply(seq_len(nrow(points)), function(i) {
      x_idx <- which.min(abs(density_data$x_vals - points$x[i]))
      y_idx <- which.min(abs(density_data$y_vals - points$y[i]))
      
      data.frame(
        Cut = i,
        Direction = c("x = fixed", "y = fixed"),
        Position = c(points$x[i], points$y[i]),
        Grid = c(
          paste(round(density_data$y_vals, 2), collapse = "; "),
          paste(round(density_data$x_vals, 2), collapse = "; ")
        ),
        Density = c(
          paste(round(density_data$z_matrix[x_idx, ], 6), collapse = "; "),
          paste(round(density_data$z_matrix[, y_idx], 6), collapse = "; ")
        )
      )
    })
    
    do.call(rbind, slices)
  })
  
  output$model_outputs_cuts <- renderUI({
    req(isTRUE(input$density_cut), exists("last_density_result", envir = .GlobalEnv))
    tagList(
      h4("Density Cuts Table"),
      tableOutput("cut_slices_table")
    )
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
  
  observeEvent(input$remove_quantile, {
    current <- quantile_inputs()
    if (length(current) > 1) {
      quantile_inputs(current[-length(current)])
    }
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
    
    variable_types <- identify_variables(data)
    predictor_is_continuous <- !is.null(input$reg_predictor) &&
      !is.null(variable_types$Spojite) &&
      input$reg_predictor %in% variable_types$Spojite
    
    predictor_is_discrete <- !is.null(input$reg_predictor) &&
      !is.null(variable_types$Diskretne) &&
      input$reg_predictor %in% variable_types$Diskretne
    
    mean_method <- NULL
    quantile_method <- NULL
    poly_mean_degree <- NULL
    poly_quant_degree <- NULL
    quant_values <- NULL
    specific_x <- NULL
    discrete_model_type <- NULL
    
    if (predictor_is_continuous) {
      mean_method <- input$mean_method
      quantile_method <- input$quantile_method
      poly_mean_degree <- if (mean_method == "poly") input$poly_mean_degree else NULL
      poly_quant_degree <- if (quantile_method == "poly") input$poly_quant_degree else NULL
      specific_x <- input$specific_x
      
      # Kvantily
      quant_values <- sapply(seq_along(quantile_inputs()), function(i) {
        input[[paste0("quant_", i)]]
      })
    }
    
    if (predictor_is_discrete) {
      discrete_model_type <- input$discrete_model_type
    }
    
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
        specific_x = specific_x,
        discrete_model_type = discrete_model_type
      )
      
      output$model_outputs_regression <- renderPlot({
        result$combined_plot
      })
      
      output$model_outputs_combined <- renderUI({
        ui_elements <- list(
          h4("Regression Plot:"),
          plotOutput("model_outputs_regression")
        )
        
        if (!is.null(result$mean_result) &&
            !is.null(result$mean_result$r_squared) &&
            !is.na(result$mean_result$r_squared)) {
          
          r2_label <- h5(paste("Presnosť modelu (R²):",
                               round(result$mean_result$r_squared * 100, 2), "%"))
          ui_elements <- append(ui_elements, list(r2_label))
        }
        
        tagList(ui_elements)
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
    bw_scale <- NULL
    if (isTRUE(input$manual_bw_scale)) {
      bw_scale <- input$bw_scale
    }
    
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
        empirical_density = empirical_density,
        bw_scale = bw_scale
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