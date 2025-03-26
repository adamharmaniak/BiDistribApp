source("global.R")
source("funkcie.R")

server <- function(input, output, session) {
  
  data <- reactive({
    if (input$data_source == "default") {
      return(mtcars)
    } else {
      req(input$datafile)
      return(read.csv(input$datafile$datapath))
    }
  })
  
  output$summaryVars <- renderPrint({
    req(data())
    printVariables(data())
  })
  
  output$var_selection_ui <- renderUI({
    req(data())
    variables <- identify_variables(data())
    
    tagList(
      selectInput("selected_vars", "Vyber 2 premenné:", 
                  choices = c(variables$Diskretne, variables$Spojite),
                  selected = c(variables$Diskretne[1], variables$Spojite[1]),
                  multiple = TRUE),
      conditionalPanel(
        condition = "input.model_type == 'kernel'",
        numericInput("bw", "Šírka pásma (bandwidth):", value = NA)
      ),
      conditionalPanel(
        condition = "input.model_type == 'Kopula'",
        selectInput("copula_type", "Typ kopuly:", choices = c("Clayton", "Gumbel", "Frank", "Joe", "empirical")),
        checkboxInput("use_copula", "Použiť kopulu", value = TRUE)
      )
    )
  })
  
  output$jointPlot <- renderPlot({
    req(data(), input$selected_vars, input$model_type)
    
    model_type <- switch(input$model_type,
                         "Kopula" = "parametric",
                         "GAM" = "kernel",
                         "Random Forest" = "t")
    
    validate(need(length(input$selected_vars) == 2, "Vyber presne 2 premenné."))
    
    result <- model_joint_distribution_density(
      data = data(),
      selected_variables = input$selected_vars,
      model_type = model_type,
      bw = ifelse(is.na(input$bw), NULL, input$bw),
      use_copula = ifelse(is.null(input$use_copula), FALSE, input$use_copula),
      copula_type = input$copula_type
    )
    
    print(result$final_plot_2d)
  })
}