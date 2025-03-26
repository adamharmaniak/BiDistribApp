source("C:/Users/Adam/STUBA/Rocnik_3/Bakalárska_práca/aplikacie/ShinyApp/global.R")

ui <- fluidPage(
  titlePanel("Modelovanie spoločného rozdelenia zmesi premenných"),
  
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("data_source", "Vyber zdroj dát:",
                   choices = c("Predvolený dataset (mtcars)" = "default",
                               "Nahrať vlastný CSV súbor" = "upload")),
      
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("datafile", "Nahraj CSV súbor")
      ),
      selectInput("model_type", "Zvoľ typ modelu:",
                  choices = c("Kopula", "GAM", "Random Forest")),
      uiOutput("var_selection_ui")
    ),
    
    mainPanel(
      h4("Vizualizácia spoločného rozdelenia"),
      plotOutput("jointPlot"),
      h4("Informácie o premenných"),
      verbatimTextOutput("summaryVars")
    )
  )
)