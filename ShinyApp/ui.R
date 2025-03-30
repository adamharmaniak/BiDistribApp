source("global.R")

ui <- fluidPage(
  theme = shinytheme("slate"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Horne menu (prepina len sidebar, vystup ostava rovnaky)
  tabsetPanel(id = "main_tabs",
              tabPanel("Data selection", value = "data"),
              tabPanel("Density model", value = "joint_density"),
              tabPanel("Regression", value = "regression"),
              tabPanel("Classification", value = "classification"),
              tabPanel("Conditional Densities", value = "conditional_densities")
  ),
  
  # Spolocne rozlozenie
  sidebarLayout(
    sidebarPanel(
      uiOutput("tools_sidebar")
    ),
    mainPanel(
      div(
        id = "model_outputs",
        style = "padding: 10px; margin-bottom: 30px;",
        uiOutput("model_outputs_combined")
      ),
      uiOutput("variable_info_ui")
    )
  )
)