source("global.R")

ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel("Joint probability distribution modelling"),
  
  # Horné menu (prepína len sidebar, výstup ostáva rovnaký)
  tabsetPanel(id = "main_tabs",
              tabPanel("Data selection", value = "data"),
              tabPanel("Density model", value = "joint_density"),
              tabPanel("Regression", value = "regression"),
              tabPanel("Classification", value = "classification"),
              tabPanel("Conditional Densities", value = "conditional_densities")
  ),
  
  # Spoločné rozloženie
  sidebarLayout(
    sidebarPanel(
      uiOutput("tools_sidebar")
    ),
    mainPanel(
      div(
        id = "model_outputs",
        style = "border: 1px dashed gray; padding: 10px; margin-bottom: 30px;",
        uiOutput("model_outputs_combined")
      ),
      uiOutput("variable_info_ui")
    )
  )
)