source("global.R")

ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel("Joint probability distribution modelling"),
  
  # Horné menu (prepína len sidebar, výstup ostáva rovnaký)
  tabsetPanel(id = "main_tabs",
              tabPanel("Data selection", value = "data"),
              tabPanel("Density model", value = "joint_density"),
              tabPanel("GAM model", value = "gam"),
              tabPanel("Random Forest", value = "rf")
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
        
        # Kombinovaný výstup: najprv plotly, potom plot
        conditionalPanel("input.plot_type.includes('3D')",
                         plotlyOutput("model_outputs_plot3d")),
        conditionalPanel("input.plot_type.includes('2D')",
                         plotOutput("model_outputs_plot2d"))
      ),
      uiOutput("variable_info_ui")
    )
  )
)