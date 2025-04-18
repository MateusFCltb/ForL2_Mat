# Shiny App for Forest Inventory Analysis with Modular Data Loader

library(shiny)
library(terra)
library(tmap)
library(sf)

source("func_Load_Data.R")  # This script must define load_forest_data()

library(bslib)

ui <- page_sidebar(
  title = "Extract Slope and Aspect",
  sidebar = sidebar(
    textInput("filepath_input", "File Path (Folder Containing Data)", value = "C:/Users/mateuscaltabiano/GitProjects/ForL2_Mat/files/"),
    numericInput("buffer_radius", "Buffer Radius (m)", value = 17.83, step = 0.1),
    actionButton("process", "Process Data")
  ),
  navset_tab(
    nav_panel("DEM", tmapOutput("dem_map")),
    nav_panel("Extract Slope and Aspect",
              selectInput("layer_select", "Choose an option:",
                          choices = c("Slope", "Aspect", "Reclassified Aspect"),
                          selected = "Slope"),
              tmapOutput("slope_aspect_dynamic")
    ),
    nav_panel("Species by Aspect", tmapOutput("species_aspect_map")),
    nav_panel("Species by Slope", tmapOutput("species_slope_map"))
  )
)

server <- function(input, output, session) {
  options(tmap.mode = "view")
  
  data <- reactiveValues()
  
  observeEvent(input$process, {
    req(input$filepath_input)
    result <- load_forest_data(input$filepath_input, buffer_radius = input$buffer_radius)
    
    data$dem <- result$dem
    data$slope <- result$slope
    data$aspect <- result$aspect
    data$asp_class <- result$asp
    data$sf_plot <- result$sf_plot
  })
  
  output$dem_map <- renderTmap({
    req(data$dem)
    tm_shape(data$dem) + tm_raster(title = "DEM")
  })
  
  output$slope_aspect_dynamic <- renderTmap({
    req(input$layer_select)
    if (input$layer_select == "Slope") {
      req(data$slope)
      tm_shape(data$slope) + tm_raster(style = "cont", title = "Slope (deg)")
    } else if (input$layer_select == "Aspect") {
      req(data$aspect)
      tm_shape(data$aspect) + tm_raster(style = "cont", title = "Aspect (deg)")
    } else if (input$layer_select == "Reclassified Aspect") {
      req(data$asp_class)
      tm_shape(data$asp_class) +
        tm_raster(style = "cat",
                  palette = c("white", "blue", "green", "yellow", "red"),
                  labels = c(NA, "North", "East", "South", "West"),
                  alpha = 0.2)
    }
  })
  
  output$species_aspect_map <- renderTmap({
    req(data$asp_class, data$sf_plot)
    tm_shape(data$asp_class, alpha = 0.5) +
      tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow", "red"),
                showNA = FALSE, alpha = 0.2,
                labels = c(NA, "North", "East", "South", "West")) +
      tm_shape(data$sf_plot) +
      tm_polygons("Common.name") +
      tm_layout(legend.outside = TRUE, legend.outside.size = 0.2) +
      tm_text("Plot", ymod = -0.9)
  })
  
  output$species_slope_map <- renderTmap({
    req(data$slope, data$sf_plot)
    tm_shape(data$slope, alpha = 0.5) +
      tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)") +
      tm_shape(data$sf_plot) +
      tm_polygons("Common.name", title = "Dominant Species", alpha = 0.6) +
      tm_layout(title = "Dominant Trees by Slope", legend.outside = TRUE, legend.outside.size = 0.2) +
      tm_text("Plot", ymod = -0.9, size = 1.2)
  })
}

shinyApp(ui, server)
