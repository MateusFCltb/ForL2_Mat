install.packages("shiny")
library(shiny)
library(bslib)

ui <- page_sidebar(
)

server <- function(input, outputs){
  
}

shinyApp(ui = ui, server = server)  
  