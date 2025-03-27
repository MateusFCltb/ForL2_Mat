install.packages("shiny")
library(shiny)
library(bslib)

ui <- page_sidebar(
  textInput(
    "name",
    "Your Name",
    textOutput("value")
    )
)

server <- function(input, outputs){
  
}

shinyApp(ui = ui, server = server)  
  