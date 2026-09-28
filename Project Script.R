#install.packages("shiny")
library(shiny)

ui <- fluidPage(
  "Hello, world!",
  tableOutput("data")
)

server <- function(input, output, session) {
  
  df <- read.csv("CapstoneData1.csv", skip = 8)
  
  output$data <- renderTable({
    head(df)
  })
}

shinyApp(ui, server)
