#install.packages("shiny")
library(shiny)

ui <- fluidPage(
  "Hello, world!",
  tableOutput("data")
)

server <- function(input, output, session) {
  
  df <- read.csv("CapstoneData1.csv", header = 8)
  df2 <- read.csv("CapstoneData2.csv", skip = 8)
  
  output$data <- renderTable({
    head(df2)
  })
}

shinyApp(ui, server)