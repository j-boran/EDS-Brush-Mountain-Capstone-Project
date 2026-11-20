install.packages("shiny")
install.packages("readr")
library(shiny)
library(readr)

ui <- fluidPage(
  tableOutput("github_data")
)

server <- function(input, output, session) {
  data_url <- "https://raw.githubusercontent.com/j-boran/EDS-Brush-Mountain-Capstone-Project/main/CapstoneData1.csv"
  
  my_data <- reactive({
    tryCatch({
      read_csv(data_url)
    }, error = function(e) {
      showNotification(paste("Failed to load data:", e$message), type = "error")
      NULL
    })
  })
  
  output$github_data <- renderTable({
    req(my_data())
    my_data()
  })
}

shinyApp(ui, server)
