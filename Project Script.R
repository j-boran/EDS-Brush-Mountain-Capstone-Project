library(shiny)

# Load data
#df1 is currently broken, oops.
df2 <- read.csv("CapstoneData2.csv", skip = 8)
df3 <- read.csv("CapstoneData3.csv", skip = 8)
df4 <- read.csv("CapstoneData4.csv", skip = 8)
df5 <- read.csv("CapstoneData5.csv", skip = 8)

dfcombined <- rbind(df2, df3, df4, df5)

ui <- fluidPage(
  
  h1("Data Explorer"),
  
  selectInput(
    inputId = "column",
    label = "Select a column:",
    choices = names(dfcombined)
  ),
  
  tableOutput("data"),
  
  verbatimTextOutput("summary")
)

server <- function(input, output, session) {
  

  output$summary <- renderPrint({
    summary(dfcombined[, input$column])
  })
  
  output$plot <- renderPlot({
    ggplot(aes(dfcombined[, input$column])) + 
    geom_line() +
    labs(y = "Plot title")
  })
}

shinyApp(ui, server)