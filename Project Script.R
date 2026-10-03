library(shiny)
library(ggplot2)

# Load data
df1 <- read.csv("CapstoneData1part2.csv", skip = 8)
df2 <- read.csv("CapstoneData2.csv", skip = 8)
df3 <- read.csv("CapstoneData3.csv", skip = 8)
df4 <- read.csv("CapstoneData4.csv", skip = 8)
df5 <- read.csv("CapstoneData5.csv", skip = 8)

dfOfMisfitsToys <- data.frame()
dfcombined <- rbind(df1, df2, df3, df4, df5)
#here I sort data for outliers and duplicates


clean_column <- function(column) {
for(data in column) {
  if(data > (mean(column)+3*(sd(column))) || data < (mean(column)- 3*(sd(column)))){
    
  }
  
}
}  
ui <- fluidPage(
  
  h1("Data Explorer"),
  
  selectInput(
    inputId = "column",
    label = "Select a column:",
    choices = names(dfcombined)
  ),
  
  tableOutput("data"),
  
  verbatimTextOutput("summary"),
  
  plotOutput("plot")
)

server <- function(input, output, session) {
  
  #need a better home page
  #and a delay

  output$summary <- renderPrint({
    print(summary(dfcombined[, input$column]))
    print(sum(dfcombined[[input$column]] != 0, na.rm = TRUE))
  })
 
  #my data still needs to be cleaned for major outliers/figure out why they're there - I think it has to do with sensor battery life. 
  output$plot <- renderPlot({
    ggplot(data=dfcombined, aes(x = Date.Time, y =.data[[input$column]])) + 
    geom_line() +
    labs(y = "Plot title")
  })
}

shinyApp(ui, server)