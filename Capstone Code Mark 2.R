library(shiny)
library(ggplot2)

# Load data
df1 <- read.csv("CapstoneData1part2.csv", skip = 8)
df2 <- read.csv("CapstoneData2.csv", skip = 8)
df3 <- read.csv("CapstoneData3.csv", skip = 8)
df4 <- read.csv("CapstoneData4.csv", skip = 8)
df5 <- read.csv("CapstoneData5.csv", skip = 8)

# Outlier removal using IQR fences
remove_outliers <- function(df, iqr_mult = 2.5) {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  is_outlier <- rep(FALSE, nrow(df))
  
  for (col in numeric_cols) {
    x <- df[[col]]
    q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    
    # Skip columns with no spread
    if (is.na(iqr) || iqr == 0) next
    
    lower <- q[1] - iqr_mult * iqr
    upper <- q[2] + iqr_mult * iqr
    
    flagged <- !is.na(x) & (x < lower | x > upper)
    is_outlier <- is_outlier | flagged
  }
  
  list(
    clean    = df[!is_outlier, , drop = FALSE],
    outliers = df[is_outlier, , drop = FALSE]
  )
}

# Combine, drop exact duplicate rows, then remove outliers
parse_dt <- function(x) {
  x <- trimws(x)
  out <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")      # ISO (df1)
  alt <- is.na(out)
  out[alt] <- as.POSIXct(x[alt], format = "%m/%d/%y %H:%M:%S", tz = "UTC")  # 9/21/22 4:00:00.000
  out
}

dfcombined <- unique(rbind(df1, df2, df3, df4, df5))
dfcombined$Date.Time <- parse_dt(dfcombined$Date.Time)
dfcombined$seg <- cumsum(c(0, diff(as.numeric(dfcombined$Date.Time)) > 3600))


print(sum(is.na(dfcombined$Date.Time)))
result          <- remove_outliers(dfcombined, iqr_mult = 2.5)
dfcombined      <- result$clean
dfOfMisfitsToys <- result$outliers
dfcombined <- dfcombined[order(dfcombined$Date.Time), ]

numeric_choices <- names(dfcombined)[sapply(dfcombined, is.numeric)]

print(table(as.Date(dfOfMisfitsToys$Date.Time)))  
#print(head(dfcombined$Date.Time))
#print(class(dfcombined$Date.Time))        # should be "POSIXct" "POSIXt"
#print(sum(is.na(dfcombined$Date.Time)))   # should be 0
#print(nrow(dfcombined))
#print(nrow(dfOfMisfitsToys))

ui <- fluidPage(
  h1("Capstone Project"),
  
  
  
  tabsetPanel(
    tabPanel("Welcome Page", 
             verbatimTextOutput("Welcome_text"),
             mainPanel(
               imageOutput("Brush_Mountain")
             )),
    tabPanel("Page 2", 
             selectInput(
               inputId = "column",
               label = "Select a column:",
               choices = numeric_choices
             ),
             tableOutput("data"),
             verbatimTextOutput("summary"),
             plotOutput("plot"),
    ),
    tabPanel("Page 3")
  ))

server <- function(input, output, session) {
  
  output$Welcome_text <- renderText({
    "Jackson Boran EDS capstone Project Fall 2026"})
  output$Brush_Mountain <- renderImage({
    list(src = "brushMountainPlaceholder.jpg",
         contentType = "image/jpeg",
         width = 400)
  }, deleteFile = FALSE)
  
  output$summary <- renderPrint({
    print(summary(dfcombined[[input$column]]))
    print(sum(dfcombined[[input$column]] != 0, na.rm = TRUE))
  })
  
  output$plot <- renderPlot({
    # in the plot:
    ggplot(dfcombined, aes(Date.Time, .data[[input$column]], group = seg)) + geom_line() +
      labs(y = input$column)
  })
}

shinyApp(ui, server)
