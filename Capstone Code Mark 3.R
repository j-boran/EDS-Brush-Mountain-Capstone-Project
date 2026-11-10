dfcombined <- read.csv("CapstoneHourlyData.csv")
numeric_choices <- expected_names[-1]
print(head(dfcombined))

ui <- fluidPage(
  h1("Capstone Project"),
  
  conditionalPanel(
    condition = "input.active_tab === 'Page 2' || input.active_tab === 'Page 3'",
    sliderInput(
      inputId = "Datevalue",
      label = "Choose a range of dates",
      min = min(dfcombined$Hour, na.rm = TRUE),
      max = max(dfcombined$Hour, na.rm = TRUE),
      value = range(dfcombined$Hour, na.rm = TRUE),
      timeFormat = "%Y-%m-%d",
      timezone = "+0000"
    ),
    dateRangeInput(
      inputId = "DatevalueB",
      label = "Choose a range of dates",
      start = as.Date(min(dfcombined$Hour, na.rm = TRUE), tz = "UTC"),
      end = as.Date(max(dfcombined$Hour, na.rm = TRUE), tz = "UTC"),
      min = as.Date(min(dfcombined$Hour, na.rm = TRUE), tz = "UTC"),
      max = as.Date(max(dfcombined$Hour, na.rm = TRUE), tz = "UTC"),
      format = "yyyy-mm-dd"
    )
  ),
  
  tabsetPanel(
    id = "active_tab",
    tabPanel(
      "Home Page",
      h2("Jackson Boran EDS Capstone Project Fall 2026"),
      p("This project is a web application used to visualize watershed sensor data.
         The data will be taken from Dr. JP Gannon's personal sensor, which saves and
         sends data to a server. The details of the data will be explored later in
         the methods section."),
      p("This project aims to take data from the server, then transform it into an
         interactive user interface that will allow visualizations and statistics of
         the collected data. The focus of this project will be on the development of code
         to accept and transform the raw data, and on interactive visualizations
         that allow the user of the app to generate chosen visualizations based on
         selected data."),
      p("Brush Mountain is a hiking trail in Blacksburg Virginia. I sure wish I knew more about it,
         so I could type it here and give more context to the reader."),
      imageOutput("Brush_Mountain")
    ),
    tabPanel(
      "Line Plot",
      selectInput(
        inputId = "column",
        label = "Select a column:",
        choices = numeric_choices,
        multiple = TRUE,
        selected = "Temperature"
      ),
      p("Drag over the plot to see statistics for the selected points."),
      plotOutput("plot", brush = brushOpts(id = "pBrush", resetOnNew = TRUE)),
      verbatimTextOutput("summary")
    ),
    tabPanel(
      "Scatter Plot",
      selectInput(
        inputId = "Dategrouping",
        label = "Choose how you want the group to be categorized",
        choices = c("hour", "day", "week")
      ),
      selectInput(
        inputId = "XScatter",
        label = "Select an X variable",
        choices = numeric_choices
      ),
      selectInput(
        inputId = "YScatter",
        label = "Select a Y variable",
        choices = numeric_choices
      ),
      selectInput(
        inputId = "ZScatter",
        label = "Select a Z variable",
        choices = numeric_choices
      ),
      plotOutput("PlotPage3")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$Datevalue, {
    req(input$DatevalueB)
    selected <- as.Date(input$Datevalue, tz = "UTC")
    if (!identical(selected, input$DatevalueB)) {
      updateDateRangeInput(session, "DatevalueB",
                           start = selected[1], end = selected[2])
    }
  })
  
  filtered <- reactive({
    req(input$DatevalueB)
    dates <- as.Date(dfcombined$Hour, tz = "UTC")
    dfcombined[!is.na(dates) &
                 dates >= input$DatevalueB[1] &
                 dates <= input$DatevalueB[2], , drop = FALSE]
  })
  
  aggregated <- reactive({
    d <- filtered()
    d$Hour <- lubridate::floor_date(d$Hour, input$Dategrouping,
                                         week_start = 1)
    aggregate(d[setdiff(numeric_choices, "seg")],
              list(Hour = d$Hour), mean, na.rm = TRUE)
  })
  
  output$Brush_Mountain <- renderImage({
    list(src = "brushMountainPlaceholder.jpg",
         contentType = "image/jpeg",
         width = 400)
  }, deleteFile = FALSE)
  
  # Build one dataset for plotting and brushing.
  plot_data <- reactive({
    req(input$column)
    d <- filtered()
    req(nrow(d) > 0)
    
    do.call(rbind, lapply(input$column, function(col) {
      data.frame(
        Hour = d$Hour,
        seg = d$seg,
        variable = col,
        value = d[[col]],
        stringsAsFactors = FALSE
      )
    }))
  })
  
  output$plot <- renderPlot({
    ggplot(plot_data(),
           aes(x = Hour, y = value, color = variable,
               group = interaction(variable, seg))) +
      geom_line()
  })
  
  output$summary <- renderPrint({
    d <- plot_data()
    if (!is.null(input$pBrush)) {
      d <- brushedPoints(d, input$pBrush,
                         xvar = "Hour", yvar = "value")
      cat("Statistics for brushed points\n\n")
    } else {
      cat("Statistics for all points in the selected date range\n\n")
    }
    
    if (nrow(d) == 0) {
      cat("No points selected.")
      return()
    }
    
    for (col in input$column) {
      values <- d$value[d$variable == col]
      if (length(values) == 0) next
      cat(col, "-", length(values), "points\n")
      print(summary(values))
      cat("Nonzero values:", sum(values != 0, na.rm = TRUE), "\n\n")
    }
  })
  
  output$PlotPage3 <- renderPlot({
    req(input$XScatter, input$YScatter, input$ZScatter)
    d <- aggregated()
    
    ggplot(d, aes(x = .data[[input$XScatter]],
                  y = .data[[input$YScatter]],
                  color = .data[[input$ZScatter]])) +
      geom_point()
  })
}

shinyApp(ui, server)

