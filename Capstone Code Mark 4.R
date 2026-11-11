library(shiny)
library(ggplot2)
library(dplyr)

tz_sensor <- "UTC"  # Hour timestamps in CapstoneHourlyData.csv are UTC.
dfcombined <- read.csv("CapstoneHourlyData.csv")
dfcombined$Hour <- as.POSIXct(
  as.character(dfcombined$Hour),
  format = "%Y-%m-%d %H:%M:%S",
  tz = tz_sensor
)
if (anyNA(dfcombined$Hour)) {
  stop("Some Hour values could not be parsed; check CapstoneHourlyData.csv.")
}
dfcombined <- dfcombined %>% arrange(Hour)

# The exported CSV contains hourly measurements, not a seg column.
# Start a new line segment whenever consecutive hours have a gap.
dfcombined$seg <- cumsum(c(TRUE, diff(as.numeric(dfcombined$Hour)) > 3600)) - 1
numeric_choices <- setdiff(
  names(dfcombined)[vapply(dfcombined, is.numeric, logical(1))],
  c("Observations", "seg")
)
if (length(numeric_choices) == 0) stop("No numeric sensor columns found in the CSV.")

safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

first_hour <- min(dfcombined$Hour)
last_hour <- max(dfcombined$Hour)
first_date <- as.Date(first_hour, tz = tz_sensor)
last_date <- as.Date(last_hour, tz = tz_sensor)

ui <- fluidPage(
  h1("Capstone Project"),
  conditionalPanel(
    condition = "input.active_tab === 'Line Plot' || input.active_tab === 'Scatter Plot'",
    sliderInput(
      inputId = "Datevalue",
      label = "Choose a range of dates",
      min = first_hour,
      max = last_hour,
      value = c(first_hour, last_hour),
      timeFormat = "%Y-%m-%d",
      timezone = "+0000"
    ),
    dateRangeInput(
      inputId = "DatevalueB",
      label = "Choose a range of dates",
      start = first_date,
      end = last_date,
      min = first_date,
      max = last_date,
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
        inputId = "column", label = "Select a column:",
        choices = numeric_choices, multiple = TRUE,
        selected = "Temperature"
      ),
      p("Drag over the plot to see statistics for the selected points."),
      plotOutput("plot", brush = brushOpts(id = "pBrush", resetOnNew = TRUE)),
      downloadButton("download_line_plot", "Download line plot PNG"),
      verbatimTextOutput("summary")
    ),
    tabPanel(
      "Scatter Plot",
      selectInput(
        inputId = "Dategrouping", label = "Choose how you want the group to be categorized",
        choices = c("hour", "day", "week")
      ),
      selectInput("XScatter", "Select an X variable", choices = numeric_choices),
      selectInput("YScatter", "Select a Y variable", choices = numeric_choices),
      selectInput("ZScatter", "Select a Z variable", choices = numeric_choices),
      plotOutput("PlotPage3"),
      downloadButton("download_scatter_plot", "Download scatter plot PNG")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$Datevalue, {
    selected <- as.Date(input$Datevalue, tz = tz_sensor)
    if (!identical(selected, input$DatevalueB)) {
      updateDateRangeInput(session, "DatevalueB", start = selected[1], end = selected[2])
    }
  })
  
  filtered <- reactive({
    req(input$DatevalueB)
    dates <- as.Date(dfcombined$Hour, tz = tz_sensor)
    dfcombined[
      dates >= input$DatevalueB[1] & dates <= input$DatevalueB[2],
      , drop = FALSE
    ]
  })
  
  aggregated <- reactive({
    d <- filtered()
    req(nrow(d) > 0, input$Dategrouping)
    d$Hour <- lubridate::floor_date(d$Hour, input$Dategrouping, week_start = 1)
    d %>%
      group_by(Hour) %>%
      summarise(across(all_of(numeric_choices), safe_mean), .groups = "drop")
  })
  
  output$Brush_Mountain <- renderImage({
    list(src = "brushMountainPlaceholder.jpg", contentType = "image/jpeg", width = 400)
  }, deleteFile = FALSE)
  
  plot_data <- reactive({
    req(input$column)
    d <- filtered()
    req(nrow(d) > 0)
    do.call(rbind, lapply(input$column, function(col) {
      data.frame(
        Hour = d$Hour, seg = d$seg, variable = col, value = d[[col]],
        stringsAsFactors = FALSE
      )
    }))
  })
  
  line_plot <- reactive({
    ggplot(plot_data(), aes(
      x = Hour, y = value,
      group = interaction(variable, seg)
    )) +
      geom_line(color = "steelblue") +
      facet_wrap(~variable, scales = "free_y", ncol = 1) +
      labs(x = "Hour", y = "Value") +
      theme_minimal(base_size = 14)
  })
  
  output$plot <- renderPlot({ line_plot() })
  
  output$download_line_plot <- downloadHandler(
    filename = function() "capstone_line_plot.png",
    content = function(file) {
      p <- line_plot()
      ggsave(file, plot = p, device = "png", width = 10,
             height = max(5, 3 * length(input$column)), units = "in", dpi = 300)
    }
  )
  
  output$summary <- renderPrint({
    d <- plot_data()
    if (!is.null(input$pBrush)) {
      d <- brushedPoints(d, input$pBrush, xvar = "Hour", yvar = "value")
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
  
  scatter_plot <- reactive({
    req(input$XScatter, input$YScatter, input$ZScatter)
    d <- aggregated()
    ggplot(d, aes(
      x = .data[[input$XScatter]],
      y = .data[[input$YScatter]],
      color = .data[[input$ZScatter]]
    )) + geom_point() + theme_minimal(base_size = 14) + coord_fixed(ratio = 1) #see also +theme(aspect.ratio=1)
  })
  
  output$PlotPage3 <- renderPlot({ scatter_plot() })
  
  output$download_scatter_plot <- downloadHandler(
    filename = function() "capstone_scatter_plot.png",
    content = function(file) {
      ggsave(file, plot = scatter_plot(), device = "png",
             width = 10, height = 6, units = "in", dpi = 300)
    }
  )
}

shinyApp(ui, server)
