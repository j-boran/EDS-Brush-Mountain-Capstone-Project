library(shiny)
library(ggplot2)
library(dplyr)

tz_sensor <- "UTC"
dfcombined <- read.csv("CapstoneHourlyData.csv")
dfcombined$Hour <- as.POSIXct(
  as.character(dfcombined$Hour), format = "%Y-%m-%d %H:%M:%S", tz = tz_sensor
)
if (anyNA(dfcombined$Hour)) stop("Some Hour values could not be parsed.")
dfcombined <- arrange(dfcombined, Hour)

numeric_choices <- setdiff(
  names(dfcombined)[vapply(dfcombined, is.numeric, logical(1))],
  "Observations"
)
if (!length(numeric_choices)) stop("No numeric sensor columns found.")
default_column <- if ("Depth..m." %in% numeric_choices) "Depth..m." else numeric_choices[1]
# Start the scatter plot with two different sensors that have paired readings.
default_x <- numeric_choices[1]
other_choices <- setdiff(numeric_choices, default_x)
default_y <- if (length(other_choices)) {
  paired <- vapply(other_choices, function(col) {
    sum(is.finite(dfcombined[[default_x]]) & is.finite(dfcombined[[col]]))
  }, integer(1))
  other_choices[which.max(paired)]
} else default_x

safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
first_hour <- min(dfcombined$Hour)
last_hour <- max(dfcombined$Hour)
first_date <- as.Date(first_hour, tz = tz_sensor)
last_date <- as.Date(last_hour, tz = tz_sensor)

ui <- fluidPage(
  tags$head(tags$style(HTML("\n    body, .form-control, .btn, .selectize-input, .selectize-dropdown {
      font-size: calc(14px + 3pt);
    }
    h1 { font-size: calc(36px + 3pt); }
    h2 { font-size: calc(30px + 3pt); }
    pre { font-size: calc(13px + 3pt); }
  "))),
  h1("Capstone Project"),
  conditionalPanel(
    condition = "input.active_tab === 'Line Plot' || input.active_tab === 'Scatter Plot'",
    sliderInput(
      "Datevalue", "Choose a range of dates",
      min = first_hour, max = last_hour, value = c(first_hour, last_hour),
      timeFormat = "%Y-%m-%d %H:%M", timezone = "+0000"
    ),
    dateRangeInput(
      "DatevalueB", "Or enter dates",
      start = first_date, end = last_date,
      min = first_date, max = last_date, format = "yyyy-mm-dd"
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
      selectInput("column", "Select a column:", numeric_choices,
                  multiple = TRUE, selected = default_column),
      p("Drag across the plot to zoom; double-click the plot or use Reset zoom to restore all dates."),
      plotOutput("plot", brush = brushOpts(id = "pBrush", resetOnNew = TRUE),
                 dblclick = "pDblclick", width = "100%", height = "800px"),
      actionButton("reset_zoom", "Reset zoom"),
      downloadButton("download_line_plot", "Download line plot PNG"),
      verbatimTextOutput("summary")
    ),
    tabPanel(
      "Scatter Plot",
      selectInput("Dategrouping", "Choose how you want the group to be categorized",
                  c("hour", "day", "week")),
      selectInput("XScatter", "Select an X variable", numeric_choices, selected = default_x),
      selectInput("YScatter", "Select a Y variable", numeric_choices, selected = default_y),
      selectInput("ZScatter", "Select a Z variable", numeric_choices, selected = default_x),
      actionButton("fit_toggle", "Switch to logarithmic fit"),
      textOutput("fit_status"),
      plotOutput("PlotPage3"),
      downloadButton("download_scatter_plot", "Download scatter plot PNG"),
      verbatimTextOutput("fit_summary")
    )
  )
)

server <- function(input, output, session) {
  logarithmic_fit <- reactiveVal(FALSE)
  observeEvent(input$fit_toggle, {
    logarithmic_fit(!logarithmic_fit())
    updateActionButton(session, "fit_toggle", label =
                         if (logarithmic_fit()) "Switch to linear fit" else "Switch to logarithmic fit")
  })
  
  observeEvent(input$Datevalue, {
    selected <- as.Date(input$Datevalue, tz = tz_sensor)
    if (!identical(selected, input$DatevalueB)) {
      updateDateRangeInput(session, "DatevalueB", start = selected[1], end = selected[2])
    }
  })
  observeEvent(input$DatevalueB, {
    req(length(input$DatevalueB) == 2)
    if (!identical(as.Date(input$Datevalue, tz = tz_sensor), input$DatevalueB)) {
      start <- as.POSIXct(input$DatevalueB[1], tz = tz_sensor)
      end <- as.POSIXct(input$DatevalueB[2], tz = tz_sensor) + 86399
      updateSliderInput(session, "Datevalue",
                        value = c(max(first_hour, start), min(last_hour, end)))
    }
  })
  observeEvent(input$pBrush, {
    bounds <- sort(c(input$pBrush$xmin, input$pBrush$xmax))
    zoom_hours <- as.POSIXct(bounds, origin = "1970-01-01", tz = tz_sensor)
    if (diff(bounds) > 0 &&
        any(dfcombined$Hour >= zoom_hours[1] & dfcombined$Hour <= zoom_hours[2])) {
      updateSliderInput(session, "Datevalue", value = zoom_hours)
    }
  })
  observeEvent(input$pDblclick, {
    updateSliderInput(session, "Datevalue", value = c(first_hour, last_hour))
    session$resetBrush("pBrush")
  })
  observeEvent(input$reset_zoom, {
    updateSliderInput(session, "Datevalue", value = c(first_hour, last_hour))
    session$resetBrush("pBrush")
  })
  
  filtered <- reactive({
    req(input$Datevalue)
    dfcombined[
      dfcombined$Hour >= input$Datevalue[1] &
        dfcombined$Hour <= input$Datevalue[2], , drop = FALSE
    ]
  })
  aggregated <- reactive({
    d <- filtered()
    req(nrow(d) > 0, input$Dategrouping)
    d$Hour <- lubridate::floor_date(d$Hour, input$Dategrouping, week_start = 1)
    d %>% group_by(Hour) %>%
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
      data.frame(Hour = d$Hour, variable = col, value = d[[col]])
    }))
  })
  line_plot <- reactive({
    d <- plot_data()
    visible <- d[is.finite(d$value) & !is.na(d$Hour), , drop = FALSE]
    validate(need(nrow(visible) > 0, "No measurements for this sensor and date range."))
    visible <- arrange(visible, variable, Hour)
    # A segment is specific to each sensor: never bridge missing measurement hours.
    visible$seg <- 0L
    for (rows in split(seq_len(nrow(visible)), visible$variable)) {
      visible$seg[rows] <- cumsum(c(TRUE, diff(as.numeric(visible$Hour[rows])) > 3600))
    }
    
    ggplot(visible, aes(x = Hour, y = value,
                        group = interaction(variable, seg))) +
      geom_line(color = "steelblue", linewidth = 0.4) +
      geom_point(color = "steelblue", size = 0.4, alpha = 0.5) +
      facet_wrap(~variable, scales = "free_y", ncol = 1) +
      labs(x = "Hour", y = "Value") + theme_minimal(base_size = 14)
  })
  output$plot <- renderPlot(line_plot())
  output$download_line_plot <- downloadHandler(
    filename = function() "capstone_line_plot.png",
    content = function(file) {
      ggsave(file, plot = line_plot(), device = "png", width = 10,
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
    if (!nrow(d)) { cat("No points selected."); return() }
    for (col in input$column) {
      values <- d$value[d$variable == col]
      if (!length(values)) next
      measured <- values[is.finite(values)]
      cat(col, "-", length(measured), "measurements;",
          sum(is.na(values)), "missing hours\n")
      if (length(measured)) print(summary(measured))
      cat("Nonzero values:", sum(measured != 0), "\n\n")
    }
  })
  scatter_data <- reactive({
    req(input$XScatter, input$YScatter)
    d <- aggregated()
    # Fit and scatter points need paired X/Y values only; Z is just point color.
    d[is.finite(d[[input$XScatter]]) &
        is.finite(d[[input$YScatter]]), , drop = FALSE]
  })
  fit_model <- reactive({
    d <- scatter_data()
    x <- d[[input$XScatter]]
    y <- d[[input$YScatter]]
    usable <- if (logarithmic_fit()) x > 0 else rep(TRUE, length(x))
    x <- x[usable]
    y <- y[usable]
    if (length(x) < 2 || length(unique(x)) < 2) return(NULL)
    
    model <- if (logarithmic_fit()) {
      lm(y ~ log(x), data = data.frame(x = x, y = y))
    } else {
      lm(y ~ x, data = data.frame(x = x, y = y))
    }
    list(model = model, x = x, n = length(x))
  })
  fit_line <- reactive({
    fit <- fit_model()
    if (is.null(fit)) return(NULL)
    grid <- data.frame(x = seq(min(fit$x), max(fit$x), length.out = 200))
    data.frame(x = grid$x, y = predict(fit$model, newdata = grid), n = fit$n)
  })
  output$fit_summary <- renderText({
    fit <- fit_model()
    if (is.null(fit)) return("Equation and R-squared unavailable: insufficient paired X/Y values for this fit.")
    
    coefficients <- coef(fit$model)
    number <- function(value) formatC(value, digits = 6, format = "g")
    x_term <- if (logarithmic_fit()) {
      paste0("ln(", input$XScatter, ")")
    } else input$XScatter
    equation <- paste0(
      input$YScatter, " = ", number(coefficients[1]),
      if (coefficients[2] < 0) " - " else " + ",
      number(abs(coefficients[2])), " × ", x_term
    )
    r_squared <- summary(fit$model)$r.squared
    paste0("Best-fit equation: ", equation, "\nR-squared: ",
           if (is.finite(r_squared)) number(r_squared) else "undefined",
           "\nPaired X/Y points used: ", fit$n)
  })
  output$fit_status <- renderText({
    type <- if (logarithmic_fit()) "Logarithmic" else "Linear"
    line <- fit_line()
    if (is.null(line)) {
      paste(type, "fit unavailable: select sensors with at least two distinct paired X values",
            if (logarithmic_fit()) "greater than zero." else ".")
    } else {
      paste0(type, " fit of Y against X (", line$n[1],
             " paired X/Y points; Z only controls point color).",
             if (input$XScatter == input$YScatter)
               " X and Y are the same sensor; choose different variables to compare them." else "")
    }
  })
  scatter_plot <- reactive({
    d <- scatter_data()
    validate(need(nrow(d) > 0, "No paired measurements for these variables and dates."))
    p <- ggplot(d, aes(x = .data[[input$XScatter]], y = .data[[input$YScatter]],
                       color = .data[[input$ZScatter]])) +
      geom_point() + scale_color_viridis_c(na.value = "grey70") +
      theme_minimal(base_size = 14) + theme(aspect.ratio = 1)
    line <- fit_line()
    if (!is.null(line)) {
      p <- p + geom_line(data = line, aes(x = x, y = y),
                         inherit.aes = FALSE, color = "firebrick", linewidth = 1)
    }
    p
  })
  output$PlotPage3 <- renderPlot(scatter_plot())
  output$download_scatter_plot <- downloadHandler(
    filename = function() "capstone_scatter_plot.png",
    content = function(file) {
      ggsave(file, plot = scatter_plot(), device = "png",
             width = 10, height = 6, units = "in", dpi = 300)
    }
  )
}

shinyApp(ui, server)
