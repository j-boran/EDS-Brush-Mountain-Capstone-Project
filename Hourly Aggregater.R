library(dplyr)

files <- c(
  "CapstoneData1part2.csv", "CapstoneData2.csv", "CapstoneData3.csv",
  "CapstoneData4.csv", "CapstoneData5.csv"
)

expected_names <- c(
  "Date.Time", "Temperature", "Turbidity", "Dissolved Oxygen",
  "% Oxygen Saturation", "Partial Pressure Oxygen", "Actual conductivity",
  "Specific Conductivity", "Salinity", "Resistivity", "Density",
  "Total Dissolved Solids", "External Voltage"
)
sensor_cols <- expected_names[-1]

# Fixed EST (UTC-5); unlike America/New_York, this does not switch to EDT.
time_zone <- "Etc/GMT+5"

parse_dt <- function(x) {
  x <- trimws(as.character(x))
  out <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  missing <- is.na(out)
  out[missing] <- as.POSIXct(
    x[missing], format = "%m/%d/%y %H:%M:%OS", tz = "UTC"
  )
  out
}

safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

remove_outliers <- function(df, cols = sensor_cols, iqr_mult = 2.5) {
  is_outlier <- rep(FALSE, nrow(df))
  
  for (col in cols) {
    x <- df[[col]]
    if (all(is.na(x))) next
    q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
    spread <- iqr_mult * (q[2] - q[1])
    
    if (spread > 0) {
      is_outlier <- is_outlier |
        ((x < q[1] - spread | x > q[2] + spread) %in% TRUE)
    }
  }
  
  list(
    clean = df[!is_outlier, , drop = FALSE],
    outliers = df[is_outlier, , drop = FALSE]
  )
}

data_list <- lapply(files, function(file) {
  data <- read.csv(
    file, skip = 8, check.names = FALSE, colClasses = "character",
    na.strings = c("", "NA")
  )
  if (ncol(data) != length(expected_names)) {
    stop(file, ": expected ", length(expected_names),
         " columns, found ", ncol(data), ". Check skip = 8.")
  }
  # Data2–Data5 put Temperature in column 12; Data1part2 uses column 2.
  if (identical(names(data)[12], "Temperature (C)")) {
    data <- data[c(1, 12, 2:11, 13)]
  }
  names(data) <- expected_names
  data
})

dfcombined <- bind_rows(data_list)
dfcombined$Date.Time <- parse_dt(dfcombined$Date.Time)
if (anyNA(dfcombined$Date.Time)) {
  stop("Could not parse ", sum(is.na(dfcombined$Date.Time)),
       " timestamp(s). Check the date formats and source files.")
}

for (col in sensor_cols) {
  raw <- trimws(dfcombined[[col]])
  value <- suppressWarnings(as.numeric(raw))
  invalid <- !is.na(raw) & nzchar(raw) & is.na(value)
  if (any(invalid)) {
    stop("Non-numeric value(s) in '", col, "': ",
         paste(head(unique(raw[invalid]), 5), collapse = ", "))
  }
  dfcombined[[col]] <- value
}

dfcombined <- dfcombined %>%
  distinct() %>%
  arrange(Date.Time)

# Remove outlier observations before computing hourly means.
result <- remove_outliers(dfcombined)
dfOfMisfitsToys <- result$outliers
dfcombined <- result$clean

# Bin timestamps into fixed one-hour intervals.
dfcombined$Hour <- as.POSIXct(
  floor(as.numeric(dfcombined$Date.Time) / 3600) * 3600,
  origin = "1970-01-01", tz = time_zone
)

CapstoneDataframe <- dfcombined %>%
  group_by(Hour) %>%
  summarise(
    across(all_of(sensor_cols), safe_mean),
    Observations = n(),
    .groups = "drop"
  ) %>%
  arrange(Hour)

# Use a fixed format so midnight hours include 00:00:00 in the CSV.
export_data <- CapstoneDataframe
export_data$Hour <- format(export_data$Hour, "%Y-%m-%d %H:%M:%S", tz = time_zone)
write.csv(export_data, "CapstoneHourlyData.csv", row.names = FALSE)
