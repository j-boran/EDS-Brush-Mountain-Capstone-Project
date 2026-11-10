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

# Eastern Time (automatically switches between EST and EDT).
time_zone <- "Etc/GMT+5"

parse_dt <- function(x) {
  x <- trimws(as.character(x))
  out <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = time_zone)
  missing <- is.na(out)
  out[missing] <- as.POSIXct(
    x[missing], format = "%m/%d/%y %H:%M:%S", tz = time_zone
  )
  out
}

safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# The files must have the same 13 columns in the same order.
data_list <- lapply(files, function(file) {
  data <- read.csv(
    file, skip = 8, check.names = FALSE, colClasses = "character",
    na.strings = c("", "NA")
  )
  if (ncol(data) != length(expected_names)) {
    stop(file, ": expected ", length(expected_names),
         " columns, found ", ncol(data), ". Check skip = 8.")
  }
  names(data) <- expected_names
  data
})

dfcombined <- bind_rows(data_list)

raw_data <- bind_rows(data_list, .id = "source")

dfcombined$Date.Time <- parse_dt(dfcombined$Date.Time)
if (anyNA(dfcombined$Date.Time)) {
  stop("Could not parse ", sum(is.na(dfcombined$Date.Time)),
       " timestamp(s). Check the date formats and source files.")
}

sensor_cols <- expected_names[-1]
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

# Round down to the start of the hour without merging repeated fall-back hours.
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

write.csv(CapstoneDataframe, "CapstoneHourlyData.csv", row.names = FALSE)

