library(dplyr)

# Run from the repository root (for example, an RStudio project).
data_dir <- file.path(getwd(), "CapstoneDataCSV")
files <- sort(list.files(data_dir, pattern = "\\.csv$", full.names = TRUE,
                         ignore.case = TRUE))
if (!length(files)) stop("No CSV files found in: ", data_dir)

read_hydrovu <- function(file) {
  metadata <- readLines(file, n = 8, warn = FALSE)
  offset <- trimws(gsub('"', '', sub(".*Time Offset = ", "", grep("Time Offset = ", metadata, value = TRUE)), fixed = TRUE))
  if (length(offset) != 1 || !grepl("^[+-][0-9]{2}:[0-9]{2}:[0-9]{2}$", offset)) {
    stop("Invalid time offset in: ", basename(file))
  }
  parts <- as.integer(strsplit(substring(offset, 2), ":", fixed = TRUE)[[1]])
  offset_seconds <- (if (startsWith(offset, "-")) -1 else 1) *
    (parts[1] * 3600 + parts[2] * 60 + parts[3])
  
  data <- read.csv(file, skip = 9, check.names = FALSE,
                   na.strings = c("", "NA"), fileEncoding = "UTF-8")
  if (!identical(names(data)[1], "Date Time")) {
    stop("Unexpected CSV header in: ", basename(file))
  }
  if (!nrow(data)) return(NULL)
  
  dates <- as.POSIXct(data[[1]], format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  missing <- is.na(dates)
  dates[missing] <- as.POSIXct(data[[1]][missing],
                               format = "%m/%d/%y %H:%M:%OS", tz = "UTC")
  if (anyNA(dates)) stop("Unparseable date in: ", basename(file))
  
  data[-1] <- lapply(data[-1], as.numeric)
  data[[1]] <- dates - offset_seconds  # Convert the export's fixed offset to UTC.
  names(data)[1] <- "Date.Time"
  data$Location <- sub("^HydroVu_(default-[0-9]+)_.*$", "\\1", basename(file))
  relocate(data, Location, Date.Time)
}

# Duplicates are removed after import, including duplicate files and overlapping exports.
data_list <- Filter(Negate(is.null), lapply(files, read_hydrovu))
if (!length(data_list)) stop("All CSV files are empty.")
dfcombined <- distinct(bind_rows(data_list))

# Do not silently average different readings for the same location and time.
if (any(duplicated(dfcombined[c("Location", "Date.Time")]))) {
  stop("Conflicting readings at the same location and timestamp.")
}
sensor_cols <- setdiff(names(dfcombined), c("Location", "Date.Time"))

# Remove entire observations that contain a 2.5 x IQR outlier, per location.
is_outlier <- function(data) {
  flagged <- rep(FALSE, nrow(data))
  for (col in sensor_cols) {
    x <- data[[col]]
    if (all(is.na(x))) next
    q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
    spread <- 2.5 * (q[2] - q[1])
    if (spread > 0) {
      flagged <- flagged | ((x < q[1] - spread | x > q[2] + spread) %in% TRUE)
    }
  }
  flagged
}

flagged <- rep(FALSE, nrow(dfcombined))
for (rows in split(seq_len(nrow(dfcombined)), dfcombined$Location)) {
  flagged[rows] <- is_outlier(dfcombined[rows, , drop = FALSE])
}
dfOfMisfitsToys <- dfcombined[flagged, , drop = FALSE]
dfcombined <- dfcombined[!flagged, , drop = FALSE]

dfcombined$Hour <- as.POSIXct(
  floor(as.numeric(dfcombined$Date.Time) / 3600) * 3600,
  origin = "1970-01-01", tz = "UTC"
)
# One row per hour across all locations. Sensors absent from a location are NA;
# when multiple locations measure the same sensor, average their readings.
CapstoneDataframe <- dfcombined %>%
  group_by(Hour) %>%
  summarise(across(all_of(sensor_cols), ~ {
    if (all(is.na(.x))) {
      NA_real_
    } else {
      mean(.x, na.rm = TRUE)
    }
  }),
            Observations = n(), .groups = "drop") %>%
  arrange(Hour)

output <- CapstoneDataframe
output$Hour <- format(output$Hour, "%Y-%m-%d %H:%M:%S", tz = "UTC")
write.csv(output, "CapstoneHourlyData.csv", row.names = FALSE)
cat("Imported", length(data_list), "nonempty CSV files; wrote",
    nrow(output), "hourly rows.\n")
