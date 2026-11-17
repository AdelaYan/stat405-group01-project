
# STAT 405 - Citi Bike Data Preparation
# Dataset: January 2024 - December 2025

library(data.table)

# -----------------------------------------
# 1. Configuration
# -----------------------------------------

RAW_DIR <- "data/raw"
OUTPUT_DIR <- "data/processed"
REPORT_DIR <- "reports"

CHUNK_SIZE <- 100000

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(REPORT_DIR, recursive = TRUE, showWarnings = FALSE)

required_cols <- c(
  "ride_id", "rideable_type", "started_at", "ended_at",
  "start_lat", "start_lng", "end_lat", "end_lng",
  "member_casual"
)

# -----------------------------------------
# 2.Distance(Haversine Distance)
# -----------------------------------------

haversine <- function(lat1, lng1, lat2, lng2) {

  R <- 6371.0088

  lat1 <- lat1 * pi / 180
  lat2 <- lat2 * pi / 180

  dlat <- lat2 - lat1
  dlng <- (lng2 - lng1) * pi / 180

  a <- sin(dlat / 2)^2 +
    cos(lat1) * cos(lat2) * sin(dlng / 2)^2

  a <- pmin(1, pmax(0, a))

  2 * R * asin(sqrt(a))
}

# -----------------------------------------
# 3. Clean One Chunk
# -----------------------------------------

clean_chunk <- function(df, target_month) {

  setDT(df)

  quality <- list()
  quality$raw <- nrow(df)

  # Validate ride ID and membership
  df[, ride_id := trimws(as.character(ride_id))]
  df[, member_casual := tolower(
    trimws(as.character(member_casual))
  )]

  before <- nrow(df)

  df <- df[
    !is.na(ride_id) &
    ride_id != "" &
    member_casual %in% c("member", "casual")
  ]

  quality$invalid_id_or_member <- before - nrow(df)

  # Parse timestamps with optional fractional seconds
  df[, started_at := as.POSIXct(
    started_at,
    format = "%Y-%m-%d %H:%M:%OS",
    tz = "UTC"
  )]

  df[, ended_at := as.POSIXct(
    ended_at,
    format = "%Y-%m-%d %H:%M:%OS",
    tz = "UTC"
  )]

  # Calculate ride duration
  df[, ride_duration_min := as.numeric(
    difftime(ended_at, started_at, units = "mins")
  )]

  before <- nrow(df)

  df <- df[
    !is.na(started_at) &
    !is.na(ended_at) &
    !is.na(ride_duration_min) &
    ride_duration_min >= 1 &
    ride_duration_min <= 1440
  ]

  quality$invalid_time <- before - nrow(df)

  # Assign rides to the target month
  before <- nrow(df)

  df <- df[
    format(started_at, "%Y%m", tz = "UTC") == target_month
  ]

  quality$outside_month <- before - nrow(df)

  # Convert coordinate columns to numeric
  coord_cols <- c(
    "start_lat", "start_lng", "end_lat", "end_lng"
  )

  for (col in coord_cols) {
    set(df, j = col, value = suppressWarnings(
      as.numeric(df[[col]])
    ))
  }

  # Remove missing or invalid geographic coordinates
  before <- nrow(df)

  df <- df[
    !is.na(start_lat) & !is.na(end_lat) &
    !is.na(start_lng) & !is.na(end_lng) &
    start_lat >= 40.4 & start_lat <= 41.1 &
    end_lat >= 40.4 & end_lat <= 41.1 &
    start_lng >= -74.5 & start_lng <= -73.5 &
    end_lng >= -74.5 & end_lng <= -73.5
  ]

  quality$invalid_coordinates <- before - nrow(df)

  # Calculate geographic distance
  df[, distance_km := haversine(
    start_lat, start_lng, end_lat, end_lng
  )]

  # Create year and month variables
  df[, year := as.integer(
    format(started_at, "%Y", tz = "UTC")
  )]

  df[, month := as.integer(
    format(started_at, "%m", tz = "UTC")
  )]

  # Create season variable
  df[, season := fifelse(
    month %in% c(12, 1, 2), "Winter",
    fifelse(
      month %in% c(3, 4, 5), "Spring",
      fifelse(
        month %in% c(6, 7, 8), "Summer",
        "Fall"
      )
    )
  )]

  
  # Create weekday / weekend variables

  # Get weekday number: Sunday = 0, Saturday = 6
  df[, weekday_num := as.POSIXlt(
    started_at,
    tz = "UTC"
  )$wday]

  # Convert weekday number to names
  weekday_names <- c(
    "Sunday", "Monday", "Tuesday", "Wednesday",
    "Thursday", "Friday", "Saturday" )

  df[, day_of_week := weekday_names[weekday_num + 1L]]

  # Classify weekdays and weekends
  df[, day_type := fifelse(
    weekday_num %in% c(0L, 6L),
    "Weekend",
    "Weekday"
  )]

  # Remove temporary weekday number
  df[, weekday_num := NULL]

  # Keep variables required for later analysis
  output_cols <- c(
    "ride_id", "rideable_type", "member_casual",
    "started_at", "ended_at", "ride_duration_min",
    "distance_km", "year", "month", "season",
    "day_of_week", "day_type",
    "start_lat", "start_lng", "end_lat", "end_lng"
  )

  df <- df[, ..output_cols]

  quality$kept <- nrow(df)

  list(data = df, quality = quality)
}

# -----------------------------------------
# 4. Find Monthly ZIP
# -----------------------------------------

find_zip <- function(month) {

  candidates <- file.path(
    RAW_DIR,
    paste0(
      month,
      c(
        "-citibike-tripdata.zip",
        "-citibike-tripdata.csv.zip"
      )
    )
  )

  found <- candidates[file.exists(candidates)]

  if (length(found) == 0) {
    stop(paste("ZIP not found:", month))
  }

  found[1]
}

# -----------------------------------------
# 5. Process One Month
# -----------------------------------------

process_month <- function(month) {

  cat("\nProcessing:", month, "\n")

  zip_path <- find_zip(month)

  files <- unzip(zip_path, list = TRUE)$Name
  csv_files <- files[
    grepl("\\.csv$", files, ignore.case = TRUE) &
    !grepl("^__MACOSX/", files)
  ]

  if (length(csv_files) == 0) {
    stop("No CSV files found in ZIP.")
  }

  month_dir <- file.path(OUTPUT_DIR, paste0("citibike_", month))

  # Recreate the month output to avoid stale parts
  if (dir.exists(month_dir)) {
    unlink(month_dir, recursive = TRUE)
  }

  dir.create(month_dir, recursive = TRUE)

  totals <- c(
    raw = 0,
    invalid_id_or_member = 0,
    invalid_time = 0,
    outside_month = 0,
    invalid_coordinates = 0,
    kept = 0
  )

  summary_parts <- list()
  part_number <- 0L

  for (csv_name in csv_files) {

    cat("Reading:", csv_name, "\n")

    # Create a temporary directory
    temp_dir <- tempfile(pattern = "citibike_")
    dir.create(temp_dir, recursive = TRUE)

    # Extract the current CSV from the ZIP
    extracted_files <- unzip(
      zip_path,
      files = csv_name,
      exdir = temp_dir
    )

    # Get the actual extracted file path
    if (length(extracted_files) != 1 || !file.exists(extracted_files[1])) {
      stop(paste("Extraction failed:", csv_name))
    }

    extracted_file <- extracted_files[1]

    # Open the extracted CSV
    con <- file(extracted_file, open = "rt")
   
    tryCatch({

      # Read the header
      header <- strsplit(
        readLines(con, n = 1L, warn = FALSE),
        ",",
        fixed = TRUE
      )[[1]]

      header <- sub("^\ufeff", "", header)

      if (!all(required_cols %in% header)) {
        stop("Missing required columns in: ", csv_name)
      }

      repeat {

        chunk <- read.csv(
          con,
          header = FALSE,
          col.names = header,
          nrows = CHUNK_SIZE,
          colClasses = "character",
          check.names = FALSE,
          stringsAsFactors = FALSE
        )

        if (nrow(chunk) == 0) break

        # Keep necessary columns
        chunk <- as.data.table(chunk)[, ..required_cols]

        result <- clean_chunk(chunk, month)

        cleaned <- result$data
        q <- unlist(result$quality)

        totals[names(q)] <- totals[names(q)] + q

        if (nrow(cleaned) == 0) next

        part_number <- part_number + 1L

        output_file <- file.path(
          month_dir,
          sprintf("part-%05d.gz", part_number)
        )

          data.table::fwrite(cleaned, output_file)

        summary_parts[[length(summary_parts) + 1L]] <-
          cleaned[, .(
            n = .N,
            sum_duration_min = sum(ride_duration_min),
            sum_distance_km = sum(distance_km)
          ), by = .(
            member_casual, season, day_type
          )]

      }

    }, finally = {
      close(con)

      unlink(temp_dir, recursive = TRUE)
    })
  }

  if (totals["kept"] == 0) {
    stop("No valid rides found for ", month)
  }

  # Combine chunk-level summaries
  summary <- rbindlist(summary_parts)

  summary <- summary[, .(
    n = sum(n),
    sum_duration_min = sum(sum_duration_min),
    sum_distance_km = sum(sum_distance_km)
  ), by = .(
    member_casual, season, day_type
  )]

  summary[, mean_duration_min :=
    sum_duration_min / n
  ]

  summary[, mean_distance_km :=
    sum_distance_km / n
  ]

  summary[, year_month := month]

  setcolorder(summary, c(
    "year_month", "member_casual", "season",
    "day_type", "n", "sum_duration_min",
    "sum_distance_km", "mean_duration_min",
    "mean_distance_km"
  ))

  fwrite(
    summary,
    file.path(REPORT_DIR, paste0("summary_", month, ".csv"))
  )

  quality <- as.data.table(as.list(totals))
  quality[, year_month := month]
  setcolorder(quality, "year_month")

  fwrite(
    quality,
    file.path(REPORT_DIR, paste0("quality_", month, ".csv"))
  )

  cat(
    "Raw:", totals["raw"],
    "| Kept:", totals["kept"], "\n"
  )

  invisible(quality)
}

# -----------------------------------------
# 6. Main
# -----------------------------------------

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
  stop("Usage: Rscript scripts/prepare.R 202401 OR all")
}

if (args[1] == "all") {

  months <- sprintf(
    "%04d%02d",
    rep(2024:2025, each = 12),
    rep(1:12, times = 2)
  )

} else {

  months <- args[1]

  valid_months <- sprintf(
    "%04d%02d",
    rep(2024:2025, each = 12),
    rep(1:12, times = 2)
  )

  if (!months %in% valid_months) {
    stop("Month must be between 202401 and 202512")
  }
}

for (month in months) {
  process_month(month)
}

# Combine available monthly reports
quality_files <- list.files(
  REPORT_DIR,
  pattern = "^quality_[0-9]{6}\\.csv$",
  full.names = TRUE
)

summary_files <- list.files(
  REPORT_DIR,
  pattern = "^summary_[0-9]{6}\\.csv$",
  full.names = TRUE
)

if (length(quality_files) > 0) {
  all_quality <- rbindlist(
    lapply(quality_files, fread),
    fill = TRUE
  )

  fwrite(
    all_quality,
    file.path(REPORT_DIR, "quality_all_months.csv")
  )
}

if (length(summary_files) > 0) {
  all_summary <- rbindlist(
    lapply(summary_files, fread),
    fill = TRUE
  )

  fwrite(
    all_summary,
    file.path(REPORT_DIR, "monthly_group_summary.csv")
  )
}

cat("\nData preparation completed!\n")

