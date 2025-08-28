#' Extract Date-Time from LIFT CSV Filename
#'
#' Extracts the date and time from a LIFT CSV filename formatted as:
#' `plotnumber~~yyyy_mm_dd__hh_mm_ss_id_type.csv` and returns it as a POSIXct object.
#'
#' @param filename Character. Full path or filename of the CSV file.
#' @param tz Character. Timezone for the returned POSIXct object (default: "CET").
#'
#' @return POSIXct object representing the date and time in the specified timezone.
#' @export
#'
#' @examples
#' get_file_datetime("plot123~~2025_08_22__14_30_15_01_type.csv")
#' get_file_datetime("plot123~~2025_08_22__14_30_15_01_type.csv", tz = "UTC")
get_file_datetime <- function(filename, tz = "CET") {

  # Extract base filename in case full path is given
  fname <- basename(filename)

  # Split by "__" to separate date and time parts
  parts <- stringr::str_split(fname, "__")[[1]]

  if (length(parts) < 2) {
    stop("Filename does not contain '__' to separate date and time parts.")
  }

  # Extract date (yyyy_mm_dd) and time (hh_mm_ss) from the expected positions
  date_part <- stringr::str_extract(parts[1], "\\d{4}_\\d{2}_\\d{2}")
  time_part <- stringr::str_extract(parts[2], "\\d{2}_\\d{2}_\\d{2}")

  if (is.na(date_part) || is.na(time_part)) {
    stop("Could not extract date or time from filename. Check format.")
  }

  # Convert underscores to standard separators
  dt_str <- paste0(
    stringr::str_replace_all(date_part, "_", "/"), " ",
    stringr::str_replace_all(time_part, "_", ":")
  )

  # Convert to POSIXct with user-specified timezone
  lubridate::ymd_hms(dt_str, tz = tz)
}

# End
