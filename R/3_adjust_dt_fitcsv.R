#' Correct Date and Time in a LIFT _fit.csv file
#'
#' Reads a LIFT _fit.csv file and shifts all timestamps in data rows
#' if they differ from the timestamp encoded in the filename.
#'
#' @param file_path Character. Path to the _fit.csv file to process.
#' @param file_dt POSIXct. Date-time extracted from the filename (use \code{get_file_datetime()}).
#' @param time_threshold Numeric. Maximum allowed difference in seconds before correction (default: 1).
#' @param time_offset difftime or numeric. Time shift to apply if correction needed (default: 0 seconds).
#'
#' @return Character vector of corrected file lines, or NULL if no correction needed.
#' @export
#'
#' @examples
#' \dontrun{
#' file_dt <- get_file_datetime("plot123~~2025_08_22__14_30_15_01_type_fit.csv")
#' corrected_lines <- adjust_dt_fitcsv("plot123~~2025_08_22__14_30_15_01_type_fit.csv", file_dt)
#' if(!is.null(corrected_lines)) writeLines(corrected_lines, "plot123~~2025_08_22__14_30_15_01_type_fit.csv")
#' }
adjust_dt_fitcsv <- function(file_path, file_dt, time_threshold = 1, time_offset = 0) {
  lines <- readLines(file_path)
  
  if (length(lines) < 3) {
    warning("File too short to process: ", basename(file_path))
    return(NULL)
  }
  
  # Check first data row (line 3) timestamp
  parts <- stringr::str_split(lines[3], ",")[[1]] %>% stringr::str_trim()
  if (length(parts) < 3) {
    warning("First data row malformed: ", basename(file_path))
    return(NULL)
  }
  
  internal_dt <- lubridate::ymd_hms(paste(parts[2], parts[3]), tz = "CET")
  diff_sec <- as.numeric(difftime(internal_dt, file_dt, units = "secs"))
  
  if (abs(diff_sec) <= time_threshold) {
    message("No correction needed: ", basename(file_path))
    return(NULL)
  }
  
  message("Correcting fit file: ", basename(file_path))
  
  # Apply shift to all data rows
  for (i in 3:length(lines)) {
    parts <- stringr::str_split(lines[i], ",")[[1]]
    if (length(parts) < 3) next
    
    old_dt <- lubridate::ymd_hms(paste(parts[2], parts[3]), tz = "CET")
    new_dt <- old_dt + lubridate::seconds(time_offset)
    
    lines[i] <- paste(
      parts[1],                     # first column (file path)
      format(new_dt, "%Y/%m/%d"),   # DATE
      format(new_dt, "%H:%M:%S"),   # TIME
      if (length(parts) > 3) paste(parts[4:length(parts)], collapse = ","),  # rest of line
      sep = ", "
    )
  }
  
  return(lines)
}


# End