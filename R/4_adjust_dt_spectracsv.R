#' Correct Date and Time in a LIFT _spectr.csv file
#'
#' Reads a LIFT _spectr.csv file and shifts all "DateTime:" lines
#' if they differ from the timestamp encoded in the filename.
#'
#' @param file_path Character. Path to the _spectr.csv file to process.
#' @param file_dt POSIXct. Date-time extracted from the filename (use \code{get_file_datetime()}).
#' @param time_threshold Numeric. Maximum allowed difference in seconds before correction (default: 1).
#' @param time_offset difftime or numeric. Time shift to apply if correction needed (default: 0 seconds).
#'
#' @return Character vector of corrected file lines, or NULL if no correction needed.
#' @export
#'
#' @examples
#' \dontrun{
#' file_dt <- get_file_datetime("plot123~~2025_08_22__14_30_15_01_type_spectr.csv")
#' corrected_lines <- adjust_dt_spectra("plot123~~2025_08_22__14_30_15_01_type_spectr.csv", file_dt)
#' if(!is.null(corrected_lines)) writeLines(corrected_lines, "plot123~~2025_08_22__14_30_15_01_type_spectr.csv")
#' }
adjust_dt_spectra <- function(file_path, file_dt, time_threshold = 1, time_offset = 0) {
  lines <- readLines(file_path)
  
  dt_idx <- which(stringr::str_detect(lines, "DateTime:"))
  if (length(dt_idx) == 0) {
    message("No 'DateTime:' lines found: ", basename(file_path))
    return(NULL)
  }
  
  parts_first <- stringr::str_split(lines[dt_idx[1]], ",")[[1]] %>% stringr::str_trim()
  if (length(parts_first) < 4) {
    warning("Malformed first DateTime line: ", basename(file_path))
    return(NULL)
  }
  
  internal_dt <- lubridate::ymd_hms(paste(parts_first[2], parts_first[3]), tz = "CET")
  diff_sec <- as.numeric(difftime(internal_dt, file_dt, units = "secs"))
  
  if (abs(diff_sec) <= time_threshold) {
    message("No correction needed: ", basename(file_path))
    return(NULL)
  }
  
  message("Correcting file: ", basename(file_path))
  
  for (pos in dt_idx) {
    parts <- stringr::str_split(lines[pos], ",")[[1]] %>% stringr::str_trim()
    if (length(parts) < 4) next
    
    old_dt <- lubridate::ymd_hms(paste(parts[2], parts[3]), tz = "CET")
    new_dt <- old_dt + lubridate::seconds(time_offset)
    
    lines[pos] <- paste0(
      "DateTime:        , ",
      format(new_dt, "%Y/%m/%d"), ", ",
      format(new_dt, "%H:%M:%S"), ", ",
      parts[4], ","
    )
  }
  
  return(lines)
}

# End