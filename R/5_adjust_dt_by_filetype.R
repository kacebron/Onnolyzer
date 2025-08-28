#' Process a LIFT CSV File and Correct Timestamps
#'
#' Automatically detects the file type (_data, _fit, _spectr) and
#' applies the appropriate date/time correction.
#'
#' @param file_path Character. Full path to the CSV file.
#' @param time_threshold Numeric. Maximum allowed difference in seconds before correction (default: 1).
#' @param time_offset difftime or numeric. Time shift to apply if correction needed (default: 0 seconds).
#'
#' @return NULL. File is overwritten if correction is applied.
#' @export
#'
#' @examples
#' \dontrun{
#' adjust_dt_by_filetype("plot123~~2025_08_22__14_30_15_01_type_data.csv")
#' }
adjust_dt_by_filetype <- function(file_path, time_threshold = 1, time_offset = 0) {
  # Determine file type
  type <- if (stringr::str_detect(file_path, "_data\\.csv$")) {
    "data"
  } else if (stringr::str_detect(file_path, "_fit\\.csv$")) {
    "fit"
  } else if (stringr::str_detect(file_path, "_spectr\\.csv$")) {
    "spectr"
  } else {
    NA
  }
  
  if (is.na(type)) return(NULL)
  
  # Extract datetime from filename
  file_dt <- get_file_datetime(file_path)
  
  # Call appropriate function
  corrected_lines <- switch(
    type,
    data   = adjust_dt_datacsv(file_path, file_dt, time_threshold, time_offset),
    fit    = adjust_dt_fitcsv(file_path, file_dt, time_threshold, time_offset),
    spectr = adjust_dt_spectra(file_path, file_dt, time_threshold, time_offset)
  )
  
  # Save if corrections were made
  if (!is.null(corrected_lines)) {
    writeLines(corrected_lines, file_path, sep = "\r\n")
    message("File overwritten with corrected timestamps: ", basename(file_path))
  } else {
    message("No correction needed: ", basename(file_path))
  }
}

# End