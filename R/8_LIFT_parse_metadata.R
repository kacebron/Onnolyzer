#' Parse and Clean LIFT Metadata from Raw Data
#'
#' This function extracts experiment metadata, renames columns,
#' removes the first (incomplete/noisy) measurement, coerces columns
#' to appropriate types, and returns a tidy data.table of transients.
#'
#' @param df Data frame returned by \code{lift_load_raw()}.
#' @param f Integer. Number of flashlets (datapoints per transient).
#'
#' @return A \code{data.table} with cleaned and typed transient data:
#'         - Renamed columns: DataPt, Time.us, EX, EM
#'         - Metadata columns: Date, Time, TimeSec, IRRAD, Intensity, Gain, S.N_RAT
#'         - Drops first transient (sets TimeSec to NA, then removes with na.omit)
#'         - Proper numeric, character, and date formats
#'
#' @examples
#' \dontrun{ <- system.file("extdata", package = "Onnolyzer")
#' fnames <- list.files(fpath, full.names = TRUE)
#' raw <- lift_load_raw(fpath, fnames, f = 427)
#' clean <- lift_parse_metadata(raw, f = 427)
#'}
#' @export
lift_parse_metadata <- function(df, f) {
  # --- Extract metadata ---
  df$Date <- rep(na.omit(df$V2[grepl("^DateTime:", df$V1)]), each = f + 19)
  df$Time <- rep(na.omit(df$V3[grepl("^DateTime:", df$V1)]), each = f + 19)
  df$TimeSec <- rep(na.omit(df$V4[grepl("^DateTime:", df$V1)]), each = f + 19)

  # Handle software version differences: "PIF:" vs "Irradiance:"
  df$IRRAD <- rep(na.omit(df$V2[df$V1 %in% c("PIF:", "Irradiance:")]), each = f + 19)

  df$Intensity <- rep(na.omit(df$V2[df$V1 == "Lamps:"]), each = f + 19)
  df$Gain <- rep(na.omit(df$V2[df$V1 == "Gain:"]), each = f + 19)
  df$S.N_RAT <- rep(na.omit(df$V2[df$V1 == "SNR_raw:"]), each = f + 19)

  # --- Sort by TimeSec (if present) ---
  if ("TimeSec" %in% names(df)) {
    df <- df[with(df, order(TimeSec)), ]
  }

  # --- Drop V5 if it exists ---
  if ("V5" %in% names(df)) {
    df$V5 <- NULL
  }

  # --- Rename first 4 columns ---
  names(df)[1:4] <- c("DataPt", "Time.us", "EX", "EM")

  # --- Remove first measurement (set TimeSec NA for first 18 datapoints) ---
  df$TimeSec[df$DataPt %in% c(df$DataPt[1:18])] <- NA

  # --- Convert to data.table and drop NA rows ---
  df <- data.table::data.table(df)
  df <- na.omit(df)

  # --- Type coercion ---
  df$DataPt     <- as.numeric(df$DataPt)
  df$Time.us    <- as.numeric(df$Time.us)
  df$EX         <- as.numeric(df$EX)
  df$EM         <- as.numeric(df$EM)
  df$Date       <- lubridate::as_date(df$Date)
  df$TimeSec    <- as.numeric(df$TimeSec)
  df$IRRAD      <- as.numeric(df$IRRAD)
  df$Intensity  <- stringr::str_trim(df$Intensity)
  df$Gain       <- as.numeric(df$Gain)
  df$S.N_RAT    <- as.numeric(df$S.N_RAT)

  return(df)
}

# End
