#' Reduction and Reoxidation Fit Calculation
#'
#' Calculates fluorescence reduction parameters (Fo, Fm, Fq/Fm, Relative)
#' and re-oxidation/relaxation parameters (Fr1, Fr2, Fr) from LIFT data.
#'
#' @param df data.table or data.frame from lift_parse_metadata
#' @param f integer, number of excitation flashlets (e.g. 427)
#' @param method character vector, one or both of "relative"/"Keller" and "absolute"/"Zendonadi"
#' @param regression logical; if TRUE, run regression_fit() and return its result
#' @return data.table with added columns (or the regression result if regression = TRUE)
#' @export
redox_fit <- function(df, f, method = c("relative", "absolute"), regression = TRUE) {
  # Ensure df is a data.table
  df <- data.table::as.data.table(df)

  # Defensive check
  required <- c("DataPt", "Time.us", "EM", "Target_ID", "TimeSec")
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop("`df` is missing required column(s): ", paste(missing, collapse = ", "))
  }

  # --- Reduction step ---
  df$Fo <- rep(
    df[DataPt %in% 1, mean(get("EM")), by = .(Target_ID, TimeSec)]$V1,
    each = f + 1
  )
  df$Fm <- rep(
    df[DataPt %in% 298:302, max(get("EM")), by = .(Target_ID, TimeSec)]$V1,
    each = f + 1
  )
  df$Fq.Fm   <- (df$Fm - df$Fo) / df$Fm
  df$Relative <- (df$EM - df$Fo) / (df$Fm - df$Fo)

  # Normalize method names
  method <- tolower(method)
  method[method %in% "keller"]    <- "relative"
  method[method %in% "zendonadi"] <- "absolute"

  for (m in method) {
    if (m == "relative") {
      # Keller method
      df$Fr1.Area <- rep(
        df[DataPt %in% 303:320,
           agricolae::audpc(get("Relative"), get("Time.us"), type = "absolute"),
           by = .(Target_ID, TimeSec)]$V1,
        each = f + 1
      )
      df$Fr1.Time <-
        rep(df[DataPt %in% 320, mean(get("Time.us")), by = .(Target_ID, TimeSec)]$V1, each = f + 1) -
        rep(df[DataPt %in% 303, mean(get("Time.us")), by = .(Target_ID, TimeSec)]$V1, each = f + 1)
      df$Fr1.Fq <- 1 - (df$Fr1.Area / df$Fr1.Time)

      df$Fr2.Area <- rep(
        df[DataPt %in% 303:360,
           agricolae::audpc(get("Relative"), get("Time.us"), type = "absolute"),
           by = .(Target_ID, TimeSec)]$V1,
        each = f + 1
      )
      df$Fr2.Time <-
        rep(df[DataPt %in% 360, mean(get("Time.us")), by = .(Target_ID, TimeSec)]$V1, each = f + 1) -
        rep(df[DataPt %in% 303, mean(get("Time.us")), by = .(Target_ID, TimeSec)]$V1, each = f + 1)
      df$Fr2.Fq <- 1 - (df$Fr2.Area / df$Fr2.Time)

      df$Fr.Area <- rep(
        df[DataPt %in% 303:f,
           agricolae::audpc(get("Relative"), get("Time.us"), type = "absolute"),
           by = .(Target_ID, TimeSec)]$V1,
        each = f + 1
      )
      df$Fr.Time <-
        rep(df[DataPt %in% f, mean(get("Time.us")), by = .(Target_ID, TimeSec)]$V1, each = f + 1) -
        rep(df[DataPt %in% 303, mean(get("Time.us")), by = .(Target_ID, TimeSec)]$V1, each = f + 1)
      df$Fr.Fq <- 1 - (df$Fr.Area / df$Fr.Time)

    } else if (m == "absolute") {
      # Zendonadi method
      df$Fr1.Area <- rep(
        df[DataPt %in% 303:320,
           agricolae::audpc(get("EM"), get("Time.us"), type = "absolute"),
           by = .(Target_ID, TimeSec)]$V1,
        each = f + 1
      )
      df$Fr1.Time <-
        rep(df[DataPt %in% 320, mean(get("Time.us")), by = .(Target_ID, TimeSec)]$V1, each = f + 1) -
        rep(df[DataPt %in% 303, mean(get("Time.us")), by = .(Target_ID, TimeSec)]$V1, each = f + 1)
      df$Fr1.Fm <- 1 - (df$Fr1.Area / (df$Fm * df$Fr1.Time))

      df$Fr2.Area <- rep(
        df[DataPt %in% 303:360,
           agricolae::audpc(get("EM"), get("Time.us"), type = "absolute"),
           by = .(Target_ID, TimeSec)]$V1,
        each = f + 1
      )
      df$Fr2.Time <-
        rep(df[DataPt %in% 360, mean(get("Time.us")), by = .(Target_ID, TimeSec)]$V1, each = f + 1) -
        rep(df[DataPt %in% 303, mean(get("Time.us")), by = .(Target_ID, TimeSec)]$V1, each = f + 1)
      df$Fr2.Fm <- 1 - (df$Fr2.Area / (df$Fm * df$Fr2.Time))

      df$Fr.Area <- rep(
        df[DataPt %in% 303:f,
           agricolae::audpc(get("EM"), get("Time.us"), type = "absolute"),
           by = .(Target_ID, TimeSec)]$V1,
        each = f + 1
      )
      df$Fr.Time <-
        rep(df[DataPt %in% f, mean(get("Time.us")), by = .(Target_ID, TimeSec)]$V1, each = f + 1) -
        rep(df[DataPt %in% 303, mean(get("Time.us")), by = .(Target_ID, TimeSec)]$V1, each = f + 1)
      df$Fr.Fm <- 1 - (df$Fr.Area / (df$Fm * df$Fr.Time))
    }
  }

  # Cleanup intermediate columns
  df[, c("Fr1.Area","Fr1.Time","Fr2.Area","Fr2.Time","Fr.Area","Fr.Time","Relative") := NULL]

  if (isTRUE(regression)) {
    return(regression_fit(df, f, reorder_cols = TRUE))
  } else {
    redox_fit <- subset(df, DataPt == 303 | DataPt == 321)
    redox_fit <- redox_fit[!duplicated(redox_fit$TimeSec), ]
    return(redox_fit)
  }
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("DataPt", "Target_ID", "TimeSec", "Group", "."))
}

# End
