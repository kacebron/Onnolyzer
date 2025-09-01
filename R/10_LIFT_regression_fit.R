#' Perform Log-Log Regression on LIFT Fluorescence Data
#'
#' This function calculates slopes, intercepts, and other regression parameters
#' for two groups of a LIFT transient dataset.
#'
#' @param Transient A data.table containing cleaned LIFT transient data.
#' @param f Integer, number of flashlets per transient (used for grouping).
#' @param reorder_cols Logical, whether to reorder columns (default: TRUE)
#' 
#' @return A data.table containing merged regression results for Group 1 and Group 2.
#' 
#' @export
regression_fit <- function(df, f, reorder_cols = TRUE) {
  # Remove DataPt == 0
  df <- subset(df, DataPt != 0)
  
  # Avoid negative values for log regression
  df$EM <- df$EM^2
  df$EM <- sqrt(df$EM)
  df$EM <- df$EM + 1e-42
  
  # Define Groups
  df$Group <- rep((df[df$DataPt %in% 1:f, paste(0), by = .(Target_ID, TimeSec)])$V1, each = f)
  df$Group[df$DataPt %in% c(paste(df$DataPt[303:320]))] <- 1
  df$Group[df$DataPt %in% c(paste(df$DataPt[321:360]))] <- 2
  
  # Split by Group
  Target_ID_G1 <- subset(df, Group == 1)
  Target_ID_G2 <- subset(df, Group == 2)
  
  # Regression function for each Target_ID
  calc_slopes <- function(df_group) {
    tmp_list <- plyr::dlply(df_group, .variables = c("Filename_Source", "Target_ID"))
    for (i in seq_along(tmp_list)) {
      tmp <- lm(log(tmp_list[[i]][["EM"]]) ~ log(tmp_list[[i]][["Time.us"]]), data = tmp_list[[i]])
      tmp_summary <- summary.lm(tmp)
      
      tmp_list[[i]][["Slope"]]        <- tmp_summary$coefficients[2,1]
      tmp_list[[i]][["Slope_SE"]]     <- tmp_summary$coefficients[2,2]
      tmp_list[[i]][["Intercept"]]    <- tmp_summary$coefficients[1,1]
      tmp_list[[i]][["Intercept_SE"]] <- tmp_summary$coefficients[1,2]
      tmp_list[[i]][["Adj.R.squared"]] <- tmp_summary$adj.r.squared
      tmp_list[[i]][["Initial_F"]] <- exp((log(tmp_list[[i]][["Time.us"]][[1]]) *
                                           tmp_list[[i]][["Slope"]]) + tmp_list[[i]][["Intercept"]])
      tmp_list[[i]][["Final_F"]] <- exp((log(tmp_list[[i]][["Time.us"]][[nrow(tmp_list[[i]])]]) *
                                         tmp_list[[i]][["Slope"]]) + tmp_list[[i]][["Intercept"]])
      tmp_list[[i]][["Time.df"]] <- tmp_list[[i]][["Time.us"]][nrow(tmp_list[[i]])] -
                                    tmp_list[[i]][["Time.us"]][1]
    }
    dt <- plyr::ldply(tmp_list)
    dt$.id <- NULL
    dt <- data.table::as.data.table(dt)
    dt <- dt[with(dt, order(TimeSec, DataPt)), ]
    return(dt)
  }
  
  # Calculate slopes for Group 1 & 2
  Target_ID_G1 <- calc_slopes(Target_ID_G1)
  Target_ID_G2 <- calc_slopes(Target_ID_G2)
  
  # Merge back
  Target_ID_Data <- rbind(Target_ID_G1, Target_ID_G2)
  
  Target_ID_Slps <- subset(Target_ID_Data, DataPt == 303 | DataPt == 321)

  # Optional: reorder columns
  if (reorder_cols) {
    col_order <- c("Data_Source", "Filename_Source", "Date", "Time", "TimeSec", "Target_ID",
                   "DataPt", "Time.us", "Group", "Intensity", "Gain", "EX", "EM", "IRRAD", "S.N_RAT",
                   "Fo", "Fm", "Fq.Fm", "Fr1.Fq", "Fr2.Fq", "Fr.Fq", "Fr1.Fm", "Fr2.Fm", "Fr.Fm",
                   "Slope", "Slope_SE", "Intercept", "Intercept_SE", "Adj.R.squared",
                   "Initial_F", "Final_F", "Time.df")
    cols_present <- col_order[col_order %in% names(Target_ID_Slps)]
    data.table::setcolorder(Target_ID_Slps, cols_present)
  }

  Target_ID_Slopes_G1 <- subset(Target_ID_Slps, Group == 1)
  names(Target_ID_Slopes_G1) <- c("Data_Source", "Filename_Source", "Date", "Time", "TimeSec",
                                  "Target_ID", "DataPt_1", "Time.us_1", "Group_1", "Intensity",
                                  "Gain", "EX_1", "EM_1", "IRRAD", "S.N_RAT", "Fo", "Fm", "Fq.Fm",
                                  "Fr1.Fq", "Fr2.Fq", "Fr.Fq", "Fr1.Fm", "Fr2.Fm", "Fr.Fm", "Slope_1",
                                  "Slope_SE_1", "Intercept_1", "Intercept_SE_1", "Adj.R.squared_1",
                                  "Initial_F_1", "Final_F_1", "Time.df_1")

  Target_ID_Slopes_G2 <- subset(Target_ID_Slps, Group == "2")
  names(Target_ID_Slopes_G2) <- c("Data_Source", "Filename_Source", "Date", "Time", "TimeSec",
                                  "Target_ID", "DataPt_2", "Time.us_2", "Group_2", "Intensity",
                                  "Gain","EX_2", "EM_2", "IRRAD", "S.N_RAT", "Fo", "Fm", "Fq.Fm",
                                  "Fr1.Fq", "Fr2.Fq", "Fr.Fq", "Fr1.Fm", "Fr2.Fm", "Fr.Fm", "Slope_2",
                                  "Slope_SE_2", "Intercept_2", "Intercept_SE_2", "Adj.R.squared_2",
                                  "Initial_F_2", "Final_F_2", "Time.df_2")

  Final_Data <- merge(
    Target_ID_Slopes_G1, Target_ID_Slopes_G2,
    by = c("TimeSec", "Target_ID", "Data_Source", "Filename_Source", "Date", "Time",
           "Intensity", "Gain", "IRRAD", "S.N_RAT", "Fo", "Fm", "Fq.Fm", "Fr1.Fq",
           "Fr2.Fq", "Fr.Fq", "Fr1.Fm", "Fr2.Fm", "Fr.Fm"),
    all = TRUE
  )
  
  regression_fit <- Final_Data[!duplicated(Final_Data$TimeSec), ]

  return(regression_fit)
}

# End