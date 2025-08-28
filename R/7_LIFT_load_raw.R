#' Load Raw LIFT Transient Data
#'
#' This function reads multiple LIFT raw data files from a given directory,
#' merges them into a single data frame, and adds metadata. It also checks
#' that the number of rows is consistent with the number of flashlets + 19.
#'
#' @param fpath Character. Path to the directory containing raw LIFT files.
#' @param fnames Character vector. List of filenames to be read (relative to fpath).
#' @param f Integer. Number of flashlets (datapoints per transient).
#' @param sep Character. Separator used in the raw data files. Default = ",".
#'
#' @return A data frame containing all raw transient data plus metadata:
#'         - Data_Source (directory path)
#'         - Filename_Source (individual file name)
#'         - Target_ID (index of measurement/target within each file)
#'
#' #' @examples
#' fpath <- system.file("extdata", package = "Onnolyzer")
#' fnames <- list.files(fpath, full.names = TRUE)
#' raw <- lift_load_raw(fpath, fnames, f = 427)
#'
#' @export
lift_load_raw <- function(fpath, fnames, f, sep = ",") {
  plyr::ldply(fnames, function(filename) {
    dum <- utils::read.csv(
      file = file.path(fpath, filename),
      header = FALSE,
      sep = sep,
      stringsAsFactors = FALSE,
      na.strings = c("----", "==========", "=========", "", "--")
    )

    # --- Consistency check ---
    n_transients <- nrow(dum) / (f + 19)
    if (abs(n_transients - round(n_transients)) > .Machine$double.eps^0.5) {
      stop(paste0(
        "File ", filename,
        " has ", nrow(dum), " rows. ",
        "This is not divisible by (f+19 = ", f + 19, "). ",
        "Check if 'f' is set correctly or if the file is corrupted."
      ))
    }

    dum$Data_Source <- fpath
    dum$Filename_Source <- filename
    dum$Target_ID <- rep(1:round(n_transients), each = f + 19)

    return(dum)
  })
}

# End
