#' Create LIFT Experiment Settings
#'
#' This function defines the basic parameters for analyzing
#' Light-Induced Fluorescence Transients (LIFT) data.
#'
#' @param f Integer. Number of flashlets (datapoints per transient). Default = 427.
#' @param ff Integer. Spectral range (number of points collected). Default = 840.
#' @param sep Character. Separator used in raw data file. Default = ",".
#' @param liftmodel Character. Identifier of excitation protocol. Default = "L008".
#'
#' @return A list containing the experimental parameters.
#' @examples
#' settings <- lift_settings()
#' print(settings$f)
#' print(settings$liftmodel)
#'
#' @export
lift_settings <- function(f = 427, 
                          ff = 840, 
                          sep = ",", 
                          liftmodel = "L008") {
  list(
    f = f,
    ff = ff,
    sep = sep,
    liftmodel = liftmodel
  )
}

# End