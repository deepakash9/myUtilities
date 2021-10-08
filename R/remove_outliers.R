#' Remove outliers from a vector
#'
#' This function takes a vector and removes outliers out of 1.5 IQR
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}