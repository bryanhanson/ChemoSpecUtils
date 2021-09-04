#'
#' Label Extreme Values
#'
#' A utility function which plots the sample names next to the sample points.
#' The number of samples labeled can be specified by passing it from the
#' calling function. An internal function, not generally called by the user.
#'
#' @param data A matrix containing the x values of the points/samples in the
#' first column, and the y values in the second.
#'
#' @param names A character vector of sample names.  Length must match the
#' number of rows in \code{x}.
#'
#' @template param-tol
#'
#' @return None.  Annotates the plot with labels.
#'
#' @author Bryan A. Hanson (DePauw University), Tejasvi Gupta.
#'
#' @keywords utilities
#' @export
#' @noRd
#'
.labelExtremes <- function(data, names, tol) {
  List <- .getExtremeCoords(data, names, tol)

  x <- List$x
  y <- List$y
  l <- List$l

  for (n in c(1:length(x))) {
    text(x[n], y[n], l[n], pos = 4, offset = 0.2, cex = 0.5)
  }
  List
}
