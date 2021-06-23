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
#' @param tol A number describing the fraction of points to be labeled.
#' \code{tol = 1.0} labels all the points; \code{tol = 0.05} labels
#' \emph{approximately} the most extreme 5 percent.  Note that this is simply
#' based upon quantiles, assumes that both x and y are each normally
#' distributed, and treats x and y separately.  Thus, this is not a formal
#' treatment of outliers, just a means of labeling points.  Note too that while
#' this function could deal with groups separately, the way it is called by
#' \code{\link{.plotScoresDecoration}} lumps all groups together.
#'
#' @return None.  Annotates the plot with labels.
#'
#' @author Bryan A. Hanson, DePauw University,Tejasvi Gupta.
#'
#' @keywords utilities
#' @export
#' @noRd
#'
.labelExtremes <- function(data, names, tol) {
  newList <- .getExtremeCoords(data, names, tol)

  x <- newList$x
  y <- newList$y
  l <- newList$l

  for (n in c(1:length(x))) {
    text(x[n], y[n], l[n], pos = 4, offset = 0.2, cex = 0.5)
  }
}
