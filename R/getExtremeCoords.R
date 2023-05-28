#'
#' getExtremeCoords
#'
#' *Internal function*. A utility function which returns a list of the extreme
#' coordinates that are to be labelled.
#'
#' @param data A matrix containing the x values of the points/samples in the
#' first column, and the y values in the second.
#'
#' @param names A character vector of sample names.  Length must match the
#' number of rows in \code{data}.
#'
#' @param tol `.writeDoc_Tol()`
#'
#' @return  A list with elements \code{x}, \code{y} and \code{l}, giving the coordinates,
#'          and labels of the most extreme data points.
#'
#' @author `r .writeDoc_Authors(c("BH", "TG"))`
#'
#' @keywords internal
#' @export
#' @importFrom stats quantile
#' @importFrom graphics text
#'
.getExtremeCoords <- function(data, names, tol) {
  px <- data[, 1]
  py <- data[, 2]
  pl <- names
  if (is.numeric(pl)) pl <- sprintf("%.2f", pl)

  q.x <- quantile(px, probs = c(1.0 - tol, tol), na.rm = TRUE)
  sel.x <- (px <= q.x[2]) | (px >= q.x[1])
  keep.x <- subset(px, sel.x)
  keep.x <- match(keep.x, px) # need to keep this & corresponding y

  q.y <- quantile(py, probs = c(1.0 - tol, tol), na.rm = TRUE)
  sel.y <- (py <= q.y[2]) | (py >= q.y[1])
  keep.y <- subset(py, sel.y)
  keep.y <- match(keep.y, py) # need to keep this & corresponding x

  keep <- unique(c(keep.x, keep.y))

  x <- px[keep]
  y <- py[keep]
  l <- pl[keep]

  List <- list("x" = x, "y" = y, "l" = l)
  return(List)
}
