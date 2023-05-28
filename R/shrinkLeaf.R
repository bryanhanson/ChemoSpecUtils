#'
#'
#' Shrink the Leaves of a Dendrogram Based on a Spectra Object
#'
#' *Internal function*. This function shrinks the size of leaves of a dendrogram object.  The code
#' was taken from the help files.
#'
#' @param n A node in a dendrogram object.
#'
#' @param spectra `r .writeDoc_Spectra1()`
#'
#' @return Returns a node with the label size properties set.
#'
#' @author `r .writeDoc_Authors("BH")`
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec}
#'
#' @keywords internal
#' @export
#' @importFrom stats is.leaf
#'
.shrinkLeaf <- function(n, spectra) { # this is called iteratively by dendrapply

  # A little trick to color leaves properly, derived from the archives
  # Part of the ChemoSpec package
  # Bryan Hanson, DePauw University, June 2008

  lab.size <- 1.0
  if (length(spectra$names) > 20) lab.size <- 0.75
  if (length(spectra$names) > 50) lab.size <- 0.50
  if (length(spectra$names) > 75) lab.size <- 0.25
  if (length(spectra$names) > 100) lab.size <- 0.20

  if (is.leaf(n)) {
    a <- attributes(n)
    i <- match(a$label, spectra$names)

    attr(n, "nodePar") <- c(a$nodePar, list(
      lab.col = "black",
      pch = NA, lab.cex = lab.size
    ))
  }
  n
}
