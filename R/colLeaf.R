#' Color the Leaves of a Dendrogram Based on a Spectra Object
#'
#' *Internal function.* This function colors the leaves of a dendrogram object.  The code was taken
#' from the help files.
#'
#' @param n A node in a dendrogram object.
#'
#' @param spectra `r .writeDoc_Spectra1()`
#'
#' @return Returns a node with the label color properties set.
#'
#' @author `r .writeDoc_Authors("BH")`
#'
#' @keywords internal
#'
#' @export
#' @importFrom stats is.leaf
#'
.colLeaf <- function(n, spectra) { # this is called iteratively by dendrapply

  # A little trick to color leaves properly, derived from the archives
  # Part of the ChemoSpec package
  # Bryan Hanson, DePauw University, June 2008

  if (is.leaf(n)) {
    a <- attributes(n)
    i <- match(a$label, spectra$names)

    attr(n, "nodePar") <- c(a$nodePar, list(
      lab.col = spectra$colors[i],
      pch = NA
    ))
  }
  n
}
