#'
#' Plot Dendrogram for Spectra or Spectra2D Object
#'
#' *Internal function*. This function plots the results of an HCA analysis.
#'
#' @param spectra `r .writeDoc_Spectra3()`
#'
#' @param hclst A \code{\link{hclust}} object.
#'
#' @param sub.title A character string for the subtitle.
#'
#' @param use.sym Logical; if true, the color scheme will be black and
#' lower-case letters will be used to indicate group membership.
#' Applies to \code{Spectra} objects only.
#'
#' @param leg.loc Character; if \code{"none"} no legend will be drawn.
#' Otherwise, any string acceptable to \code{\link{legend}}.
#'
#' @param \dots `r .writeDoc_GraphicsDots()`
#'
#' @return An object of class \code{\link{dendrogram}}. Side effect is a plot.
#'
#' @author `r .writeDoc_Authors("BH")`
#'
#' @export
#' @importFrom stats as.dendrogram dendrapply
#' @importFrom graphics plot
#' @keywords internal
#'
.plotHCA <- function(spectra, hclst, sub.title, use.sym, leg.loc, ...) {

  # Function to plot HCA results, basically a wrapper to existing methods
  # Part of the ChemoSpec package
  # Bryan Hanson, DePauw University, Dec 2009

  .chkArgs(mode = 0L)

  cluster <- as.dendrogram(hclst)
  if (!use.sym) cluster <- dendrapply(cluster, .colLeaf, spectra)
  cluster <- dendrapply(cluster, .shrinkLeaf, spectra)

  plot(cluster, sub = sub.title, horiz = FALSE, ...)

  if (leg.loc == "none") {
    return(cluster)
  }
  if (leg.loc != "none") .addLegend(spectra, leg.loc, use.sym, bty = "n")
  return(cluster)
}
