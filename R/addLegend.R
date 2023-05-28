#'
#' Add a Legend to a Plot
#'
#' *Internal function*.  Add a legend to a plot created by either \code{ChemoSpec} or \code{ChemoSpec2D}.
#' \code{ChemoSpec2D} does not use symbols.
#'
#' @param spectra `r .writeDoc_Spectra3()`
#'
#' @param use.sym Logical; if true, the color scheme is set to black and the
#'   points plotted with symbols.  Applies only to \code{ChemoSpec}.
#'
#' @param leg.loc `r .writeDoc_LegLoc()`
#'
#' @param \dots `r .writeDoc_GraphicsDots()`
#'
#' @return None.  Side effect is to modify an existing plot.
#'
#' @author `r .writeDoc_Authors("BH")`
#' @importFrom graphics legend
#' @export
#' @keywords internal
#'

.addLegend <- function(spectra, leg.loc, use.sym, ...) {
  .chkArgs(mode = 0L)

  if (inherits(spectra, "Spectra")) gr <- sumGroups(spectra)
  if (inherits(spectra, "Spectra2D")) {
    if (use.sym) stop("use.sym cannot be used with ChemoSpec2D")
    gr <- sumGroups(spectra)
  }

  leg.txt <- c("Key", gr$group)
  leg.col <- c("black", gr$color)
  if (use.sym) leg.col <- "black"
  leg.pch <- NA
  if (use.sym) leg.pch <- c(NA, gr$sym)
  legend(leg.loc, leg.txt, text.col = leg.col, cex = 0.75, pch = leg.pch, ...)
} # end of addLegend
