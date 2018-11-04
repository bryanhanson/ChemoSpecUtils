
#' Add a Legend to a Plot
#'
#' Add a legend to a plot created by either \code{ChemoSpec} or \code{ChemoSpec2D}.
#' \code{ChemoSpec2D} does not use symbols.
#'
#' @param spectra An object of S3 class \code{\link[ChemoSpec]{Spectra}} or 
#' \code{Spectra2D}.
#'
#' @param use.sym Logical; if true, the color scheme is set to black and the
#'   points plotted with symbols.  Applies only to \code{ChemoSpec}.
#'
#' @param leg.loc Character; Any string acceptable to \code{\link{legend}}.
#'
#' @param \dots Additional parameters to be passed to the plotting functions.
#'
#' @return None.  Side effect is to modify an existing plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @importFrom graphics legend
#'
#' @export
#' @noRd
#'
#' @examples
#' if (requireNamespace("ChemoSpec", quietly = TRUE)) {
#'   library("ChemoSpec")
#'   data(SrE.IR)
#'   plotSpectra(SrE.IR, main = "SrE.IR Data",
#'	   which = c(1, 5, 16), offset = 0.1, yrange = c(0, 0.5))
#'   .addLegend(SrE.IR, use.sym = FALSE, leg.loc = list(x = 3500, y = 0.5), bty = "n") 
#' }
#'

.addLegend <- function(spectra, leg.loc, use.sym, ...) {
   
  .chkArgs(mode = 0L)
  
  if (class(spectra) == "Spectra") gr <- sumGroups(spectra)
  if (class(spectra) == "Spectra2D") {
  	if (use.sym) stop("use.sym cannot be used with ChemoSpec2D")
  	gr <- sumGroups(spectra)
  }
  
  leg.txt <- c("Key", gr$group)
  leg.col <- c("black", gr$color)
  if (use.sym) leg.col = "black"
  leg.pch <- NA
  if (use.sym) leg.pch <- c(NA, gr$sym)
  legend(leg.loc, leg.txt, text.col = leg.col, cex = 0.75, pch = leg.pch, ...)
  
} # end of addLegend


