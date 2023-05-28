#'
#' Add Points to a Plot
#'
#' *Internal function.*
#'
#' @param PCs  Integer.  The PCs to plot.
#' @param spectra `r .writeSpectra()`
#' @param case String. The type of data reduction that was done.
#' @param use.sym  Logical.  Should symbols be used?
#' @param \ldots `r .writeGraphicsDots()`
#' @return None.  Side effect is a modifed plot.
#'
#' @author `r .writeAuthors("BH")`
#' @export
#' @keywords internal
#' @importFrom graphics plot
#'

.drawPoints <- function(PCs, spectra, case, use.sym, ...) {
  if (case == "PCA") {
    if (!use.sym) plot(PCs, type = "p", col = spectra$colors, pch = 20, ...)
    if (use.sym) plot(PCs, type = "p", col = "black", pch = spectra$sym, ...)
  }

  if (case == "MIA") {
    plot(PCs, type = "p", col = spectra$colors, pch = 20, ...)
  }
}
