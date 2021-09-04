#'
#' Add Points to a Plot
#'
#' @template authors-BH
#' @export
#' @noRd
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
