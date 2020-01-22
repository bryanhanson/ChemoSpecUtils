#'
#' sampleDist.Spectra2D
#'
#' @export
#' @noRd
#'
sampleDist.Spectra2D <- function(spectra, method = "pearson", plot = TRUE, ...) {
  .chkArgs(mode = 21L)
  chkSpectra(spectra)

  M1 <- ChemoSpec2D::.unstack(spectra)
  M <- rowDist(M1, method)
  M <- as.matrix(M)
  dimnames(M) <- list(spectra$names, spectra$names)

  if (plot) .distPlot(spectra, M, method, ...)

  M
}
