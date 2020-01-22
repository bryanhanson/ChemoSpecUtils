#'
#' sampleDist.Spectra
#'
#' @export
#' @noRd
#'
sampleDist.Spectra <- function(spectra, method = "pearson", plot = TRUE, ...) {
  .chkArgs(mode = 11L)
  chkSpectra(spectra)

  M <- rowDist(spectra$data, method)
  M <- as.matrix(M)
  dimnames(M) <- list(spectra$names, spectra$names)

  if (plot) .distPlot(spectra, M, method, ...)

  M
}
