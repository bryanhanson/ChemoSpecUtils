#'
#' removeFreq.Spectra
#'
#' @template authors-BH
#' @noRd
#' @export
#'
removeFreq.Spectra <- function(spectra, rem.freq = NULL, remF2 = NULL, remF1 = NULL) {
  .chkArgs(mode = 11L)

  chkSpectra(spectra)

  if (!inherits(rem.freq, "formula")) {
    rfi <- which(rem.freq)
    spectra$data <- spectra$data[, -c(rfi), drop = FALSE]
    spectra$freq <- spectra$freq[-c(rfi)]
  }

  if (inherits(rem.freq, "formula")) {
    limits <- .getLimits(spectra, "freq", rem.freq)
    toss <- !((spectra$freq >= limits[1]) & (spectra$freq <= limits[2]))
    spectra$data <- spectra$data[, toss, drop = FALSE]
    spectra$freq <- spectra$freq[toss]
  }

  chkSpectra(spectra)
  return(spectra)
}
