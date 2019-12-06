#'
#' removeSample.Spectra2D
#'
#' @noRd
#' @export
#'
removeSample.Spectra2D <- function(spectra, rem.sam) {
  .chkArgs(mode = 21L)

  if (missing(rem.sam)) stop("Nothing to remove")
  chkSpectra(spectra)

  spectra <- .remGrpSam(spectra, rem.sam, FALSE)
  return(spectra)
}
