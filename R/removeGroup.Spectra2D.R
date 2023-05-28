#'
#' removeGroup.Spectra2D
#'
#' @author `r .writeDoc_Authors("BH")`
#' @noRd
#' @export
#'
removeGroup.Spectra2D <- function(spectra, rem.group) {
  .chkArgs(mode = 21L)

  if (missing(rem.group)) stop("Nothing to remove")
  chkSpectra(spectra)

  spectra <- .remGrpSam(spectra, rem.group, TRUE)
  return(spectra)
}
