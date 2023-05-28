#'
#' removeGroup.Spectra
#'
#' @author `r .writeDoc_Authors("BH")`
#' @noRd
#' @export
#'
removeGroup.Spectra <- function(spectra, rem.group) {
  .chkArgs(mode = 11L)

  if (missing(rem.group)) stop("Nothing to remove")
  chkSpectra(spectra)

  spectra <- .remGrpSam(spectra, rem.group, TRUE)
  return(spectra)
}
