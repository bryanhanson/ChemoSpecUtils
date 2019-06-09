#'
#' removeSample.Spectra
#'
#' @noRd
#' @export
#'
removeSample.Spectra <- function(spectra, rem.sam) {

	.chkArgs(mode = 11L)

	if (missing(rem.sam)) stop("Nothing to remove")
	chkSpectra(spectra)
	
	spectra <- .remGrpSam(spectra, rem.sam, FALSE)
	return(spectra)
}
