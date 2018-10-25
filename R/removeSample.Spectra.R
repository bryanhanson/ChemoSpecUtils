#'
#' @noRd
#' @export
#'
removeSample.Spectra <- function(spectra, rem.sam) {

	if (missing(spectra)) stop("No spectral data provided")
	if (class(spectra) != "Spectra") stop("You need to provide a Spectra object")
	if (missing(rem.sam)) stop("Nothing to remove")
	chkSpectra(spectra)
	
	spectra <- .remGrpSam(spectra, rem.sam, FALSE)
	return(spectra)
}
