#'
#' @noRd
#' @export
#'
removeSample.Spectra2D <- function(spectra, rem.sam) {

	if (missing(spectra)) stop("No spectral data provided")
	if (class(spectra) != "Spectra2D") stop("You need to provide a Spectra2D object")
	if (missing(rem.sam)) stop("Nothing to remove")
	chkSpectra(spectra)
	
	spectra <- .remGrpSam(spectra, rem.sam, FALSE)
	return(spectra)
}
