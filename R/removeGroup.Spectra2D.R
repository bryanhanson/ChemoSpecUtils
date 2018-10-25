#'
#' @noRd
#' @export
#'
removeGroup.Spectra2D <- function(spectra, rem.group) {

	if (missing(spectra)) stop("No spectral data provided")
	if (class(spectra) != "Spectra2D") stop("You need to provide a Spectra2D object")
	if (missing(rem.group)) stop("Nothing to remove")
	chkSpectra(spectra)
	
	spectra <- .remGrpSam(spectra, rem.group, TRUE)
	return(spectra)
}
