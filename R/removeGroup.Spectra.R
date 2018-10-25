#'
#' @noRd
#' @export
#'
removeGroup.Spectra <- function(spectra, rem.group) {

	if (missing(spectra)) stop("No spectral data provided")
	if (class(spectra) != "Spectra") stop("You need to provide a Spectra object")
	if (missing(rem.group)) stop("Nothing to remove")
	chkSpectra(spectra)
	
	spectra <- .remGrpSam(spectra, rem.group, TRUE)
	return(spectra)
}
