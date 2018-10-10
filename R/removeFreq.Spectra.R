#'
#' @noRd
#' @export
#'
removeFreq.Spectra <- function(spectra, rem.freq = NULL, remF2 = NULL, remF1 = NULL) {

	if (missing(spectra)) stop("Missing Spectra object (1st argument)")
	if (missing(rem.freq)) stop("Nothing to remove.  Did you intend to use rem.F1 or rem.F2?")
	chkSpectra(spectra)
	
	rfi <- which(rem.freq)
	spectra$data <- spectra$data[,-c(rfi), drop = FALSE]
	spectra$freq <- spectra$freq[-c(rfi)]
	chkSpectra(spectra)

	return(spectra)
	}

