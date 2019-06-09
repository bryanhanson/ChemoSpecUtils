#'
#' removeFreq.Spectra
#'
#' @noRd
#' @export
#'
removeFreq.Spectra <- function(spectra, rem.freq = NULL, remF2 = NULL, remF1 = NULL) {

	.chkArgs(mode = 11L)
	
	chkSpectra(spectra)
	
	rfi <- which(rem.freq)
	spectra$data <- spectra$data[,-c(rfi), drop = FALSE]
	spectra$freq <- spectra$freq[-c(rfi)]
	chkSpectra(spectra)

	return(spectra)
	}

