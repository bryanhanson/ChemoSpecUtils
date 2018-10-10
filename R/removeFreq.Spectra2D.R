
#'
#' @noRd
#' @importFrom plyr is.formula
#' @export
#'
removeFreq.Spectra2D <- function(spectra, rem.freq = NULL, remF2 = NULL, remF1 = NULL) {

	if (missing(spectra)) stop("No spectral data provided")
	if (is.null(remF2) & is.null(remF1)) stop("Nothing to remove")
	chkSpectra(spectra)
	
	# Subset data as requested
	
	if (!is.null(remF2)) { # F2 dimension: sorted F2 runs e.g. 0...10
		if (!is.formula(remF2)) stop("remF2 must be a formula")
		limits <- .getLimits(spectra, "F2", remF2)
		toss <- !((spectra$F2 >= limits[1]) & (spectra$F2 <= limits[2]))
		for (i in 1:length(spectra$data)) spectra$data[[i]] <- spectra$data[[i]][,rev(toss), drop = FALSE]
		spectra$F2 <- spectra$F2[toss] # rev needed since 0 in lr corner
	}

	if (!is.null(remF1)) { # F1 dimension: sorted F1 runs e.g. 0...10 unsorted F1 runs e.g. 10...0
		if (!is.formula(remF1)) stop("remF1 must be a formula")
		limits <- .getLimits(spectra, "F1", remF1)
		toss <- !((spectra$F1 >= limits[1]) & (spectra$F1 <= limits[2]))
		for (i in 1:length(spectra$data)) spectra$data[[i]] <- spectra$data[[i]][toss,, drop = FALSE]
		spectra$F1 <- spectra$F1[toss]
	}

	chkSpectra(spectra)		
	return(spectra)
	}

