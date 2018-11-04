#'
#' Check spectra and pca Arguments of Functions That Require Them
#'
#' @export
#' @noRd
#'
.chkArgs <- function(mode = 11L) {
	
	# The following is from stackoverflow.com/a/53137483/633251
    fargs <- function(n) { mget(names(formals(sys.function(n))), sys.frame(n), inherits = TRUE); }
    args <- fargs(-2);  # 2 because the helper function fargs is yet another level down
	
	# print(data.frame(cls = unlist(lapply(args, class)))) # save for debugging
	
	if (mode == 0L) {
		specOK <-  ((class(args$spectra) == "Spectra") | (class(args$spectra) == "Spectra2D"))
		if (!specOK) stop("Argument 'spectra' was not found or did not have class Spectra or Spectra2D")
	}

	if (mode == 11L) {
		if (class(args$spectra) != "Spectra")
			stop("Argument 'spectra' was not a Spectra object or not found")
	}

	if (mode == 21L) {
		if (class(args$spectra) != "Spectra2D")
			stop("Argument 'spectra' was not a Spectra2D object or not found")
	}

	if (mode == 12L) {
		if (class(args$spectra) != "Spectra")
			stop("Argument 'spectra' was not a Spectra object or not found")
		pcaOK <- ((class(args$pca) == "prcomp") | (class(args$pca) == "princomp"))
		if (!pcaOK)  stop("Argument 'pca' was not found or did not have class prcomp or princomp")
	}

	if (mode == 22L) {
		if (class(args$spectra) != "Spectra2D")
			stop("Argument 'spectra' was not a Spectra2D object or not found")
		pcaOK <- ((class(args$pca) == "mia") | (class(args$pca) == "parafac"))
		if (!pcaOK)  stop("Argument 'pca' was not found or did not have class mia or parafac")
	}
	
} # end of chkArgs

