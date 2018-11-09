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
			stop("Argument 'spectra' was not found or not a Spectra object")
	}

	if (mode == 21L) {
		if (class(args$spectra) != "Spectra2D")
			stop("Argument 'spectra' was not found or not a Spectra2D object")
	}

	if (mode == 12L) {
		if (class(args$spectra) != "Spectra")
			stop("Argument 'spectra' was not found or not a Spectra object")
		pcaOK <- ((class(args$pca) == "prcomp") | (class(args$pca)[1] == "conPCA"))
		if (!pcaOK)  stop("Argument 'pca' was not found or did not have class prcomp or pcaCon")
	}

	if (mode == 22L) {
		if (class(args$spectra) != "Spectra2D")
			stop("Argument 'spectra' was not found or not a Spectra2D object")
		pcaOK <- ((class(args$mia) == "mia") | (class(args$pfac) == "parafac"))
		if (!pcaOK)  stop("Argument 'mia/pfac' was not found or did not have class mia/parafac")
	}

	##### Special modes below; checking functions that handle any kind of score object

	if (mode == 13L) { # Special for hcaScores.Spectra & plotScores.Spectra
		if (class(args$spectra) != "Spectra")
			stop("Argument 'spectra' was not found or not a Spectra2D object")
		soOK <- ((class(args$so) == "prcomp") | (class(args$so)[1] == "conPCA"))
		if (!soOK)  stop("Argument 'so' was not found or did not have class prcomp or pcaCon")
	}

	if (mode == 23L) { # Special for hcaScores.Spectra2D & plotScores.Spectra2D
		if (class(args$spectra) != "Spectra2D")
			stop("Argument 'spectra' was not found or not a Spectra2D object")
		soOK <- ((class(args$so) == "mia") | (class(args$so) == "parafac"))
		if (!soOK)  stop("Argument 'so' was not found or did not have class mia/parafac")
	}
} # end of chkArgs

