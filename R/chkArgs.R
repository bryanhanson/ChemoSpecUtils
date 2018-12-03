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
		specOK <- FALSE
		specOK <-  any("Spectra" %in% class(args$spectra), "Spectra2D" %in% class(args$spectra))
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
		pcaOK <- FALSE
		pcaOK <- any("prcomp" %in% class(args$pca), "conPCA" %in% class(args$pca))
		if (!pcaOK)  stop("Argument 'pca' was not found or did not have class prcomp or pcaCon")
	}

	if (mode == 22L) {
		if (class(args$spectra) != "Spectra2D")
			stop("Argument 'spectra' was not found or not a Spectra2D object")
		pcaOK <- FALSE
		pcaOK <- any("mia" %in% class(args$mia), "parafac" %in% class(args$pfac))
		if (!pcaOK)  stop("Argument 'mia/pfac' was not found or did not have class mia/parafac")
	}

	##### Special modes below; checking functions that handle any kind of score object

	if (mode == 13L) { # Special for hcaScores.Spectra & plotScores.Spectra
		if (class(args$spectra) != "Spectra")
			stop("Argument 'spectra' was not found or not a Spectra object")
		soOK <- FALSE
		soOK <- any("prcomp" %in% class(args$so), "conPCA" %in% class(args$so), 
		  grepl("PCAgrid", args$so), "princomp" %in% class(args$so))
		if (!soOK)  stop("Argument 'so' was not found or was not the correct class")
	}

	if (mode == 23L) { # Special for hcaScores.Spectra2D & plotScores.Spectra2D
		if (class(args$spectra) != "Spectra2D")
			stop("Argument 'spectra' was not found or not a Spectra2D object")
		soOK <- FALSE
		soOK <- any("mia" %in% class(args$so), "parafac" %in% class(args$so))
		if (!soOK)  stop("Argument 'so' was not found or did not have class mia/parafac")
	}
} # end of chkArgs

