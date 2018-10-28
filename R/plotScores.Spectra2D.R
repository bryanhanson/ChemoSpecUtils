#' 
#' @export
#' @noRd
#'
plotScores.Spectra2D <- function(spectra, pca,
	pcs = c(1,2), ellipse = "none", tol = "none",
	use.sym = FALSE, leg.loc = "topright", ...) {
	
	# This function will handle score plots from MIA or PARAFAC analyses
	
	if (class(spectra) != "Spectra2D") stop("spectra argument was not a Spectra2D object")
	chkSpectra(spectra)
	pcaOK <- FALSE
	if (class(pca) == "mia") pcaOK <- TRUE
	if (class(pca) == "parafac") pcaOK <- TRUE
	if (!pcaOK) stop("Argument pca must be a mia or parafac object")
	if (length(pcs) != 2L) stop("Please supply two scores to plot (argument pcs)")

	# See stackoverflow.com/a/46289614/633251 for the concepts re: argument handling
	
	# Use a sensible xlab and ylab if none provided
	args <- as.list(match.call()[-1])
	if (!("xlab" %in% names(args))) {
		xlab <- paste("Component", pcs[1], sep = " ")
		args <- c(args, list(xlab = xlab))
		}
	if (!("ylab" %in% names(args))) {
		ylab <- paste("Component", pcs[2], sep = " ")
		args <- c(args, list(ylab = ylab))
		}

	# Update & clean the argument list
	
	args <- c(args, list(use.sym = FALSE))
	do.call(.scorePlot, args)
}