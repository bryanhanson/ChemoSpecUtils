#' 
#' @export
#' @noRd
#'
plotScores.Spectra <- function(spectra, pca,
	pcs = c(1,2), ellipse = "none", tol = "none",
	use.sym = FALSE, leg.loc = "topright", ...) {

	# This function will handle score plots from classical or robust analyses
	
	.chkArgs(mode = 12L)
	
	chkSpectra(spectra)
	pcaOK <- FALSE
	if ("prcomp" %in% class(pca)) pcaOK <- TRUE
	if ("princomp" %in% class(pca)) pcaOK <- TRUE
	if (!pcaOK) stop("Argument pca must be a prcomp or princomp object")
	if (length(pcs) != 2L) stop("Please supply two scores to plot (argument pcs)")

	eigensum <- sum(pca$sdev*pca$sdev) # compute but may not be used
	variance <- 100*(pca$sdev*pca$sdev/eigensum)

	# See stackoverflow.com/a/46289614/633251 for the concepts re: argument handling
	# Use a sensible xlab and ylab if none provided
	args <- as.list(match.call()[-1])
	if (!("xlab" %in% names(args))) {
		xlab <- paste("PC", pcs[1], " score (", format(variance[pcs[1]], digits=2), "%", ")", sep = "")
		args <- c(args, list(xlab = xlab))
		}
	if (!("ylab" %in% names(args))) {
		ylab <- paste("PC", pcs[2], " score (", format(variance[pcs[2]], digits=2), "%", ")", sep = "")
		args <- c(args, list(ylab = ylab))
		}

	# Update & clean the argument list
	
	do.call(.scorePlot, args)
}

