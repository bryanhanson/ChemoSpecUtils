
#'
#' Add Data Points and Optionally Ellipses to a Score Plot
#' 
#' Plots the requested PCA, MIA or PARAFAC scores using the color scheme derived from a
#' \code{\link[ChemoSpec]{Spectra}} or \code{Spectra2D} object.
#' Options are provided to add confidence ellipses for each group in the object.
#' The ellipses may be robust or classical.  Option to label the extreme points provided.
#' Any legend requested is also handled here.
#' 
#' @param spectra An object of S3 class \code{\link[ChemoSpec]{Spectra}}
#' or \code{Spectra2D} object.
#'
#' @param pca An object of class \code{\link{prcomp}}, \code{parafac}
#' or \code{pcasup1}.
#' In the first case the object should be modified to include a
#' list element called \code{$method}, a character string describing the
#' pre-processing carried out and the type of PCA performed (it appears on the
#' plot).  This is automatically provided if \code{ChemoSpec} functions
#' \code{\link{c_pcaSpectra}} or \code{\link{r_pcaSpectra}} were used to create
#' \code{pca}.
#'
#' @param pcs A vector of two integers specifying the PCA scores to plot.
#'
#' @param ellipse A character vector specifying the type of ellipses to be
#' plotted.  One of \code{c("both", "none", "cls", "rob")}.  \code{cls}
#' specifies classical confidence ellipses, \code{rob} specifies robust
#' confidence ellipses.  An ellipse is drawn for each group in 
#' \code{spectra$groups}, unless there are not enough data points (in
#' which case a warning is issued).
#'
#' @param tol A number describing the fraction of points to be labeled.
#' \code{tol = 1.0} labels all the points; \code{tol = 0.05} labels the most
#' extreme 5 percent.
#'
#' @param use.sym A logical; if true, the color scheme is set to black and the
#' points plotted with symbols. Only applies to \code{ChemoSpec}.
#'
#' @param leg.loc Character; if \code{"none"} no legend will be drawn.
#' Otherwise, any string acceptable to \code{\link{legend}}.
#'
#' @param \dots Additional parameters to be passed to the plotting functions.
#'
#' @return None.  Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords multivariate robust hplot
#' @noRd
#' @export
#' @importFrom plyr dlply llply m_ply
#'
.plotScores <- function(spectra, pca,
	pcs = c(1,2), ellipse = "none", tol = "none",
	use.sym = FALSE, leg.loc = "topright", ...) {

	# Step 0. Check the inputs
	
	if (length(pcs) != 2) stop("You must choose exactly two PC's to plot")
	if (missing(spectra)) stop("No spectral data set provided")
	if (missing(pca)) stop("No pca results provided")
	case <- NULL # set up flags for the different classes of score results, & check for legit score object
	if ((class(pca) == "prcomp") || (class(pca) == "conPCA")) case <- "PCA"
	if ((class(pca) == "parafac") || (class(pca) == "pcasup1")) case <- "MIA"
	if (is.null(case)) stop("Your score object has the wrong class! Double check that the Spectra object is the 1st argument and the score object is the 2nd argument.")
	if ((case == "MIA") && (use.sym)) stop("ChemoSpec2D does not support use.sym.")
	chkSpectra(spectra)
	
	# Prep the data
	
	if (case == "PCA") DF <- data.frame(pca$x[,pcs], group = spectra$groups)
	if (case == "MIA") DF <- data.frame(pca$C[,pcs], group = spectra$groups)
	GRPS <- dlply(DF, "group", subset, select = c(1,2))
	
	# Step 1.  Compute overall plot limits, incl. ellipses if requested
	
	if ((ellipse == "cls") || (ellipse == "rob") || (ellipse == "both")) {
		# Compute ellipses and get overall plot limits.
		# Keep in mind the ellipses may be quite flattened and hence large.
		# At the same time, the ellipses might be quite round and
		# the scores well outside them, if there is an outlier.
		# Must check all cases!

		# There must be at least 3 data points per level to make a classic ellipse.
		# Possibly more to make a robust ellipse, as at least one point may be dropped.

		gr <- sumGroups(spectra)

		for (n in 1:length(gr$group)) {
			if (gr$no.[n] == 1) message("Group ", gr$group[n], "\n\thas only 1 member (no ellipse possible)")
			if (gr$no.[n] == 2) message("Group ", gr$group[n], "\n\thas only 2 members (no ellipse possible)")
			if (gr$no.[n] == 3) message("Group ", gr$group[n], "\n\thas only 3 members (ellipse not drawn)")
			}
		
		idx <- which(gr$no. > 3) # Index for those groups that will get ellipses
		gr <- gr[idx,]
		ELL <- llply(GRPS[idx], .plotScoresCor) # these are the ellipses we'll need later
	
		x.scores <- range(llply(GRPS, subset, select = 1))
		y.scores <- range(llply(GRPS, subset, select = 2)) 
		x.ell <- range(llply(ELL, function(x) {range(x[1])}))
		y.ell <- range(llply(ELL, function(x) {range(x[2])}))
		x.ell.r <- range(llply(ELL, function(x) {range(x[4])}))
		y.ell.r <- range(llply(ELL, function(x) {range(x[5])}))
		x.all <- range(x.scores, x.ell, x.ell.r)*c(1.0, 1.05) # expand slightly for labels
		y.all <- range(y.scores, y.ell, y.ell.r)
		y.all[2] <- y.all[2]*1.15 # leave room for annotations at top of plot
						
	}

	if (ellipse == "none") {	
		x.scores <- range(llply(GRPS, subset, select = 1))
		y.scores <- range(llply(GRPS, subset, select = 2)) 
		x.all <- range(x.scores)*c(1.0, 1.05) # expand slightly for labels
		y.all <- range(y.scores)
		y.all[2] <- y.all[2]*1.15 # leave room for annotations at top of plot
	}

	# Step 2.  Draw the scores.
	
	.drawPoints(DF[,1:2], spectra, case, use.sym, xlim = x.all, ylim = y.all, ...)

	# Step 3.  Draw the ellipses if requested.
	
	if ((ellipse == "cls") | (ellipse == "rob") | (ellipse == "both")) .drawEllipses(ELL, gr, ellipse, use.sym, ...)
	
	# Step 4.  Decorations
	
	if (case == "PCA") {
		.plotScoresDecoration(spectra, pca, pcs, tol)
		if (leg.loc == "none") return()
		if (leg.loc != "none") .addLegend(spectra, leg.loc, use.sym, bty = "n")
		.addAnnotation(ellipse)
	}
	
	if (case == "MIA") {
		if (leg.loc == "none") return()
		if (leg.loc != "none") .addLegend(spectra, leg.loc, use.sym, bty = "n")		
	}
	
	# Step 5.  Label extremes if requested
	
	if (!tol == "none") .labelExtremes(DF[,1:2], spectra$names, tol)
	
} # End of addPointsAndEllipses
