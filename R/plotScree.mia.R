#' 
#' @export
#' @noRd
#' @importFrom graphics plot abline legend axis
#' 

plotScree.mia <- function(pca,  style = "alt", ...) {

	if (style == "trad") stop("Traditional style plots not currently supported for mia objects")
	
	# compute cumulative variance
	lc <- pca$lc
	cumvariance <- cumsum(lc)/sum(lc) * 100
	ncp <- length(cumvariance)
	if (ncp > 10) ncp <- 10
		
	# main plot
	plot(rep(1:ncp, each = nrow(pca$C)), as.vector(pca$C[,1:ncp]), type = "p",
		col = "red", xlab = "component", ylab = "scores",
		xlim = c(1, ncp+0.5), cex = 0.5, xaxt = "n", ...)
	axis(1, at = c(1:ncp), labels = TRUE)
		
	# label with cumulative variance
	lab.txt <- paste(round(cumvariance[1:ncp], 0), "%", sep = "")
	y.pos <- apply(pca$C[,1:ncp], MARGIN = 2, FUN = range)
	y.pos <- y.pos[2,]
	y.max <- max(y.pos)
	off <- 0.1 * y.max
	text(c(1:ncp) + 0.35, off, labels = lab.txt, cex = 0.75)
	abline(h = 0, lty = "dashed", col = "gray")
	
	legend("topright", y = NULL, "cumulative percent variance shown to right of PC", bty = "n", cex = 0.75)

}

