#'
#' plotScree.default
#'
#' @export
#' @noRd
#' @importFrom graphics points abline legend axis
#'

plotScree.default <- function(pca, style = "alt", ...) {

	variance <- .getVarExplained(pca)
	cumvariance <- cumsum(variance)
	
	ncp <- length(variance)
	if (ncp > 10) ncp <- 10
	
	if (style == "trad") {
		
		plot(c(1:ncp), variance[1:ncp], type = "l", col = "red", xlab = "factor", ylab = "percent", ylim = c(0,100), ...)
		axis(1, at = c(1:ncp), labels = TRUE)
		points(c(1:ncp), cumvariance[1:ncp], type="l", col="blue")
		
		abline(v = c(1:ncp), h = c(0,10,20,30,40,50,60,70,80,90,100), col = "lightgray")
		abline(h = 95, lty = "dashed")

		legend("bottomleft", y = NULL, pca$method, bty = "n", cex = 0.75)
		legend("topright", y = NULL, "cumulative percent", lty = 1, bty = "n", inset = c(0, 0.40), col = "blue", cex = 0.75)
		legend("topright",y = NULL, " individual percent", lty = 1, bty = "n", inset = c(0, 0.50), col = "red", cex = 0.75)		
	}
	
	if (style == "alt") {
	
	# Handle class specific stuff here, better than separate dispatch
		if ("prcomp" %in% class(pca)) {
			plot(rep(1:ncp, each = nrow(pca$x)), as.vector(pca$x[,1:ncp]), type = "p",
				col = "red", xlab = "component", ylab = "scores",
				xlim = c(1, ncp+0.5), cex = 0.5, xaxt = "n", ...)
			y.pos <- apply(pca$x[,1:ncp], MARGIN = 2, FUN = range) # used in a moment
		}
		
		if ("mia" %in% class(pca)){
			plot(rep(1:ncp, each = nrow(pca$C)), as.vector(pca$C[,1:ncp]), type = "p",
				col = "red", xlab = "component", ylab = "scores",
				xlim = c(1, ncp+0.5), cex = 0.5, xaxt = "n", ...)
			y.pos <- apply(pca$C[,1:ncp], MARGIN = 2, FUN = range) # used in a moment
		}
			
		axis(1, at = c(1:ncp), labels = TRUE)
		
		# label with cumulative variance
		lab.txt <- paste(round(cumvariance[1:ncp], 0), "%", sep = "")
		y.pos <- y.pos[2,]
		y.max <- max(y.pos)
		off <- 0.1 * y.max
		text(c(1:ncp) + 0.35, off, labels = lab.txt, cex = 0.75)
		abline(h = 0, lty = "dashed", col = "gray")
	
		legend("bottomright", y = NULL, pca$method, bty = "n", cex = 0.75)
		legend("topright", y = NULL, "cumulative percent variance shown to right of PC", bty = "n", cex = 0.75)		
	}
	
	} # end of plotScree.default

