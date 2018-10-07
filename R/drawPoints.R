
### drawPoints (ChemoSpec and ChemoSpec2D)

#'
#' @export
#' @noRd
#' @importFrom graphics plot
#'

.drawPoints <- function(PCs, spectra, case, use.sym, xlim, ylim, ...) {
	if (case == "PCA") {
		if (!use.sym) plot(PCs, type = "p", xlab = "", ylab = "",
		col = spectra$colors, xlim, ylim, pch = 20, ...)
	
		if (use.sym) plot(PCs, type = "p", xlab = "", ylab = "",
		col = "black", xlim, ylim, pch = spectra$sym, ...)
	}
}

