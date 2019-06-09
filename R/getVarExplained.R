#'
#' Compute the Variance Explained from PCA or Other Reduction Methods
#'
#' @export
#' @noRd
#'
.getVarExplained <- function(so) {
	
	found <- FALSE
	
	# sparse pca via arrayspc objects
	if ("pev" %in% names(so)) return(so$pev*100)

	# prcomp-based objects
	if ("sdev" %in% names(so)) {
		eigensum <- sum(so$sdev*so$sdev)
		return(100*(so$sdev*so$sdev/eigensum))
	}

	# mia objects
	if ("lc" %in% names(so)) return(100*(so$lc/sum(so$lc)))
	
	if (!found) stop("Could not compute variance from data provided")
}
