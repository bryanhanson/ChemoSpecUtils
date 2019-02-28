
### remGrpSam (ChemoSpec and ChemoSpec2D)

#'
#' @export
#' @noRd
#'

.remGrpSam <- function(spectra, kill, group = TRUE) {
	
	# This function can remove group(s) or sample(s)
	
	# Get indices to drop if a regex is passed
	# If integer is passed, no need to do anything
	
	.chkArgs(mode = 0L)
	
	if (is.character(kill)) {
		drop <- NA_integer_
		for (n in 1:length(kill)) {
			if (group) more <- grep(kill[n], spectra$groups)
			if (!group) more <- grep(kill[n], spectra$names)
			drop <- c(drop, more)
			}
		kill <- drop[-1]
	}

	if ((length(kill) == 0L) & (group)) stop("No matching groups found to remove")
	if ((length(kill) == 0L) & (!group)) stop("No matching samples found to remove")
	
	# Modify the original objects
	
	if (class(spectra) == "Spectra") { # Spectra objects only
		spectra$data <- spectra$data[-kill,]
		spectra$sym <- spectra$sym[-kill]
		spectra$alt.sym <- spectra$alt.sym[-kill]
	}
	
	if (class(spectra) == "Spectra2D") spectra$data <- spectra$data[-kill, drop = FALSE]
	
	# Both classes
	spectra$names <- spectra$names[-kill]
	spectra$groups <- spectra$groups[-kill, drop = TRUE]
	spectra$colors <- spectra$colors[-kill]
	
	# Wrap up
	
	if (length(spectra$names) == 0) warning("You have removed all your samples!")
	jnk <-  .extraData(spectra)	
	chkSpectra(spectra)
	return(spectra)
}

