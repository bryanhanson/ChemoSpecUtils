
### extraData (ChemoSpec and ChemoSpec2D)

#'
#' @export
#' @noRd
#'
.extraData <- function(spectra) {
	
	.chkArgs(mode = 0L)
	
	trouble <- FALSE
	spec.names <- names(spectra)
	if (class(spectra) == "Spectra") {
		reqd.names <- c("freq", "data", "names", "groups", "colors", "sym", "alt.sym", "unit", "desc")
		}
	if (class(spectra) == "Spectra2D") {
		reqd.names <- c("F2", "F1", "data", "names", "groups", "colors", "units", "desc")
		}
	extra <- setdiff(spec.names, reqd.names)
	
	if (length(extra) > 0) {
		# Give the extra data names & check their lengths
		ns <- length(spectra$names)
		for (i in 1:length(extra)) {
			msg <- paste("\tAdditional data was found:", extra[i], sep = " ")
			message(msg)		
			if (length(spectra[[extra[i]]]) != ns) {
				msg <- paste("\tThe length of *", extra[i],
					"* did not match the number of samples.\n", sep = "")
				message(msg)
				trouble <- TRUE			
			}
		}
	}
	
	return(trouble)
}
