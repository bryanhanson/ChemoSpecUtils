#'
#' @export
#' @importFrom stats median
#' @noRd
#' 
sumSpectra.Spectra2D <- function(spectra, ...){
	
	chkSpectra(spectra)

	# Try to determine a sensible value for tol if none provided via the ...
	# Then analyze for gaps
	
	args <- names(as.list(match.call()[-1]))

	if (!("tolF1" %in% args)) {
		diffF1 <- diff(spectra$F1) 
		tolF1 <- abs(median(diffF1)) * 1.2 # ensures value is a bit larger than nominal resolution
		gF1 <- .check4Gaps(spectra$F1, tol = tolF1)	
		}
	
	if (!("tolF2" %in% args)) {
		diffF2 <- diff(spectra$F2) 
		tolF2 <- abs(median(diffF2)) * 1.2 # ensures value is a bit larger than nominal resolution
		gF2 <- .check4Gaps(spectra$F2, tol = tolF2)	
		}

	if ("tolF1" %in% args) gF1 <- .check4Gaps(spectra$F1, tol = tolF1)	
	if ("tolF2" %in% args) gF2 <- .check4Gaps(spectra$F2, tol = tolF2)	

	# Check for NAs in the matrices
	
	NAindx <- .findNA(spectra, retFreq = FALSE)
	foundNA<- FALSE
	if ((length(NAindx[[1]]) > 0) | (length(NAindx[[2]]) > 0)) foundNA <- TRUE
	
	# Now print main summary to console
	
	cat("\n", spectra$desc, "\n\n")
	
	cat("\tThere are ", length(spectra$names), " spectra in this set.\n\n", sep = "")
	
	cat("\tThe F2 dimension runs from ", spectra$F2[1], " to ", 
		spectra$F2[length(spectra$F2)], " ", spectra$unit[1],
		"\n\tand there are ", length(spectra$F2), " data points.\n", sep = "")
		
	if (nrow(gF2) > 1) {
		cat("\n\tThe F2 dimension has gaps. Here are the data chunks:\n\n")
		print(gF2)
		}
	cat("\n")
	
	cat("\tThe F1 dimension runs from ", spectra$F1[1], " to ", 
		spectra$F1[length(spectra$F1)], " ", spectra$unit[2],
		"\n\tand there are ", length(spectra$F1), " slices.\n", sep = "")
		
	if (nrow(gF1) > 1) {
		cat("\n\tThe F1 dimension has gaps. Here are the data chunks:\n\n")
		print(gF1)
		}
	cat("\n")
	
	if (foundNA) cat("\tNAs were found in the data matrices.  To see where, use plotSpectra2D.\n\n")
	
	cat("\tThe spectra are divided into", length(levels(spectra$groups)), "groups:", "\n\n")
	print(sumGroups(spectra))
	
	# Check for extra data and report if found
	cat("\n")
	jnk <- .extraData(spectra)
	
	cat("\n*** Note: this data is an S3 object of class 'Spectra2D'\n")
	}

