#'
#' sumSpectra.Spectra
#'
#' @export
#' @noRd
#' 
sumSpectra.Spectra <- function(spectra, ...){
	
	args2 <- as.list(match.call())[-1]
	.chkArgs(mode = 11L)
	chkSpectra(spectra)
	
	# if (!("tol" %in% args2)) h <- check4Gaps(spectra$freq, silent = TRUE)
	# if ("tol" %in% args2) h <- check4Gaps(spectra$freq, silent = TRUE, ...)	

	# Summarize a few things
	h <- check4Gaps(spectra$freq, silent = TRUE)
	g <- sumGroups(spectra)
	res <- abs(median(diff(spectra$freq))) # consistent with method of determining tol above
	
	# Now print main summary to console
	
	cat("\n", spectra$desc, "\n\n")
	
	cat("\tThere are ", length(spectra$names), " spectra in this set.\n", sep = "")
	
	cat("\t", "The y-axis unit is ", as.character(spectra$unit[2]), ".\n\n", sep = "")
	
	cat("\tThe frequency scale runs from\n\t", spectra$freq[1], " to ", 
		spectra$freq[length(spectra$freq)], " ", as.character(spectra$unit[1]), "\n", sep = "")
		
	cat("\tThere are ", length(spectra$freq), " frequency values.\n", 
		sep = "")
		
	cat("\tThe frequency resolution is\n\t", res, " ", as.character(spectra$unit[1]), "/point.\n\n", sep = "")
	
	if (nrow(h) > 1) {
		cat("\tThis data set is not continuous\n\talong the frequency axis.\n")
		cat("\tHere are the data chunks:\n\n")
		print(h)
		}
		
	cat("\n")
	cat("\tThe spectra are divided into", length(levels(spectra$groups)), "groups:", "\n\n")
	print(g)
	
	# Check for extra data and report if found
	cat("\n")
	jnk <- .extraData(spectra)
	
	cat("\n*** Note: this is an S3 object\nof class 'Spectra'\n")
	}

