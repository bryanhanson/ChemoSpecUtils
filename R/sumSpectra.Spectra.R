#'
#' @export
#' @noRd
#' 
sumSpectra.Spectra <- function(spectra, ...){
		
	chkSpectra(spectra)
	
	# Try to determine a sensible value for tol if none provided via the ...
	
	args <- names(as.list(match.call()[-1]))

	if (!("tol" %in% args)) {
		# Improvements to the automatic calc of a suitable tol value suggested
		# by Dana Nadler in e-mails, early March 2017.  Any errors are mine however!		
		# fdiff includes normal data resolution & any larger gaps
		# hist(fdiff) should be dominated by data resolution
		# unique(fdiff) shows that even basic data resolution suffers from encoding differences
		fdiff <- diff(spectra$freq) 
		tol <- abs(median(fdiff)) * 1.2 # ensures value is a bit larger than nominal resolution
		h <- .check4Gaps(spectra$freq, tol = tol)	
		}
	

	if ("tol" %in% args) h <- .check4Gaps(spectra$freq, ...)	

	# Other summaries
	
	g <- sumGroups(spectra)
	res <- abs(median(diff(spectra$freq))) # consistent with method of determining tol above
	
	# Now print main summary to console
	
	cat("\n", spectra$desc, "\n\n")
	
	cat("\tThere are ", length(spectra$names), " spectra in this set.\n", sep = "")
	
	cat("\t", "The y-axis unit is ", spectra$unit[2], ".\n\n", sep = "")
	
	cat("\tThe frequency scale runs from\n\t", spectra$freq[1], " to ", 
		spectra$freq[length(spectra$freq)], " ", spectra$unit[1], "\n", sep = "")
		
	cat("\tThere are ", length(spectra$freq), " frequency values.\n", 
		sep = "")
		
	cat("\tThe frequency resolution is\n\t", res, " ", spectra$unit[1], "/point.\n\n", sep = "")
	
	if (length(h) > 1) {
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
	
	cat("\n*** Note: this data is an S3 object of class 'Spectra2D'\n")
	}

