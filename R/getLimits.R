	
### getLimits (ChemoSpec2D only)

# Get limits from a user specified formula (used in removeFreq2D and removePeaks2D)
# The user may not know or think about whether F2 or F1 is ascending or descending
# so we will try to get it right no matter how the user gives
# the formula; e.g. 6 ~ 3 ought to be handled as 3 ~ 6.

#'
#' @export
#' @noRd
#'

.getLimits <- function(spectra, dim, form) {
	lhs <- form[[2]]
	rhs <- form[[3]]
	if (as.character(lhs) == "low") lhs <- min(spectra[[dim]])
	if (as.character(lhs) == "high") lhs <- max(spectra[[dim]]) 
	if (as.character(rhs) == "low") rhs <- min(spectra[[dim]])
	if (as.character(rhs) == "high") rhs <- max(spectra[[dim]])
	ans <- c(lhs, rhs)
	if (is.unsorted(ans)) ans <- rev(ans)
	return(ans) # should always give numeric values in order
}
