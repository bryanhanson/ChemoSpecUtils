#
# These are non-exported helper functions
#
# They are very lean and don't necessarily check arguments, so the calling
# function must check the arguments.
#
# When things are documented roxygen2 style, use @noRd to prevent an entry in the manual
#
		
### Get limits from a user specified formula (used in removeFreq2D and removePeaks2D)
	
# The user may not know or think about whether F2 or F1 is ascending or descending
# so we will try to get it right no matter how the user gives
# the formula; e.g. 6 ~ 3 ought to be handled as 3 ~ 6.
	
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


### findNA

#'
#'
#' Find NA in a Spectra2D Object
#' 
#' This function identifies the extent of any NA in a \code{\link{Spectra2D}} object.
#' This information can be used for summaries and plots.  Not intended to be called
#' by the user.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param retFreq Logical. Should the frequencies be returned?
#'
#' @return A list with two elements giving the indices of NAs for rows and columns,
#'         unless \code{retFreq = TRUE}, in which case the frequencies are returned.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities
#' @noRd
#' @importFrom stats na.omit
#'
.findNA <- function(spectra, retFreq = FALSE) {

	M <- spectra$data[[1]] # All spectra are assumed to have the same set of NAs
	                       # This is verified by chkSpectra2D
	
	# Find rows (columns) that are all NA
	
	# Check along F2 for columns that are all NA (displayed as vertical lines in plotSpectra2D)
	# As spectra$F2 is sorted ascending, but plotted descending, 
	# we must reverse F2 if we want correct frequencies for plotting.
	# No such fix is needed for F1.
	cNA <- rep(NA_integer_, ncol(M))
	for (i in 1:ncol(M)) {
		if (all(is.na(M[,i]))) cNA[i] <- i
	}
	cNA <- as.integer(na.omit(cNA))	
    if (retFreq) cNA <- rev(spectra$F2)[cNA]
    
	# Check along F1 for rows that are all NA (displayed as horizontal lines in plotSpectra2D)
	rNA <- rep(NA_integer_, nrow(M))
	for (i in 1:nrow(M)) {
		if (all(is.na(M[i,]))) rNA[i] <- i
	}
	rNA <- as.integer(na.omit(rNA))
    if (retFreq) rNA <- spectra$F1[rNA]
	
	return(list(rowNA = rNA, colNA = cNA))
	}


### check4Gaps

#'
#' Check for Discontinuities (Gaps) in a Vector
#' 
#' The basic procedure is to compare x[n + 1] - x[n] for successive values of
#' n.  When this value jumps, there is a gap which is flagged. \code{beg.indx}
#' and \code{end.indx} will always be contiguous as indices must be; it is the
#' \code{x} values that jump or have the gap.  The indices are provided as they
#' are more convenient in some programming contexts.  If not assigned, the
#' result appears at the console.
#' 
#' @param x A numeric vector to be checked for gaps.
#' 
#' @param tol A number indicating the tolerance for checking to see if the step
#' between successive \code{x} values are the same.  Depending upon how the
#' \code{x} values are stored and rounded, you may need to change the value of
#' \code{tol} to avoid flagging trivial "gaps".
#' 
#' @return A data frame giving the data chunks found, with one chunk per line.
#' \item{beg.freq }{The first frequency value in a
#' given data chunk.} \item{end.freq }{The last frequency value in a given data
#' chunk.} \item{size }{The length (in frequency units) of the data chunk.}
#' \item{beg.indx }{The index of the first frequency value in the data chunk.}
#' \item{eng.indx }{The index of the last frequency value in the data chunk.}
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @keywords utilities
#' @noRd
#'
.check4Gaps <- function(x, tol = 0.01) {
	
	len.x <- length(x)
	xdiff <- abs(diff(x))
	p <- min(xdiff) # nominal freq/pt
	d1 <- x[1] # beg of data chunk by value
	d1i <- 1L # beg of data chunk by index
	d2 <- c() # end of data chunk by value
	d2i <- c() # end of data chunk by index

	# Check for gaps and build up values and indices
	for (i in 1:length(xdiff)) {
		# Nuance of all.equal pointed out by Dana Nadler, e-mails March 2017. Thanks!
		if (!isTRUE(all.equal(xdiff[i], p, tolerance = tol, scale = 1.0))) { # detects discontinuity
			d1 <- c(d1, x[i+1])
			d1i <- c(d1i, i+1)
			d2 <- c(d2, x[i])
			d2i <- c(d2i, i)
			}	
		}
	# Add the last entry
	d2 <- c(d2, x[len.x])
	d2i <- c(d2i, len.x)

	DF <- data.frame(beg.freq = d1, end.freq = d2, size = NA, beg.indx = d1i, end.indx = d2i)
	DF$size <- DF$end.freq - DF$beg.freq
	
	return(DF)
	}

### extraData

.extraData <- function(spectra) {
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

