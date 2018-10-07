
### check4Gaps (ChemoSpec2D version, ChemoSpec has its own for now)

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
#' @export
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
