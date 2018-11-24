#'
#' Check for Discontinuities (Gaps) in a Vector & Optionally Make a Plot
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
#' @param y An optional vector of \code{y}-values which correspond to the \code{x}
#' values.  Only used in \code{ChemoSpec}.  If provided, a plot will be made
#' in the style of a \code{\link[ChemoSpec]{Spectra}} object showing the gap(s).
#' 
#' @param tol A number indicating the tolerance for checking to see if the step
#' between successive \code{x} values are the same.  Depending upon how the
#' \code{x} values are stored and rounded, you may need to change the value of
#' \code{tol} to avoid flagging trivial "gaps".  If \code{NULL}, a value is
#' chosen which is just above the median difference between \code{x} values.
#' 
#' @param silent Logical indicating a "no gap" message
#' should not be reported to the console.  Important because
#' \code{check4Gaps} is called iteratively by other functions.
#' 
#' @param \dots Other parameters to be passed to the plot routines if
#' \code{y} is provided, e.g. \code{xlim}.
#' 
#' @return A data frame giving the data chunks found, with one chunk per row.
#' Also a plot if {y} is provided.  In the event there are no gaps found,
#' a data frame with one row is returned.  The data frame has columns as follows:
#' \item{beg.freq }{The first frequency value in a given data chunk.}
#' \item{end.freq }{The last frequency value in a given data chunk.}
#' \item{size }{The length (in frequency units) of the data chunk.}
#' \item{beg.indx }{The index of the first frequency value in the data chunk.}
#' \item{end.indx }{The index of the last frequency value in the data chunk.}
#' 
#' @seealso \code{\link{sumSpectra}} which make extensive use of this function.
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @keywords utilities
#' 
#' @examples
#' 
#' x <- seq(0, 2*pi, 0.1)
#' y <- sin(x)
#' remove <- c(8:11, 40:45)
#' x <- x[-remove]
#' y <- y[-remove]
#' gaps <- check4Gaps(x, tol = 0.11) # tol just larger than orig spacing
#' gaps
#' gaps <- check4Gaps(x, y, tol = 0.11) # show a plot if y given
#'
#' @export
#' 
#' @importFrom graphics lines rect 
#'
check4Gaps <- function(x, y = NULL, silent = FALSE, tol = NULL, ...) {
	
	# Estimate a value for tol if none given
	if (is.null(tol)) tol <- abs(median(diff(x))) * 1.2
	
	# Prep a data frame listing the data chunks
	
	len.x <- length(x)
	xdiff <- abs(diff(x))
	d1 <- x[1] # beg of data chunk by value
	d1i <- 1L # beg of data chunk by index
	d2 <- c() # end of data chunk by value
	d2i <- c() # end of data chunk by index

	# Check for gaps and build up values and indices
	# Check globally before going through value-by-value to save time
	if (any(xdiff > tol)) {
		for (i in 1:length(xdiff)) {
			if (xdiff[i] > tol) {
				d1 <- c(d1, x[i+1])
				d1i <- c(d1i, i+1)
				d2 <- c(d2, x[i])
				d2i <- c(d2i, i)
			}	
		}		
	}

	# Add the last entry
	d2 <- c(d2, x[len.x])
	d2i <- c(d2i, len.x)

	DF <- data.frame(beg.freq = d1, end.freq = d2, size = NA, beg.indx = d1i, end.indx = d2i)
	DF$size <- DF$end.freq - DF$beg.freq
	
	# Now, plot if requested (ChemoSpec only)
	
	if ((nrow(DF) == 1) && (!silent)) cat("No gaps were found by check4Gaps\nNo plot will be made\n")
	
	if ((nrow(DF) > 1) && (!is.null(y))) {
		plot(x, y, type = "l", col = "black", main = "Gaps in Frequency Data",
			ylab = "", xlab = "marked regions are skipped in data set", ...)
		ybottom <- min(y) - 0.1 * diff(range(y))
		ytop <- max(y) + 0.1* diff(range(y)) # only needs to exceed plotting area to fill it
		for (n in 1:(nrow(DF)-1)){
			lines(x = c(DF[n,2], DF[n+1,1]), y = c(y[DF[n,5]], y[DF[n+1,4]]), lty = 2, col = "white")
			rect(xleft = DF[n,2], ybottom, xright = DF[n+1,1], ytop, density = 15, col = "pink")
			}
		}

	return(DF)

	}

