#'
#' Summarize the Group Membership of a Spectra or Spectra2D Object
#' 
#' This function summarizes the group membership	 of
#' a \code{Spectra} or \code{Spectra2D} object.
#' 
#' @param spectra An object of S3 class \code{\link[ChemoSpec]{Spectra}} or 
#' \code{\link[ChemoSpec2D]{Spectra2D}} whose group membership information is desired.
#'
#' @return A data frame as follows. Note that if there are groups with no
#' members these are dropped.
#'  \item{group}{The name of the group.}
#'  \item{no.}{The number in the group.}
#'  \item{color}{The color assigned to the group.}
#'  \item{symbol}{The symbol assigned to the group. \code{Spectra} objects only.}
#'  \item{alt.symbol}{The alternative symbol assigned to the group. \code{Spectra} objects only.}
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso To summarize the entire object, \code{\link{sumSpectra}}.
#'
#' @keywords utilities
#' @export
#'
#' @examples
#' if (requireNamespace("ChemoSpec", quietly = TRUE)) {
#'   library("ChemoSpec")
#'   data(SrE.IR)
#'   sumGroups(SrE.IR)
#' }
#' 
#' if (requireNamespace("ChemoSpec2D", quietly = TRUE)) {
#'   library("ChemoSpec2D")
#'   data(MUD1)
#'   sumGroups(MUD1)
#' }
#' 
sumGroups <- function(spectra){

	chkSpectra(spectra)

	gr.l <- levels(spectra$group)
	count <- length(gr.l)
	
	if (class(spectra) == "Spectra") {
		g.sum <- data.frame(group = NA, no. = NA, color = NA, symbol = NA, alt.sym = NA)
		for (n in 1:count) {
			gi <- match(gr.l[n], spectra$groups)
			gr <- gr.l[n]
			no. <- length(which(gr == spectra$groups))
			col <- spectra$colors[gi]
			sym <- spectra$sym[gi]
			asym <- spectra$alt.sym[gi]
			g.sum <- rbind(g.sum, data.frame(group = gr, no. = no., color = col,
				symbol = sym, alt.sym = asym))
			}		
	}
		
	if (class(spectra) == "Spectra2D") {
		g.sum <- data.frame(group = NA, no. = NA, color = NA)
		for (n in 1:count) {
			gi <- match(gr.l[n], spectra$groups)
			gr <- gr.l[n]
			no. <- length(which(gr == spectra$groups))
			col <- spectra$colors[gi]
			g.sum <- rbind(g.sum, data.frame(group = gr, no. = no., color = col))
			}		
	}

	# Wrap up both classes
	g.sum <- g.sum[-1,]
	g.sum <- subset(g.sum, no. > 0) # drop groups with no members
	rownames(g.sum) <- c(1:nrow(g.sum))
	return(g.sum)
}
