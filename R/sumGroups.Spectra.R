#'
#' sumGroups.Spectra
#'
#' @noRd
#' @export
#' 
sumGroups.Spectra <- function(spectra) {

	.chkArgs(mode = 11L)
	chkSpectra(spectra)
	
	gr.l <- levels(spectra$group)
	count <- length(gr.l)
	
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
	
	g.sum <- g.sum[-1,]
	g.sum <- subset(g.sum, no. > 0) # drop groups with no members
	rownames(g.sum) <- c(1:nrow(g.sum))
	return(g.sum)
}
