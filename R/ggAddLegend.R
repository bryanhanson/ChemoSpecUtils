#'
#' Title
#'
#' @noRd
#' @param go A string specifying the graphics mode
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#' @param use.sym A bool specifying if the symbols will be used or not.
#' @param leg.loc A list giving x and y coordinates
#' @param p ggplot on which the legend is being added on.
#'
#' @return A ggplot with custom legend.
#' @export
#' @importFrom grid grobTree textGrob gpar
#'
.ggAddLegend <- function(go,spectra,use.sym=FALSE,leg.loc,p ) {
  group <- c(NA_real_)
  color <- c(NA_real_)
  for (i in spectra$groups) {
    if (!(i %in% group)) {
      group <- c(group, i)
    }
  }
  
  for (i in spectra$colors) {
    if (!(i %in% color)) {
      color <- c(color, i)
    }
  }
  group <- group[-1]
  color <- color[-1]
  
  # If use.sym then color of the legend should be black
  if (use.sym) {
    color <- rep("black", length(group))
  }
  
  leg.loc <- .prepLegendCoords(go, leg.loc)
  lab.x <- leg.loc$x
  lab.y <- leg.loc$y
  gap<-0.04
  keys <- grobTree(textGrob("Key",
                            x = lab.x, y = lab.y + gap, hjust = 0,
                            gp = gpar(col = "black", fontsize = 10)
  ))
  
  for (i in 1:length(group)) {
    grob <- grid::grobTree(textGrob(group[i],
                                    x = lab.x, y = lab.y, hjust = 0,
                                    gp = gpar(col = color[i], fontsize = 10)
    ))
    lab.y <- lab.y - gap
    p <- p + annotation_custom(grob) + annotation_custom(keys)
  }
  return(p)
}
