#'
#' Add Legend to a ggplot2 Plot
#'
#' This function creates a suitable legend and returns it as a grob,
#' ready to be added to an existing plot.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#' @param use.sym Logical specifying if symbols will be used or not.
#' @param leg.loc A list giving x and y coordinates.
#'
#' @return A ggplot with custom legend.
#'
#' @export
#' @noRd
#' @importFrom grid grobTree textGrob gpar gTree gList
#'
.ggAddLegend <- function(spectra, use.sym, leg.loc) {

  # get the needed data
  gr_sum <- sumGroups(spectra) # group, color, symbol
  group <- gr_sum$group
  color <- gr_sum$color
  if (use.sym) color <- rep("black", nrow(gr_sum))
  symbol <- gr_sum$symbol
  ng <- length(group)

  # figure out legend coords
  leg.loc <- .prepLegendCoords("ggplot2", leg.loc)
  leg.x <- leg.loc$x
  leg.y <- leg.loc$y

  # create the pieces on the fly
  # position pieces relative to "Key" at top
  grobs_list <- vector("list", ng + 1) # list to store the grobs temporarily
  y.off <- 0.035

  key_grob <- textGrob("Key", x = leg.x, y = leg.y, just = "left",
    gp = gpar(col = "black", fontsize = 10))
  grobs_list[[1]] <- key_grob

  if (!use.sym) {
    for (i in 1:length(group)) {
      leg.y <- leg.y - y.off # descend a bit
      grobs_list[[i + 1]] <- textGrob(group[i], x = leg.x, y = leg.y, just = "left",
        gp = gpar(col = color[i], fontsize = 10))
    }
  }

  # assemble & return
  gt <- gTree(children = do.call(gList, grobs_list))
  return(annotation_custom(gt))
}
