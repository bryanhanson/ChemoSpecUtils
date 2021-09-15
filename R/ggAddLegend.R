#'
#' Add Legend to a ggplot2 Plot
#'
#' This function creates a suitable legend and returns it as a set of grobs,
#' ready to be added to an existing plot.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#' @param use.sym Logical specifying if symbols will be used or not.
#' @param leg.loc A list giving x and y coordinates.
#'
#' @return A ggplot with custom legend.
#'
#' @author Bryan A. Hanson (DePauw University), Tejasvi Gupta.
#' @export
#' @noRd
#' @importFrom grid textGrob gpar gTree gList pointsGrob unit
#'
.ggAddLegend <- function(spectra, use.sym, leg.loc) {

  # get the needed data
  gr_sum <- sumGroups(spectra) # group, color, symbol
  group <- gr_sum$group
  color <- gr_sum$color
  symbol <- gr_sum$symbol
  ng <- length(group)

  # figure out legend coords; reference is top left
  leg.loc <- .prepLegendCoords("ggplot2", leg.loc)
  leg.x <- leg.loc$x
  leg.y <- leg.loc$y

  # create the pieces on the fly
  # position pieces relative to "Key" at top
  if (!use.sym) grobs_list <- vector("list", ng + 1) # group name + title
  if (use.sym) grobs_list <- vector("list", ng*2 + 1) # group symbol, group name + title
  y.off <- 0.035
  x.off <- 0.025

  key_grob <- textGrob("Key", x = leg.x, y = leg.y, just = "left",
    gp = gpar(col = "black", fontsize = 8))
  grobs_list[[1]] <- key_grob

  if (!use.sym) {
    for (i in 1:ng) {
      leg.y <- leg.y - y.off # descend a bit
      grobs_list[[i + 1]] <- textGrob(group[i], x = leg.x, y = leg.y, just = "left",
        gp = gpar(col = color[i], fontsize = 8))
    }
  }

  if (use.sym) {
    color <- rep("black", ng)
    for (i in 1:ng) {
      leg.y <- leg.y - y.off # descend a bit
      grobs_list[[i + 1]] <- textGrob(group[i], x = leg.x + x.off, y = leg.y, just = "left",
        gp = gpar(col = color[i], fontsize = 8))
      grobs_list[[i + 1 + ng]] <- pointsGrob(symbol[i], x = leg.x , y = leg.y, size = unit(0.5, "char"))
    }
  }

  # assemble & return
  gt <- gTree(children = do.call(gList, grobs_list))
  return(annotation_custom(gt))
}
