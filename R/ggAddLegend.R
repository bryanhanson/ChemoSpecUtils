#'
#' Add Legend to a ggplot2 Plot
#'
#' This function creates a suitable legend and returns it as a set of grobs,
#' ready to be added to an existing plot.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#' @param use.sym Logical specifying if symbols will be used or not.
#' @param leg.loc A list giving x, y coordinates, and a reference point.
#'
#' @return A grob containing the legend.
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
  if (use.sym) color <- rep("black", length(color))
  symbol <- gr_sum$symbol
  ng <- length(group)

  # figure out legend coords
  leg.loc <- .prepLegendCoords("ggplot2", leg.loc)
  leg.x <- leg.loc$x
  leg.y <- leg.loc$y
  leg.r <- leg.loc$r
 
  # Determine justification needed; get 2nd letter of leg.r
  leg.j <- substr(leg.r, 2, 2)
  if (leg.j == "l") leg.j <- "left"
  if (leg.j == "c") leg.j <- "center"
  if (leg.j == "r") leg.j <- "right"

  # Set up structures to hold the grobs
  if (!use.sym) grobs_list <- vector("list", ng + 1) # group name + title
  if (use.sym) grobs_list <- vector("list", ng*2 + 1) # group symbol, group name + title

  # Set up spacing
  y.off <- 0.030 # vertical spacing between rows
  x.off <- 0.025 # horizontal spacing between symbol and text

  # 3 cases depending upon whether the legend is at top, center or bottom
  # In all cases the legend is drawn from top to bottom; leg.y is adjusted
  # if legend is in the center or bottom

  # Adjust leg.y as needed
  # increase leg.y by half the total height of the legend if legend centered vertically
  if (leg.r %in% c("cl", "cc", "cr")) leg.y <- leg.y + 0.5*(ng * y.off)
  # increase leg.y by the total height of the legend if legend is at the bottom
  if (leg.r %in% c("bl", "bc", "br")) leg.y <- leg.y + ng * y.off
  
  # Build up legend from the top down
  key_grob <- textGrob("Key", x = leg.x, y = leg.y, just = leg.j,
    gp = gpar(col = "black", fontsize = 8))
  grobs_list[[1]] <- key_grob
  for (i in 1:ng) {
    leg.y <- leg.y - y.off # descend a bit
    if (use.sym) {
      if (leg.r %in% c("tl", "cl", "bl")) { # legends on the left
        grobs_list[[i + 1]] <- textGrob(group[i], x = leg.x + x.off, y = leg.y,
          just = leg.j, gp = gpar(col = color[i], fontsize = 8))
        grobs_list[[i + 1 + ng]] <- pointsGrob(symbol[i], x = leg.x, y = leg.y,
          size = unit(0.5, "char"))
      }
      if (leg.r %in% c("tc", "cc", "bc")) { # legends in the center
        grobs_list[[i + 1]] <- textGrob(group[i], x = leg.x - 0.2*x.off, y = leg.y,
          just = "right", gp = gpar(col = color[i], fontsize = 8))
        grobs_list[[i + 1 + ng]] <- pointsGrob(symbol[i], x = leg.x + 0.8*x.off, y = leg.y,
          size = unit(0.5, "char"))
      }
      if (leg.r %in% c("tr", "cr", "br")) { # legends on the right
        grobs_list[[i + 1]] <- textGrob(group[i], x = leg.x - x.off, y = leg.y,
          just = leg.j, gp = gpar(col = color[i], fontsize = 8))
        grobs_list[[i + 1 + ng]] <- pointsGrob(symbol[i], x = leg.x, y = leg.y,
          size = unit(0.5, "char"))
      }
    }
    if (!use.sym) {
      grobs_list[[i + 1]] <- textGrob(group[i], x = leg.x, y = leg.y, just = leg.j,
        gp = gpar(col = color[i], fontsize = 8))
    }
  }

  # assemble & return
  gt <- gTree(children = do.call(gList, grobs_list))
  return(annotation_custom(gt))
}
