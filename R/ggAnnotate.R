#'
#' Annotate a ggplot2 Plot with Text
#'
#' *Internal function*. General purpose function to add text to a ggplot2 plot.  Uses
#' \code{textGrob} so arguments like \code{hjust} can be passed via ...
#'
#' @param text Character.  The text to be plotted, but certain values are intercepted
#'        for special handling.
#' @param x Numeric. The x coordinate for the text to be plotted, in NPC.
#' @param y Numeric. The y coordinate for the text to be plotted, in NPC.
#' @param \dots Additional arguments to be passed to \code{textGrob}.
#'
#' @return A custom annotation grob, ready to be added to an existing plot.
#'
#' @author `r .writeDoc_Authors(c("BH", "TG"))`
#' @export
#' @keywords internal
#' @importFrom grid textGrob linesGrob gTree gList grobTree
#' @importFrom ggplot2 annotation_custom
#'
.ggAnnotate <- function(text, x, y, ...) {
  # Capture special, more complex cases
  if (text == "cls") {
    cls_seg_grob <- linesGrob(x = c(0.05, 0.12), y = c(0.95, 0.95), gp = gpar(lty = 2))
    cls_txt_grob <- textGrob("classical ellipses by group", x = 0.14, y = 0.95, just = "left", gp = gpar(fontsize = 8))
    gt <- gTree(children = gList(cls_seg_grob, cls_txt_grob))
    return(annotation_custom(gt))
  }

  if (text == "rob") {
    rob_seg_grob <- linesGrob(x = c(0.05, 0.12), y = c(0.95, 0.95), gp = gpar(lty = 1)) 
    rob_txt_grob <- textGrob("robust ellipses by group", x = 0.14, y = 0.95, just = "left", gp = gpar(fontsize = 8))
    gt <- gTree(children = gList(rob_seg_grob, rob_txt_grob))
    return(annotation_custom(gt))
  }

  if (text == "both") {
    cls_seg_grob <- linesGrob(x = c(0.05, 0.12), y = c(0.95, 0.95), gp = gpar(lty = 2)) 
    cls_txt_grob <- textGrob("classical ellipses by group", x = 0.14, y = 0.95, just = "left", gp = gpar(fontsize = 8))
    rob_seg_grob <- linesGrob(x = c(0.05, 0.12), y = c(0.92, 0.92), gp = gpar(lty = 1)) 
    rob_txt_grob <- textGrob("robust ellipses by group", x = 0.14, y = 0.92, just = "left", gp = gpar(fontsize = 8))
    gt <- gTree(children = gList(cls_seg_grob, cls_txt_grob, rob_seg_grob, rob_txt_grob))
    return(annotation_custom(gt))
  }

  # Otherwise just a plain 'ol text annotation
  annotation <- grobTree(textGrob(text, x, y, ...))
  annotation_custom(annotation)
}

   
