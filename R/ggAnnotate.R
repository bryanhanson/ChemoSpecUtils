#'
#' Annotate a ggplot2 Plot with Text
#'
#' General purpose function to add text to a ggplot2 plot.  Uses
#' \code{textGrob} so arguments like \code{hjust} can be passed via ...
#'
#' @param text Character.  The text to be plotted.
#' @param x Numeric. The x coordinate for the text to be plotted, in NPC.
#' @param y Numeric. The y coordinate for the text to be plotted, in NPC.
#' @param \dots Additional arguments to be passed to \code{textGrob}.
#'
#' @return A custom annotation grob, ready to be added to an existing plot.
#'
#' @export
#' @noRd
#' @importFrom grid textGrob
#' @importFrom ggplot2 annotation_custom
#'
.ggAnnotate <- function(text, x, y, ...) {
  annotation <- grobTree(textGrob(text, x, y, ...))
  annotation_custom(annotation)
}