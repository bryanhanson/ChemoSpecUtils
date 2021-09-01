#'
#' Wrapper for geom_text_repel
#'
#' Wrapper for \code{geom_text_repel} to make customization, modifcation
#' and maintenance easy.
#'
#' @param df Data frame, containing elements \code{x}, \code{y} and \code{label}.
#' @return A geom that can be added to an existing plot.
#'
#' @importFrom ggrepel geom_text_repel
#' @export
#' @noRd
#'
.ggRepel <- function(df) {
  x <- y <- label <- NULL # satisfy CRAN check
  geom_text_repel(data = df, aes(x = x, y = y, label = label),
                       box.padding = 0.5, max.overlaps = Inf,
                       segment.color = "gray50")
}       
