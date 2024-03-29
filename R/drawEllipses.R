#'
#' Add Ellipses to Plot
#'
#' *Internal function*.
#'
#' @param ELL Coordinates of the ellipse points.
#' @param gr Group information.
#' @param ellipse  Type of ellipse; one of `cls`, `rob` or `both`.
#' @param use.sym  Logical.  Should symbols be used?  For this function, determines color of lines.
#' @param \ldots `r .writeDoc_GraphicsDots()`
#' @return None.  Side effect is a modified plot.
#'
#' @author `r .writeDoc_Authors("BH")`
#' @export
#' @keywords internal
#' @importFrom graphics lines
#'

.drawEllipses <- function(ELL, gr, ellipse, use.sym, ...) {

  if (ellipse == "both") {
    .drawEllipses(ELL, gr, ellipse = "cls", use.sym, ...)
    .drawEllipses(ELL, gr, ellipse = "rob", use.sym, ...)
    return()
  }

  # Prepare the coordinates

  if (ellipse == "cls") {
    coords <- lapply(ELL, function(x) { x[1:2] })
    coords <- lapply(coords, function(x) { do.call(cbind, x) })
    ltype <- 3L
  }

  if (ellipse == "rob") {
    coords <- lapply(ELL, function(x) { x[4:5] })
    coords <- lapply(coords, function(x) { do.call(cbind, x) })
    ltype <- 1L
  }

  ellpts <- lapply(coords, function(x) { cbind(x = x[,1], y = x[,2]) })

  # Draw the lines
  # note: hate to loop over list elements, but don't see a way to use
  # lapply and access gr$color
  
  for (i in 1:length(ellpts)) {
    if (use.sym) lines(ellpts[[i]], lty = ltype, col = "black", ...)
    if (!use.sym) lines(ellpts[[i]], lty = ltype, col = gr$color[i], ...)
  }
}
