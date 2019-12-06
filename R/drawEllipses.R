#'
#' Add Ellipses to Plot
#'
#' @export
#' @noRd
#' @importFrom graphics lines
#' @importFrom plyr dlply llply m_ply
#

.drawEllipses <- function(ELL, gr, ellipse, use.sym, ...) {
  if (ellipse == "cls") {
    cls.coords <- llply(ELL, function(x) {
      x[1:2]
    })
    cls.coords <- llply(cls.coords, function(x) {
      do.call(cbind, x)
    })
    if (!use.sym) m_ply(cbind(x = cls.coords, col = gr$color, lty = 3), lines, ...)
    if (use.sym) m_ply(cbind(x = cls.coords, col = "black", lty = 3), lines, ...)
  }

  if (ellipse == "rob") {
    rob.coords <- llply(ELL, function(x) {
      x[4:5]
    })
    rob.coords <- llply(rob.coords, function(x) {
      do.call(cbind, x)
    })
    if (!use.sym) m_ply(cbind(x = rob.coords, col = gr$color), lines, ...)
    if (use.sym) m_ply(cbind(x = rob.coords, col = "black"), lines, ...)
  }

  if (ellipse == "both") {
    .drawEllipses(ELL, gr, ellipse = "cls", use.sym, ...)
    .drawEllipses(ELL, gr, ellipse = "rob", use.sym, ...)
  }
}
