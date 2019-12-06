#'
#' Add Info About Ellipses to Plot
#'
#' @export
#' @noRd
#' @importFrom graphics legend
#' @importFrom plyr dlply llply m_ply
#

.addEllipseInfo <- function(ellipse) {
  if (ellipse == "cls") {
    legend("topleft",
      y = NULL, "classic ellipses by group", lty = 3, bty = "n",
      col = "black", cex = 0.75, inset = c(0, 0.03)
    )
  }

  if (ellipse == "rob") {
    legend("topleft",
      y = NULL, "robust ellipses by group", lty = 1, bty = "n",
      col = "black", cex = 0.75, inset = c(0, 0.03)
    )
  }

  if (ellipse == "both") {
    legend("topleft",
      y = NULL, "classic ellipses by group", lty = 3, bty = "n",
      col = "black", cex = 0.75, inset = c(0, 0.03)
    )
    legend("topleft",
      y = NULL, "robust ellipses by group", lty = 1, bty = "n",
      col = "black", inset = c(0.0, 0.06), cex = 0.75
    )
  }
}
