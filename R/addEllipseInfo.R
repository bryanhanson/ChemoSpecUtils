#'
#' Add Info About Ellipses to Plot
#'
#' *Internal* function.
#'
#' @param ellipse String giving the type of ellipse; one of `cls`, `rob` or `both`.
#' @return None. Side effect is a modified plot.
#'
#' @author `r .writeDoc_Authors("BH")`
#' @export
#' @importFrom graphics legend
#' @keywords internal
#'

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
