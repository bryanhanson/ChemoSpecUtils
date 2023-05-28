#'
#' Add Info About Data Reduction Method to Plot
#'
#' *Internal* function.
#'
#' @param pca An object of `prcomp` etc with a `method` slot.
#' @return None.  Side effect is a modifed plot.
#'
#' @author `r .writeDoc_Authors("BH")`
#' @export
#' @importFrom graphics legend
#' @keywords internal
#'
.addMethod <- function(pca) {
  legend("topleft", y = NULL, pca$method, bty = "n", cex = 0.75)
}
