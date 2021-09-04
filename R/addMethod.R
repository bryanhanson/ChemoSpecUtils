#'
#' Add Info About Method to Plot
#'
#' @template authors-BH
#' @export
#' @noRd
#' @importFrom graphics legend
#'
.addMethod <- function(pca) {
  legend("topleft", y = NULL, pca$method, bty = "n", cex = 0.75)
}
