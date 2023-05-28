#'
#' Process Limits for a Spectra2D Plot
#'
#' *Internal function*. Get limits from a user specified formula
#' (used in [ChemoSpec2D::removeFreq()] and [ChemoSpec2D::removePeaks2D()]).
#' The user may not know or think about whether F2 or F1 is ascending or descending
#' so we will try to get it right no matter how the user gives
#' the formula; e.g. 6 ~ 3 ought to be handled as 3 ~ 6.
#'
#' @param spectra `r .writeDoc_Spectra2()`
#' @param dim The dimension for which the limits are desired.
#' @param form A formula specification.
#'
#' @return `r .writeDoc_Spectra2()`
#'
#' @author `r .writeDoc_Authors("BH")`
#' @export
#' @keywords internal
#'

.getLimits <- function(spectra, dim, form) {
  .chkArgs(mode = 0L)

  lhs <- form[[2]]
  rhs <- form[[3]]

  # capture quoted (SE) or bare variables (NSE): low and "low"
  if (is.symbol(lhs) | is.character(lhs)) {
    if (as.character(lhs) == "low") lhs <- min(spectra[[dim]])
    if (as.character(lhs) == "high") lhs <- max(spectra[[dim]])
  }

  if (is.symbol(rhs) | is.character(rhs)) {
    if (as.character(rhs) == "low") rhs <- min(spectra[[dim]])
    if (as.character(rhs) == "high") rhs <- max(spectra[[dim]])
  }

  # capture negative numeric values, which inexplicably are parsed as language objects
  if (is.language(lhs)) lhs <- as.numeric(deparse(lhs))
  if (is.language(rhs)) rhs <- as.numeric(deparse(rhs))

  ans <- c(lhs, rhs)
  if (is.unsorted(ans)) ans <- rev(ans)
  return(ans) # should always give numeric values in order
}
