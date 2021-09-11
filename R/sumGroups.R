#'
#' Summarize the Group Membership of a Spectra or Spectra2D Object
#'
#' This function summarizes the group membership	 of
#' a \code{Spectra} or \code{\link[ChemoSpec2D]{Spectra2D}} object.
#'
#' @param spectra An object of S3 class \code{\link[ChemoSpec]{Spectra}} or
#' \code{\link[ChemoSpec2D]{Spectra2D}} whose group membership information is desired.
#'
#' @return A data frame as follows. Note that if there are groups with no
#' members these are dropped.
#'  \item{group}{The name of the group.}
#'  \item{no.}{The number in the group.}
#'  \item{color}{The color assigned to the group.}
#'  \item{symbol}{The symbol assigned to the group. \code{Spectra} objects only.}
#'  \item{alt.symbol}{The alternative symbol assigned to the group. \code{Spectra} objects only.}
#'
#' @template authors-BH
#'
#' @seealso To summarize the entire object, \code{\link{sumSpectra}}.
#'
#' @keywords utilities
#' @export
#'
#' @examples
#' if (checkForPackageWithVersion("ChemoSpec", 6.0)) {
#'   library("ChemoSpec")
#'   data(SrE.IR)
#'   sumGroups(SrE.IR)
#' }
#'
#' if (checkForPackageWithVersion("ChemoSpec2D", 0.5)) {
#'   library("ChemoSpec2D")
#'   data(MUD1)
#'   sumGroups(MUD1)
#' }
sumGroups <- function(spectra) {
  UseMethod("sumGroups")
}
