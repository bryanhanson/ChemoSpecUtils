#'
#' Find NA in a Spectra2D Object
#'
#' This function identifies the extent of any NA in a \code{\link{Spectra2D}} object.
#' This information can be used for summaries and plots.  Not intended to be called
#' by the user.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param retFreq Logical. Should the frequencies be returned?
#'
#' @return A list with two elements giving the indices of NAs for rows and columns,
#'         unless \code{retFreq = TRUE}, in which case the frequencies are returned.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities
#' @noRd
#' @importFrom stats na.omit
#' @export
#'

.findNA <- function(spectra, retFreq = FALSE) {
  .chkArgs(mode = 0L)

  M <- spectra$data[[1]] # All spectra are assumed to have the same set of NAs
  # This is verified by chkSpectra2D

  # Find rows (columns) that are all NA

  # Check along F2 for columns that are all NA (displayed as vertical lines in plotSpectra2D)
  # As spectra$F2 is sorted ascending, but plotted descending,
  # we must reverse F2 if we want correct frequencies for plotting.
  # No such fix is needed for F1.
  cNA <- rep(NA_integer_, ncol(M))
  for (i in 1:ncol(M)) {
    if (all(is.na(M[, i]))) cNA[i] <- i
  }
  cNA <- as.integer(na.omit(cNA))
  if (retFreq) cNA <- rev(spectra$F2)[cNA]

  # Check along F1 for rows that are all NA (displayed as horizontal lines in plotSpectra2D)
  rNA <- rep(NA_integer_, nrow(M))
  for (i in 1:nrow(M)) {
    if (all(is.na(M[i, ]))) rNA[i] <- i
  }
  rNA <- as.integer(na.omit(rNA))
  if (retFreq) rNA <- spectra$F1[rNA]

  return(list(rowNA = rNA, colNA = cNA))
}
