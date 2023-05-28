#'
#' Find NA in a Spectra2D Object
#'
#' *Internal function*. This function identifies the extent of any NA in a [ChemoSpec2D::Spectra2D()] object.
#' This information can be used for summaries and plots.
#'
#' @param spectra `r .writeDoc_Spectra2()`
#'
#' @param retFreq Logical. Should the frequencies be returned?
#'
#' @return A list with two elements giving the indices of NAs for rows and columns,
#'         unless \code{retFreq = TRUE}, in which case the frequencies are returned.
#'
#' @author `r .writeDoc_Authors("BH")`
#'
#' @keywords internal
#' @importFrom stats na.omit
#' @export
#'
#' @tests tinytest
#' ### Unit tests for .findNA
#' load("tiny2D.RData")
#'
#' tiny2D_NAc <- tiny2D
#' tiny2D_NAc$data[[1]][,4] <- NA # single spectrum with col of NAs
#' tiny2D_NAc$data[[2]][,4] <- NA # more spectra with all NAs in col 4
#' tiny2D_NAc$data[[3]][,4] <- NA 
#' tiny2D_NAr <- tiny2D
#' tiny2D_NAr$data[[1]][3,] <- NA # single spectrum with row of NAs
#' tiny2D_NAr$data[[2]][3,] <- NA # more spectra with all NAs in row 3
#' tiny2D_NAr$data[[3]][3,] <- NA 
#'
#' # .findNA reports col NAs correctly
#' expect_equal(.findNA(tiny2D_NAc)$colNA, 4)
#'
#' # .findNA reports row NAs correctly"
#' expect_equal(.findNA(tiny2D_NAr)$rowNA, 3)
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
