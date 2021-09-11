#' Change the Color Scheme of a Spectra or Spectra2D Object
#'
#' This function permits you to change the color scheme of an existing
#' \code{\link[ChemoSpec]{Spectra}} or \code{\link[ChemoSpec2D]{Spectra2D}} object.
#'
#' @param spectra An object of S3 class \code{\link[ChemoSpec]{Spectra}} or
#' \code{\link[ChemoSpec2D]{Spectra2D}}.
#'
#' @param new.cols A character vector giving the new color values, of
#'   \code{length(unique(spectra$colors))}.
#'   If not provided, the function will print the old values for reference.
#'
#' @param silent Logical.  If \code{TRUE}, suppresses all reporting.
#'
#' @return spectra An updated object of S3 class \code{\link[ChemoSpec]{Spectra}} or
#' \code{\link[ChemoSpec2D]{Spectra2D}}\code{\link[ChemoSpec2D]{Spectra2D}}.
#'
#' @seealso For a discussion of general issues of color, see
#' \code{colorSymbol}.
#'
#' @keywords utilities color
#' @template authors-BH
#' @export
#'
#' @examples
#' if (checkForPackageWithVersion("ChemoSpec", 6.0)) {
#'   library("ChemoSpec")
#'   data(metMUD1)
#'
#'   sumSpectra(metMUD1)
#'   newSpec <- conColScheme(metMUD1) # reports old colors
#'   newSpec <- conColScheme(metMUD1, new = c("pink", "violet"))
#' }
#'
#' if (checkForPackageWithVersion("ChemoSpec2D", 0.5)) {
#'   library("ChemoSpec2D")
#'   data(MUD1)
#'
#'   sumSpectra(MUD1)
#'   newSpec <- conColScheme(MUD1) # reports old colors
#'   newSpec <- conColScheme(MUD1, new = c("pink", "violet"))
#' }
conColScheme <- function(spectra, new.cols = NULL, silent = FALSE) {
  .chkArgs(mode = 0L)
  chkSpectra(spectra)
  if (!is.null(new.cols)) {
    if (!is.character(new.cols)) stop("'new.cols' must be a character vector")
  }
  old.cols <- spectra$colors
  old.colsU <- unique(old.cols)
  no.old <- length(old.colsU)

  if (is.null(new.cols)) {
    if (!silent) {
      message("No 'new.cols' found, here are the existing colors:")
      DF <- data.frame(
        grps = unique(as.character(spectra$groups)),
        old = old.colsU,
        stringsAsFactors = FALSE
      )
      print(DF)
    }
    return(invisible(NULL))
  }

  if (length(new.cols) != no.old) {
    stop("Length of 'new.cols' did not match the unique number of existing colors")
  }

  DF <- data.frame(
    grps = unique(as.character(spectra$groups)),
    old = old.colsU,
    new = new.cols,
    stringsAsFactors = FALSE
  )
  updated.cols <- rep(NA_character_, length(old.cols))

  for (i in 1:length(updated.cols)) {
    for (j in 1:nrow(DF)) {
      if (old.cols[i] == DF[j, "old"]) updated.cols[i] <- DF[j, "new"]
    }
  }

  if (!silent) {
    message("Colors were updated as follows:")
    print(DF)
  }

  if (any(is.na(updated.cols))) stop("Failed to match a new color to an old color")
  spectra$colors <- updated.cols
  chkSpectra(spectra)
  return(spectra)
}
