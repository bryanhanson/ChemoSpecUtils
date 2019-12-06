#'
#' Update Group Names in a Spectra or Spectra2D Object
#'
#' A convenience function that can be used to update (change)
#' group names.  The default group names come from the \code{gr.crit}
#' argument in the import functions \code{\link[ChemoSpec]{files2SpectraObject}},
#' \code{\link[ChemoSpec]{matrix2SpectraObject}} or
#' \code{\link[ChemoSpec2D]{files2Spectra2DObject}}. In some cases \code{gr.crit}
#' may have complex regex patterns, and this function makes updating them to more
#' appropriate/more readible strings easier.
#'
#' @param spectra An object of S3 class \code{\link[ChemoSpec]{Spectra}} or
#' \code{\link[ChemoSpec2D]{Spectra2D}}.
#'
#' @param new.grps A vector of character values giving the new group names.
#'   The new values must correspond to the order of the old values.  This vector should
#'   give the unique values only (so, it should have \code{length(unique(spectra$groups))}).
#'   If not provided, the function will print the old values for reference.
#'
#' @param silent Logical.  If \code{TRUE}, suppresses all reporting.
#'
#' @return spectra An updated object of S3 class \code{\link[ChemoSpec]{Spectra}} or
#' \code{\link[ChemoSpec2D]{Spectra2D}}.
#'
#' @export
#'
#' @examples
#' if (checkForPackageWithVersion("ChemoSpec", "5.1")) {
#'   library("ChemoSpec")
#'   data(metMUD1)
#'   metMUD1a <- updateGroups(metMUD1) # reports old groups
#'   metMUD1a <- updateGroups(metMUD1, new.grps = c("C", "T"))
#' }
#'
#' if (checkForPackageWithVersion("ChemoSpec2D", "0.3")) {
#'   library("ChemoSpec2D")
#'   data(MUD1)
#'   MUD1a <- updateGroups(MUD1, new.grps = c("control", "treatment"))
#' }
updateGroups <- function(spectra, new.grps = NULL, silent = FALSE) {
  .chkArgs(mode = 0L)
  chkSpectra(spectra)
  if (!is.null(new.grps)) {
    if (!is.character(new.grps)) stop("'new.grps' must be a character vector")
  }
  old.grps <- as.character(spectra$groups)
  old.grpsU <- unique(old.grps)
  no.old <- length(old.grpsU)
  if (is.null(new.grps)) {
    if (!silent) {
      message("No 'new.grps' found, here are the old groups:")
      print(old.grpsU)
    }
    return(invisible(NULL))
  }

  if (length(new.grps) != no.old) {
    stop("Length of 'new.grps' did not match the unique number of existing groups")
  }

  DF <- data.frame(old = old.grpsU, new = new.grps, stringsAsFactors = FALSE)
  updated.grps <- rep(NA_character_, length(old.grps))

  for (i in 1:length(updated.grps)) {
    for (j in 1:nrow(DF)) {
      if (old.grps[i] == DF[j, "old"]) updated.grps[i] <- DF[j, "new"]
    }
  }

  if (!silent) {
    message("Groups were matched as follows:")
    print(DF)
  }

  if (any(is.na(updated.grps))) stop("Failed to match a new to an old group")
  spectra$groups <- as.factor(updated.grps)
  chkSpectra(spectra)
  return(spectra)
}
