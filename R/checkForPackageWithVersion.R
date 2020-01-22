#'
#' Check for an Installed Package with a Particular Version or Newer
#'
#' Utility function for making sure a package is available with a particular version or newer.
#'
#' @param pkg Character.  The name of the package to check.
#'
#' @param vers Character. The minimum acceptable version of the package.  Will only be checked
#'        to the major.minor level.
#'
#' @return If successful, \code{TRUE} is return invisibly.  Stops if there is a problem.
#'
#' @export
#'
checkForPackageWithVersion <- function(pkg, vers) {
  if (!is.character(pkg)) stop("pkg must be a character string")
  if (!is.character(vers)) stop("vers must be a character string")
  msg <- paste("You must install package", pkg,
    "with version", vers, "or newer to use this function",
    sep = " "
  )

  # Helper function
  # We will only check major.minor versions and ignore anything beyond that
  # e.g. for 0.3.55 we will keep 0.3, for 0.99-20180627 we will keep 0.99
  mmVers <- function(string) {
    mm <- sub("-.+", "", string) # remove anything beyond a "-"
    mm <- sub("([0-9]+\\.[0-9]+)\\..*", "\\1", mm)
    as.numeric(mm)
  }
  # tests:
  # mmVers("0.3.55")
  # mmVers("0.99-20180627")

  # Check to see if *any* version of the package is installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(msg)
  }

  # Check to see if the existing version matches vers or newer
  if (requireNamespace(pkg, quietly = TRUE)) {
    installedVers <- mmVers(getNamespaceVersion(pkg))
    if (installedVers < mmVers(vers)) {
      stop(msg)
    }
  }

  invisible(TRUE)
}
