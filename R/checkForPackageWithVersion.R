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
#' @return If the package with the required version or higher is available, \code{TRUE} is returned
#'         invisibly.  Otherwise \code{FALSE} is returned.
#'
#' @template authors-BH
#' @export
#'
#' @tests tinytest
#'
#' # helper function
#' mmVers <- function(string) {
#'   mm <- sub("-.+", "", string) # remove anything beyond a "-"
#'   mm <- sub("([0-9]+\\.[0-9]+)\\..*", "\\1", mm)
#'   as.numeric(mm)
#' }
#'
#' expect_equal(mmVers(mmVers("0.3.55")), 0.3)
#' expect_equal(mmVers(mmVers("0.99-20180627")), 0.99)
#'
#' # get the installed version of pkg guaranteed to be available
#' ivers <- mmVers(getNamespaceVersion("utils"))
#' expect_true(checkForPackageWithVersion("utils", ivers - 0.1))
#' expect_true(checkForPackageWithVersion("utils", ivers))
#' expect_false(checkForPackageWithVersion("utils", ivers + 0.1))
#'
checkForPackageWithVersion <- function(pkg, vers) {
  if (!is.character(pkg)) stop("pkg must be a character string")
  vers <- as.character(vers)
  ans <- NULL

  # Helper function
  # We will only check major.minor versions and ignore anything beyond that
  # e.g. for 0.3.55 we will keep 0.3, for 0.99-20180627 we will keep 0.99
  mmVers <- function(string) {
    mm <- sub("-.+", "", string) # remove anything beyond a "-"
    mm <- sub("([0-9]+\\.[0-9]+)\\..*", "\\1", mm)
    as.numeric(mm)
  }

  # Check to see if *any* version of the package is installed
  inst <- requireNamespace(pkg, quietly = TRUE)
  if (!inst) ans <- FALSE
 
  # Check to see if the installed version matches vers or newer
  if (inst) {
    installedVers <- mmVers(getNamespaceVersion(pkg))
    good <- installedVers >= mmVers(vers)
    if (good) ans <- TRUE
    if (!good) ans <- FALSE
  }

  if (is.null(ans)) stop("Could not determine if package '", pkg, "' is installed")
  invisible(ans)
}
