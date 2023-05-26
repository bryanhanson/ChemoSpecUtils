#
# Functions to assist with roxygen2 documentation
#
# Not exported or documented
#
#' @noRD
#' @author `r .writeAuthors("BH")`
#'
.writeAuthors <- function(vec) {
  authors <- NA_character_
  if ("BH" %in% vec) authors <- c(authors, "Bryan A. Hanson (DePauw University)")
  if ("TG" %in% vec) authors <- c(authors, "Tejasvi Gupta")
  authors <- na.omit(authors)
  authors <- paste(authors, collapse = ", ")
  authors <- paste(authors, ".", sep = "")
  authors
}