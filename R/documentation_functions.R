#
# Functions to assist with roxygen2 documentation
#
# Not exported or documented
#
#' @author `r .writeAuthors("BH")`
#'
#' @noRd
.writeAuthors <- function(vec) {
  authors <- NA_character_
  if ("BH" %in% vec) authors <- c(authors, "Bryan A. Hanson (DePauw University)")
  if ("TG" %in% vec) authors <- c(authors, "Tejasvi Gupta")
  authors <- na.omit(authors)
  authors <- paste(authors, collapse = ", ")
  authors <- paste(authors, ".", sep = "")
  authors
}

#' @noRd
.writeGraphicsDots <- function() {
  "Parameters to be passed to the plotting routines. *Applies to base graphics only*."
}

#' @noRd
.writeLegLoc <- function() {
"Either a list with elements `x` and `y`, or a string like `'topright'`.  Values in a list should be on `[0,1]`, i.e. the lower left of the plot area is `0,0` and the upper right is `1,1`.  Allowed string values are those described in [graphics::legend()] under 'Details'. A value of `'none'` is acceptable as well."
}

#' @noRd
.writeTol <- function() {
"A number describing the fraction of points to be labeled. `tol = 1.0` labels all the points; `tol = 0.05` labels *approximately* the most extreme 5 percent. Set to `'none'` to completely suppress labels. Note that a simple approach based upon quantiles is used, assumes that both x and y are each normally distributed, and treats x and y separately.  Thus, this is not a formal treatment of outliers, just a means of labeling points. Groups are lumped together for the computation."
}

#' @noRd
.writeGraphicsReturn1 <- function() {
"The returned value depends on the graphics option selected (see [GraphicsOptions()]).
* `base`: None.  Side effect is a plot.
* `ggplot2`: The plot is displayed, and a `ggplot2` object is returned if the value is assigned.  The plot can be modified in the usual `ggplot2` manner.
"
}


