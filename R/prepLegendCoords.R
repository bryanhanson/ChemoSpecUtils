#'
#' Sanitize Legend Coordinates
#'
#' @param go A string specifying the graphics mode.
#' @param leg.loc A list giving x and y coordinates.
#' @param x.min A number giving the minimum x value in the data. Used only if graphics mode is set to base.
#' @param x.max A number giving the maximum x value in the data. Used only if graphics mode is set to base.
#' @param y.min A number giving the minimum y value in the data. Used only if graphics mode is set to base.
#' @param y.max A number giving the maximum y value in the data. Used only if graphics mode is set to base.
#'
#' @return A list of coordinates specifying x and y position of the legend along with a position
#'         designation, except in one case where a string is returned.
#'
#' @author Bryan A. Hanson (DePauw University), Tejasvi Gupta.
#' @export
#' @noRd
#'
.prepLegendCoords <- function(go, leg.loc, x.min = 0.0, x.max = 0.0, y.min = 0.0, y.max = 0.0) {
  lab.x <- NA_real_
  lab.y <- NA_real_
  lab.r <- "cc" # default to centered legend and center justified; may be overidden below

  if (is.list(leg.loc) && go == "ggplot2") { # case when a list is passed in ggplot2 mode
    if (exists("x", where = leg.loc) && (exists("y", where = leg.loc))) {
      leg.loc$r <- "cc" # center justify legend
      return(leg.loc)
    }
  } else if (is.list(leg.loc) && go == "base") { # case when list is passed in base mode
    if (exists("x", where = leg.loc) && (exists("y", where = leg.loc))) {
      # Convert NPC coordinates to native data coordinates
      lab.x <- (leg.loc$x) * (x.max - x.min) + x.min
      lab.y <- (leg.loc$y) * (y.max - y.min) + y.min
      leg.loc <- list(x = lab.x, y = lab.y, r = lab.r)
      return(leg.loc)
    }
  } else if (go == "ggplot2") { # case when a string is passed in ggplot2 mode
    # The positions are "pushed out" to the extremes
    if (leg.loc == "topright") {
      lab.x <- 0.95
      lab.y <- 0.95
      lab.r <- "tr"
    }
    if (leg.loc == "topleft") {
      lab.x <- 0.05
      lab.y <- 0.95
      lab.r <- "tl"
    }
    if (leg.loc == "bottomright") {
      lab.x <- 0.95
      lab.y <- 0.05
      lab.r <- "br"
    }
    if (leg.loc == "bottomleft") {
      lab.x <- 0.05
      lab.y <- 0.05
      lab.r <- "bl"
    }
    if (leg.loc == "bottom") {
      lab.x <- 0.5
      lab.y <- 0.05
      lab.r <- "bc"
    }

    if (leg.loc == "top") {
      lab.x <- 0.5
      lab.y <- 0.95
      lab.r <- "tc"
    }

    if (leg.loc == "left") {
      lab.x <- 0.05
      lab.y <- 0.5
      lab.r <- "cl"
    }

    if (leg.loc == "right") {
      lab.x <- 0.95
      lab.y <- 0.5
      lab.r <- "cr"
    }

    if (leg.loc == "middle") {
      lab.x <- 0.5
      lab.y <- 0.5
      lab.r <- "cc"
    }
    leg.loc <- list(x = lab.x, y = lab.y, r = lab.r)
    return(leg.loc)
  } else { # case when a string is passed in base mode
    if (!is.character(leg.loc)) {
      stop("Expected a character string for leg.loc")
    }
    return(leg.loc)
  }
}
