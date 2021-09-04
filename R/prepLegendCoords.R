#'
#' Add Points to a Plot
#'
#' @param go A string specifying the graphics mode.
#' @param leg.loc A list giving x and y coordinates.
#' @param x.min A number giving the minimum x value in the data. Used only if graphics mode is set to base.
#' @param x.max A number giving the maximum x value in the data. Used only if graphics mode is set to base.
#' @param y.min A number giving the minimum y value in the data. Used only if graphics mode is set to base.
#' @param y.max A number giving the maximum y value in the data. Used only if graphics mode is set to base.
#'
#' @return A list of coordinates specifying x and y position of the legend, except in one case where a string is returned.
#'
#' @author Bryan A. Hanson (DePauw University), Tejasvi Gupta.
#' @export
#' @noRd
#'
.prepLegendCoords <- function(go, leg.loc, x.min = 0.0, x.max = 0.0, y.min = 0.0, y.max = 0.0) {
  lab.x <- c(NA_real_)
  lab.y <- c(NA_real_)
  if (is.list(leg.loc) && go == "ggplot2") { # case when a list is passed in ggplot2 mode
    if (exists("x", where = leg.loc) && (exists("y", where = leg.loc))) {
      return(leg.loc)
    }
  } else if (is.list(leg.loc) && go == "base") { # case when list is passed in base mode
    if (exists("x", where = leg.loc) && (exists("y", where = leg.loc))) {
      # Convert NPC coordinates to native data coordinates
      lab.x <- (leg.loc$x) * (x.max - x.min) + x.min
      lab.y <- (leg.loc$y) * (y.max - y.min) + y.min
      leg.loc <- list(x = lab.x, y = lab.y)
      return(leg.loc)
    }
  } else if (go == "ggplot2") { # case when a string is passed in ggplot2 mode
    if (leg.loc == "topright") {
      lab.x <- 0.9
      lab.y <- 0.9
    }
    if (leg.loc == "topleft") {
      lab.x <- 0.01
      lab.y <- 0.9
    }
    if (leg.loc == "bottomright") {
      lab.x <- 0.9
      lab.y <- 0.2
    }
    if (leg.loc == "bottomleft") {
      lab.x <- 0.01
      lab.y <- 0.2
    }
    if (leg.loc == "bottom") {
      lab.x <- 0.45
      lab.y <- 0.2
    }

    if (leg.loc == "top") {
      lab.x <- 0.45
      lab.y <- 0.9
    }

    if (leg.loc == "left") {
      lab.x <- 0.01
      lab.y <- 0.5
    }

    if (leg.loc == "right") {
      lab.x <- 0.9
      lab.y <- 0.5
    }

    if (leg.loc == "middle") {
      lab.x <- 0.45
      lab.y <- 0.5
    }
    leg.loc <- list(x = lab.x, y = lab.y)
    return(leg.loc)
  } else { # case when a string is passed in base mode
    if (!is.character(leg.loc)) {
      stop("Expected a character string for leg.loc")
    }
    return(leg.loc)
  }
}
