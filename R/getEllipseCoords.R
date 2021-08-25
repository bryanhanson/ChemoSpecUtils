#'
#' Process Ellipse Coordinates into a Form Suitable for ggplot2
#'
#' A utility function which returns a data frame that helps in plotting the ellipse in
#' \code{ggplot2} graphics mode. An internal function, not generally called by the user.
#'
#' @param data A list containing the coordinates of the groups whose ellipses are to be plotted.
#'
#' @return A Data Frame, ready to use to plot the ellipses.
#'
#' @author Bryan A. Hanson, DePauw University, Tejasvi Gupta.
#'
#' @keywords utilities
#' @export
#' @noRd
#'

.getEllipseCoords <- function(data) {
  # used in .scorePlot
  df <- data.frame(x = numeric(), y = numeric(), name = character())
  for (i in 1:length(data)) {
    x <- c()
    y <- c()
    name <- c()
    total <- length(data[[i]]) / 2
    for (j in 1:total) {
      x <- c(x, data[[i]][j, 1])
      y <- c(y, data[[i]][j, 2])
    }
    name <- rep(names(data)[i], total)
    temp <- data.frame(x, y, name)
    df <- rbind(df, temp)
  }
  return(df)
}
