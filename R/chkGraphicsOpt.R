#'
#' Checks the Graphic Output Option/Mode
#'
#' @param silent Logical.  Silences most messages if \code{TRUE}.
#'
#' @return Character.  The value of the current graphics output option/mode.
#'
#' @author Bryan A. Hanson, DePauw University, Tejasvi Gupta.
#'
#' @seealso See \code{\link[ChemoSpec]{GraphicsOptions}} for more information about the graphics options.
#'
#' @keywords utilities
#'
#' @export
#'
#' @tests tinytest
#'
#' # check for 'base'
#' options(ChemoSpecGraphics = "base")
#' expect_equal(chkGraphicsOpt(), "base")
#'
#' # check for 'ggplot2'
#' options(ChemoSpecGraphics = "ggplot2")
#' expect_equal(chkGraphicsOpt(), "ggplot2")
#'
#' # check for invalid mode
#' options(ChemoSpecGraphics = "xyz")
#' expect_equal(chkGraphicsOpt(), "base")

chkGraphicsOpt <- function(silent = TRUE) {
  go <- getOption("ChemoSpecGraphics")
  valid <- c("base", "ggplot2", "plotly")

  flag <- 0
  if (!(go %in% valid)) {
    go <- "base"
    flag <- 1
    options(ChemoSpecGraphics = "base")
    message("An invalid option is found! \nThe ChemoSpec graphics option has been set to 'base' ")
  }
  
  if (!silent) {
    if (go == "base" && flag == 0) {
      message("\nThe ChemoSpec graphics option is set to 'base'")
    }

    if (go == "ggplot2") {
      message("\nThe ChemoSpec graphics option is set to 'ggplot2'")
    }

    if (go == "plotly") {
      message("\nThe ChemoSpec graphics option is set to 'plotly'")
    }
  }
  return(go)
}
