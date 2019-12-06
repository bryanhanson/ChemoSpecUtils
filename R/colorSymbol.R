#'  
#' Color in ChemoSpec and ChemoSpec2D
#' 
#' In \code{ChemoSpec} and \code{ChemoSpec2D}, the user may use any color name/format
#' known to R.  The current color scheme of a \code{\link[ChemoSpec]{Spectra}} or
#' \code{\link[ChemoSpec2D]{Spectra2D}} object
#' may be determined using \code{\link{sumGroups}} or \code{\link{sumSpectra}}.
#' The colors can also be queried and changed using \code{\link{conColScheme}}.
#'
#' A really important fact to keep in mind is that most people with normal vision cannot distinguish
#' more than about 8-12 colors, and doing so depends upon the viewing circumstances:
#' if on paper, printer, ink and paper type all matter, and if on a screen, the background color
#' makes a big difference.  Further, color-blind individuals have additional challenges.
#' A great discussion of color issues can be found in the \code{colorspace} package.
#' 
#' Finally, \code{ChemoSpec}, but not \code{ChemoSpec2D}, can also create plots using
#' the built-in symbols and lower case letters.  This is useful for color-blind individuals,
#' plots in \code{rgl} which can't plot regular symbols, and plots for where there are
#' more groups than could be reasonably coded in color.  A good discussion of which symbols
#' are most readily distinguished can be found in Robinson: "Good Plot Symbols by Default",
#' \emph{Journal of Computational and Graphical Statistics} DOI: 10.1080/10618600.2019.1637746 
#' 
#' 
#' @name colorSymbol
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @keywords utilities color
NULL