#'  
#' Color in ChemoSpec and ChemoSpec2D
#' 
#' In \code{ChemoSpec} and \code{ChemoSpec2D}, the user may use any color name/format
#' known to R.  The current color scheme of a \code{\link[ChemoSpec]{Spectra}} or
#' \code{\link[ChemoSpec2D]{Spectra2D}} object
#' may be determined using \code{\link{sumGroups}} or \code{\link{sumSpectra}}.
#' The colors can also be queried and changed using \code{\link{conColScheme}}.
#'
#' A really important fact to keep in mind is that most people cannot distinguish more
#' than about eight colors.  If you have more than eight groups, you'll need to plan
#' your color use carefully.  In addition, one may wish to accommodate color-blind individuals.
#' It is possible to choose colors that both normal-vision and color-blind individuals
#' can both see distinctly.  A great discussion of color issues can be found in the 
#' \code{colorspace} package.
#' 
#' Finally, \code{ChemoSpec} but not \code{ChemoSpec2D} can also create plots using
#' the built-in symbols and lower case letters.  This is useful for color-blind individuals,
#' plots in \code{rgl} which can't plot regular symbols, and plots for where there are
#' more groups than could be reasonably coded in color.
#' 
#' @name colorSymbol
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @keywords utilities color
NULL
