#'
#' Color in ChemoSpec and ChemoSpec2D
#'
#' In \code{ChemoSpec} and \code{ChemoSpec2D}, the user may use any color name/format
#' known to R.  The current color scheme of a \code{\link[ChemoSpec]{Spectra}} or
#' \code{\link[ChemoSpec2D]{Spectra2D}} object
#' may be determined using \code{\link{sumGroups}} or \code{\link{sumSpectra}}.
#' The colors can also be queried and changed using \code{\link{conColScheme}}.
#'
#' An important fact to keep in mind is that most people with normal vision cannot distinguish
#' more than about 8-12 colors, and doing so depends upon the viewing circumstances:
#' if on paper, printer, ink and paper type all matter, and if on a screen, the background color
#' makes a big difference.  Further, color-blind individuals have additional challenges.
#' A great discussion of color issues can be found in the \code{colorspace} package.  The
#' \code{Polychrome} package has further discussion and utilities for choosing qualitative
#' colorschemes, including those for color-blind individuals.
#'
#' \code{ChemoSpec}, but not \code{ChemoSpec2D}, can also create plots using
#' the built-in symbols and lower case letters.  This is useful for color-blind individuals,
#' plots in \code{rgl} which can't plot regular symbols, and plots for where there are
#' more groups than could be reasonably coded in color.  A good discussion of which symbols
#' are most readily distinguished can be found in Robinson: "Good Plot Symbols by Default"
#' \emph{Journal of Computational and Graphical Statistics} DOI: 10.1080/10618600.2019.1637746
#'
#' \code{ChemoSpecUtils} supplies four color/symbol schemes for your consideration.
#' If the particular order of colors in any of these does not suit your needs, you can always
#' choose the ones you want, and/or rearrange the order, or simply provide your own.
#' \itemize{
#'   \item The colors and symbols produced by \code{gr.cols = "auto"} in the import functions.
#'   \item \code{Col8} provides eight unique colors.  These are more saturated
#'         than the automatic colors.
#'   \item \code{Col12} provides a mostly paired set of 12 unique colors suitable for groups
#'         that come in pairs.
#'   \item \code{Col7} provides seven color-blind friendly colors.  These can be visualized
#'         at https://projects.susielu.com/viz-palette by using the hex codes obtained
#'         by typing \code{data(Col7); Col7} in the \code{R} console. 
#' }
#'
#' @docType data
#'
#' @format Colors are stored as character vectors and symbols as numeric vectors.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities color datasets
#'
#' @name colorSymbol
#' @aliases Col7 Col12 Sym12 Col8 Sym8 ColorScheme
#'
#' @examples
#'
#' # Make a plot showing all the built-in color options
#'
#' data(Col7)
#' data(Col12)
#' data(Sym12)
#' data(Col8)
#' data(Sym8)
#' auto <- RColorBrewer::brewer.pal(8, "Set1")
#'
#' sp <- 0.75 # space between major plot elements
#' tsp <- 0.15 # additional space between points and color swatches/descriptive text
#' h <- 0.25 # height of the swatch
#' y <- 0.0 # bottom of the plot, the reference point
#'
#' # empty plot
#' plot(1:12, rep(0.0, 12),
#'   type = "n", yaxt = "n", xaxt = "n", bty = "n",
#'   xlab = "", ylab = "", ylim = c(0, 3.5)
#' )
#' text(6.5, y + h + tsp * 4 + sp * 3.5,
#'   labels = "Automatic Color & Symbol Options", cex = 1.25, font = 2
#' )
#'
#' # Col12
#' for (i in 1:12) {
#'   rect(i - 0.5, y, i + 0.5, y + h, border = NA, col = Col12[i])
#' }
#' points(1:12, rep(y + h + tsp, 12), pch = Sym12)
#' text(0.6, y + h + tsp * 2, adj = 0,
#'   labels = "gr.cols = 'Col12'     12 mostly paired distinct colors/symbols"
#' )
#'
#' # Col8
#' for (i in 1:8) {
#'   rect(i - 0.5, y + sp, i + 0.5, y + sp + h, border = NA, col = Col8[i])
#' }
#' points(1:8, rep(y + h + tsp + sp, 8), pch = Sym8)
#' text(0.6, y + h + tsp * 2 + sp, adj = 0,
#'   labels = "gr.cols = 'Col8'     8 distinct colors/symbols"
#' )
#'
#' # auto (original)
#' for (i in 1:8) {
#'   rect(i - 0.5, y + sp * 2, i + 0.5, y + sp * 2 + h, border = NA, col = auto[i])
#' }
#' points(1:8, rep(y + h + tsp + sp * 2, 8), pch = Sym8)
#' text(0.6, y + h + tsp * 2 + sp * 2, adj = 0,
#'   labels = "gr.cols = 'auto'     8 distinct colors/symbols"
#' )
#'
#' # colorblind-friendly
#' for (i in 1:7) {
#'   rect(i - 0.5, y + sp * 3, i + 0.5, y + sp * 3 + h, border = NA, col = Col7[i])
#' }
#' points(1:7, rep(y + h + tsp + sp * 3, 7), pch = Sym8[1:7])
#' text(0.6, y + h + tsp * 2 + sp * 3, adj = 0,
#'   labels = "gr.cols = 'Col7'     7 colorblind-friendly colors"
#' )
NULL
