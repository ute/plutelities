# splot - stylist plot

#'@title Plot with styles: generic function
#'@description Plot using predefined lists of plot parameters. 
#'Generic template method for use by other packages.
#'@param x the object that is to be plottet.
#'@param ... parameters passed to class methods.
#'@details Defining a \code{splot} method is 
#'\itemize{
#' \item essential for classes with \code{plot} methods that internally call \code{splot},
#' since otherwise a call to \code{splot.default} risks to end up in a loop,
#'\item recommended for classes with plot methods that use \code{\link{simplist}} to coerce
#'the arguments.
#'}
#'In these cases, \code{splot} is virtually an alias for \code{plot}.
#'@export
#'@seealso \code{\link{splot.default}} for the default method, and examples.

splot <- function(x, ...) UseMethod("splot")


#'@title Plot with styles
#'@description Plot using predefined parameter lists
#'@param x an object that belongs to a class with \code{plot} method.
#'@param ... plot parameters, may be wrapped in \code{\link{simplist}}s.
#'@param .NULL.rm logical. If \code{TRUE}, ignore parameters with value \code{NULL}.
#'@param .plotmethod character naming a function, defaults to \code{"plot"}.
#'@details The function calls the \code{plot}-method of \code{x}, with arguments
#' given in \code{...}. Any \code{\link{simplist}}s contained in the arguments are
#' unwrapped. If arguments with the same name appear in the list, the last one
#' is taken.
#'
#' ! Note: Don't use this function inside a plot method, or any other method that
#' might be called with \code{splot} !
#'@author Ute Hahn,  \email{ute@@imf.au.dk}
#'@S3method splot default
#'@method splot default
#'@export
#'@examples
#'boldly <- simplist(col = "red", lwd = 5)
#'timidly <- simplist(col = "gray", lwd = 0.5, lty = "dashed")
#'
#'splot(sin, boldly, to = pi, ylim = c(-1,1))
#'splot(cos, timidly, add = TRUE, to = pi)
#'
#'# the last one wins:
#'splot(sin, boldly, timidly)
#'splot(sin, timidly, boldly, col = "green")

splot.default <- function(x, ..., .NULL.rm = TRUE, .plotmethod = "plot") {
  if (length(list(...)) == 0)
    do.call(.plotmethod, list(x))
  else{
    plotargs <- simplist(..., .NULL.rm = .NULL.rm)
    do.call(.plotmethod, c(list(x), plotargs, recursive = F))
  }
}