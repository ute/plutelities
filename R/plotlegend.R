# plot legend

#'@title Add object specific legends to plots
#'@description Generic function to add a legend to a plot, allowing to for arguments
#'given as simplists.
#'@export
#'@param ... arguments specifying the graphic legend,  
#'@details Generic function for plotting legends. Intended for use with objects
#'that carry legend information. The default method is a wrapper for the base
#'function \code{\link{legend}}
#'@seealso \code{\link{legend}} for the usual parameters.

plotlegend <- function(...) UseMethod("plotlegend")

#'@rdname plotlegend
#'@export
#
plotlegend.default <- function (...)
{
  do.call(splot, c(list(...), .plotmethod = "legend"))
}