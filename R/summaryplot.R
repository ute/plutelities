#'@title Summaryplot generic functions
#@rdname generic
#@name Generic function templates
#'@description Generic functions for use with other packages, a variant to \code{"plot"}
#'@param ... parameters passed to class methods.
#  \code{summaryplot} plot a summary of some data
#'@author Ute Hahn,  \email{ute@@imf.au.dk}
# NA
#
#@rdname generic
#'@details Meant for plotting a summary of some data.
#' @export

summaryplot <- function(...) UseMethod("summaryplot")
