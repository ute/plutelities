#'@title Find matching parameters by name

#'@description Finds elements in a list whose names matche some given arguments
#'@param x named list, input elements
#'@param ... named lists, functions or function names \code{x} is to be matched against, 
#' see the details
#'@return a list containing those elements of \code{x} whose names match the 
#'arguments, see details.
#'@details For functions or functions names contained in \code{...}, the argument
#'list is matched. This works only for non primitive functions.
#'@export
#'@author Ute Hahn,  \email{ute@@imf.au.dk}
#'@examples
#'A <- list(col = "red", xlab = "x", u = 13, v = 91)
#'B <- list(u = 5, t = 2)
#'str(matching(A, B))
#'str(matching(A, "plot.default"))
#'str(matching(A, plot.default, B))
        
matching <- function(x, ...)
{
  stopifnot(is.list(x))
  if (length(x) < 1) return(list())
  dotargs <- as.list(c(..., "", recursive = FALSE)) 
    # the "" forces something reasonable if only one element is in ...
  dotargs <- dotargs[-length(dotargs)]
  allnames <- character(0)
  rawnames <- names(dotargs)
  if(is.null(rawnames)) rawnames <- rep("", length(dotargs))
  nameless <- sapply(rawnames, function(n) is.null(n) || (n==""))
  isfun <-  sapply(dotargs, function(x) is.function(x) || (
    is.character(x) && (x !="") 
    && exists(x) && is.function(get(x))))
  for (i in 1:length(dotargs))
  {
    argi <- dotargs[[i]]
    allnames <- c(allnames, 
                  if (!nameless[i]) rawnames[i]
                  else {if (isfun[i]) names(formals(argi))})
  } 
  x[names(x) %in% allnames]
}


#'@title Predefined objects 
#'@rdname plottools-predefined
#'@name Predefined parameter lists
#'@description Predefined lists of parameters, for convenience
#'@details 
#'\tabular{ll}{ 
#' \code{.plotparams} \tab parameters that may cause warnings in low level plotting
#' \cr\tab functions, but not in \code{\link{plot.default}}
#' \cr\code{.graphparams} \tab graphical parameters that can be set by \code{\link{par}},
#' \cr\tab no risk when passed to low level plotting functions.
#'}
#'@format named lists
NA

#'@rdname plottools-predefined
#'@export
.plotparams <- list(type = "p",  xlim = NULL, ylim = NULL,
                    log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                    ann = par("ann"), axes = TRUE, frame.plot = TRUE,
                    panel.first = NULL, panel.last = NULL, asp = NA,
                    mgp = par("mgp"))

#'@rdname plottools-predefined
#'@export
.graphparams <- par(no.readonly = TRUE)
