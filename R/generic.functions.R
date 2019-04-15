#' @import methods
NULL

if (!isGeneric("plot")){
  setGeneric(name = "plot", def = function(x, y, ...){standardGeneric("plot")})
}

