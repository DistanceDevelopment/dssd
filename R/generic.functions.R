#' @importFrom graphics plot
NULL


#' S4 generic method to generate an instance of a design
#'
#' Uses the Survey.Design details to generate transects. Currently this
#' involves loading a survey shapefile from the path specified in the
#' Survey.Design object and can only work with line transect designs.
#'
#' @param object an object which inherits from class Survey.Design
#' @param silent silences some warnings
#' @param ... optional arguments used for internal calls
#' @return an object of class Line.Transect
#' @export
#' @rdname generate.transects-methods
setGeneric("generate.transects", function(object, silent, ...){standardGeneric ("generate.transects")})

if (!isGeneric("plot")){
  setGeneric(name = "plot", def = function(x, y, ...){standardGeneric("plot")})
}
