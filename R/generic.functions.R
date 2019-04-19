#' @import methods
#' @importFrom graphics plot
NULL


#' S4 generic method to generate an instance of a design
#'
#' Uses the Survey.Design details to generate transects. Currently this
#' involves loading a survey shapefile from the path specified in the
#' Survey.Design object and can only work with line transect designs.
#'
#' @param object an object of class Simulation or a class which inherits from
#'   Survey.Design
#' @param region optional only required if object is of class Survey.Design.
#' @param index specifies which set of transect should be loaded
#' @param ... optional argument index if an object of class Survey.Design is
#' supplied allowing the user to access / plot different sets of transects
#' listed in the filenames slot.
#' @return an object of class Line.Transect
#' @export
#' @rdname generate.transects-methods
setGeneric("generate.transects", function(object, region = NULL, ...){standardGeneric ("generate.transects")})


if (!isGeneric("plot")){
  setGeneric(name = "plot", def = function(x, y, ...){standardGeneric("plot")})
}
