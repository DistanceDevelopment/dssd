#' @importFrom graphics plot
#' @importFrom methods show
NULL


#' @title S4 generic method to generate an instance of a design
#'
#' @description Uses the Survey.Design details to generate a set of transects.
#' The transects are returned within an object of class Transect which records
#' some of the design options used to generate it along with the samplers as
#' an sf object of class 'POINT' or 'LINESTRING'/'MULTILINESTRING'. The
#' Transect object also contains the covered areas as a 'POLYGON' or
#' 'MULTIPOLYGON' sf object.
#'
#' @param object an object which inherits from class Survey.Design
#' @param quiet silences some warnings
#' @param ... optional arguments used for internal calls
#' @return an object of class Transect
#' @export
#' @rdname generate.transects-methods
setGeneric("generate.transects", function(object, quiet = FALSE, ...){standardGeneric ("generate.transects")})

#' S4 generic method to extract coverage scores
#'
#' Obtains the coverage scores from the survey design object.
#'
#' @param object an object which inherits from class Survey.Design
#' @return a vector of coverage scores
#' @export
#' @rdname get.coverage-methods
setGeneric("get.coverage", function(object){standardGeneric ("get.coverage")})

if (!isGeneric("plot")){
  setGeneric(name = "plot", def = function(x, y, ...){standardGeneric("plot")})
}

if (!isGeneric("show")){
  setGeneric(name = "show", def = function(object){standardGeneric("show")})
}

if (!isGeneric("summary")){
  setGeneric(name = "summary", def = function(object, ...){standardGeneric("summary")})
}
