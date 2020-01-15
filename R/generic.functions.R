#' @importFrom graphics plot
#' @importFrom methods show
NULL

#' @title S4 generic method to generate an instance of a design
#'
#' @description Uses the survey design details in the design class to generate
#' a set of transects, i.e. a single survey.
#'
#' @details The transects are returned within an object of class Transect which
#' records some of the design options used to generate it along with the samplers
#' as an sf object of class 'POINT' or 'LINESTRING'/'MULTILINESTRING'. The
#' Transect object also contains the covered areas as a 'POLYGON' or
#' 'MULTIPOLYGON' sf object.
#'
#' @param object an object which inherits from class Survey.Design
#' @param quiet if TRUE silences some warnings
#' @param ... optional arguments used for internal calls
#' @return an object of class Transect
#' @author L Marshall
#' @export
#' @seealso \link{write.transects}
#' @rdname generate.transects-methods
#' @examples
#' #Point transect example
#' shapefile.name <- system.file("extdata", "TrackExample.shp", package = "dssd")
#' region <- make.region(region.name = "study area",
#'                      shape = shapefile.name)
#' design <- make.design(region = region,
#'                       transect.type = "point",
#'                       design = "random",
#'                       samplers = 25,
#'                       design.angle = 45,
#'                       edge.protocol = "minus",
#'                       truncation = 3)
#' # Generate a single survey instance
#' survey <- generate.transects(design)
#' plot(region, survey, covered.area = TRUE)
#'
#' #Line transect example
#' # Define the design
#' design <- make.design(region = region,
#'                       transect.type = "line",
#'                       design = c("systematic"),
#'                       line.length = 1000,
#'                       design.angle = c(179),
#'                       edge.protocol = "minus",
#'                       truncation = 1)
#'
#' # Create a single set of transects to check
#' survey <- generate.transects(design)
#' plot(region, survey, covered.area = TRUE)
setGeneric("generate.transects", function(object, quiet = FALSE, ...){standardGeneric ("generate.transects")})

#' S4 generic method to extract coverage scores
#'
#' Obtains the coverage scores from the survey design object.
#'
#' @details See ?make.design for example code
#' @param object an object which inherits from class Survey.Design
#' @param strata.id either "all" or a numeric value indicating the strata index.
#' @return a vector of coverage scores
#' @seealso \link{make.design}
#' @export
#' @rdname get.coverage-methods
setGeneric("get.coverage", function(object, strata.id = "all"){standardGeneric ("get.coverage")})

if (!isGeneric("plot")){
  setGeneric(name = "plot", def = function(x, y, ...){standardGeneric("plot")})
}

if (!isGeneric("show")){
  setGeneric(name = "show", def = function(object){standardGeneric("show")})
}

if (!isGeneric("summary")){
  setGeneric(name = "summary", def = function(object, ...){standardGeneric("summary")})
}
