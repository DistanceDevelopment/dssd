#' Virtual Class "Survey.Design"
#'
#' Virtual Class \code{"Survey.Design"} is an S4 class detailing the survey
#' design.
#'
#' @name Survey.Design-class
#' @title S4 Class "Survey.Design"
#' @slot region.obj Object of class \code{"character"}; The name of
#' the region which the survey design has been made for.
#' @slot truncation Object of class \code{"numeric"}; The maximum distance
#' at which observations can be made. This is used to determine the covered
#' area during the coverage calculations.
#' @keywords classes
#' @export
#' @seealso \code{\link{make.design}}
setClass(Class = "Survey.Design",
         representation = representation(region.obj = "character",
                                         truncation = "numberic","VIRTUAL")
)

setMethod(
  f="initialize",
  signature="Survey.Design",
  definition=function(.Object, region, truncation, ...){
    #Set slots
    .Object@region.obj    <- region
    .Object@truncation    <- truncation
    #Check object is valid
    validObject(.Object)
    # return object
    return(.Object)
  }
)

setValidity("Survey.Design",
            function(object){
              return(TRUE)
            }
)

