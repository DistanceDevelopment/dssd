#' Virtual Class "Survey.Design"
#'
#' Virtual Class \code{"Survey.Design"} is an S4 class detailing the survey
#' design.
#'
#' @name Survey.Design-class
#' @title S4 Class "Survey.Design"
#' @slot truncation Object of class \code{"numeric"}; The maximum distance
#' at which observations can be made. This is used to determine the covered
#' area during the coverage calculations.
#' @slot no.samplers Numeric values defining the number of samplers in each
#' stratum.
#' @slot edge.protocol Character value defining whether a "minus" or "plus"
#' sampling strategy should be used.
#' @keywords classes
#' @export
#' @seealso \code{\link{make.design}}
setClass(Class = "Survey.Design",
         representation = representation(design = "character",
                                         no.samplers = "numeric",
                                         effort.allocation  = "numeric",
                                         spacing = "numeric",
                                         design.angle = "numeric",
                                         edge.protocol = "character",
                                         truncation = "numeric", "VIRTUAL")
)


setValidity("Survey.Design",
            function(object){
              if(length(object@effort.allocation) > 0){
                if(sum(object@effort.allocation) != 1){
                  return("Effort allocation should either be omitted or sum to 1")
                }
                if(length(object@effort.allocation) > 1 && length(object@no.samplers) > 1){
                  warning("You have supplied effort allocation and multiple values for the no.of samplers, the sum of the number or samplers will be used as the total number of samplers.")
                  object@no.samplers <- sum(object@no.samplers)
                }
                if(length(object@truncation) > 1){
                  warning("You have supplied more than one truncation value. Currently the same truncation value must be applied across the entire study region. Using only the first value supplied")
                  object@truncation <- object@truncation[1]
                }
              }
              return(TRUE)
            }
)

