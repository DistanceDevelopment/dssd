#' Virtual Class "Survey.Design"
#'
#' Virtual Class \code{"Survey.Design"} is an S4 class detailing the survey
#' design.
#'
#' @name Survey.Design-class
#' @title S4 Class "Survey.Design"
#' @slot region An object of class 'Region' defining the study area.
#' @slot design Character value describing the name of the design.
#' @slot no.samplers Numeric values defining the number of samplers in each
#' stratum.
#' @slot effort.allocation numeric values used to indicate the proportion of effort
#' to be allocated to each strata from number of samplers or line length. If length 0,
#' effort allocated based on stratum area.
#' @slot spacing used by systematic designs, numeric value to define spacing
#' between transects.
#' @slot design.angle numeric value detailing the angle of the design. Can provide
#' multiple values relating to strata. The use of the angle varies with design, it
#' can be either the angle of the grid of points, the angle of lines or the design
#' axis for the zigzag design.
#' @slot edge.protocol Character value defining whether a "minus" or "plus"
#' sampling strategy should be used.
#' @slot truncation Object of class \code{"numeric"}; The maximum distance
#' at which observations can be made. This is used to determine the covered
#' area during the coverage calculations.
#' @keywords classes
#' @export
#' @seealso \code{\link{make.design}}
setClass(Class = "Survey.Design",
         representation = representation(region = "Region",
                                         design = "character",
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
                  warning("You have supplied more than one truncation value. Currently the same truncation value must be applied across the entire study region. Using only the first value supplied.")
                  object@truncation <- object@truncation[1]
                }else if(object@truncation <= 0){
                  return("The truncation distance must be > 0.")
                }
              }
              #Check edge protocol
              if(!all(object@edge.protocol %in% c("minus", "plus"))){
                warning("Edge protocol option(s) not recognised using minus sampling.", call. = FALSE)
                index <- which(!(object@edge.protocol %in% c("minus", "plus")))
                object@edge.protocol[index] <- "minus"
              }
              #Check the design angle
              if(any(object@design.angle < 0 || object@design.angle >= 180)){
                return("The design angle should be >= 0 and < 180 degrees.")
              }
              return(TRUE)
            }
)

