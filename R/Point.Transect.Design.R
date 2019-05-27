#' @include generic.functions.R
#' @include Survey.Design.R
#' @include Region.R
#' @importFrom methods validObject

#' @title Virtual Class "Point.Transect.Design" extends Class "Survey.Design"
#'
#' @description Virtual Class \code{"Point.Transect.Design"} is an S4 class detailing
#' the type of point transect design.
#' @name Point.Transect.Design-class
#' @title S4 Class "Point.Transect.Design"
#' @section Methods:
#' \describe{
#'  \item{\code{generate.transects}}{\code{signature=(object = "Point.Transect.Design", ...)}:
#'  generates a set of transects from a shapefile.}
#' }
#' @keywords classes
#' @seealso \code{\link{make.design}}
#' @export
setClass(Class = "Point.Transect.Design",
         representation = representation(),
         contains = "Survey.Design"
)


setMethod(
  f="initialize",
  signature="Point.Transect.Design",
  definition=function(.Object, region, truncation, design, spacing, no.samplers, effort.allocation, design.angle, edge.protocol, coverage.grid){
    #Set slots
    .Object@region        <- region
    .Object@truncation    <- truncation
    .Object@design        <- design
    .Object@spacing       <- spacing
    .Object@no.samplers   <- no.samplers
    .Object@effort.allocation <- effort.allocation
    .Object@design.angle  <- design.angle
    .Object@edge.protocol <- edge.protocol
    .Object@coverage.grid <- coverage.grid
    .Object@design.statistics <- data.frame()
    #Check object is valid
    valid <- try(validObject(.Object), silent = TRUE)
    if(class(valid) == "try-error"){
      stop(attr(valid, "condition")$message, call. = FALSE)
    }
    # return object
    return(.Object)
  }
)

setValidity("Point.Transect.Design",
            function(object){
              return(TRUE)
            }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname generate.transects-methods
#' @export
setMethod(
  f="generate.transects",
  signature="Point.Transect.Design",
  definition=function(object, silent = FALSE, ...){
    # Process additional arguments
    additional.args <- list(...)
    for.coverage <- ifelse("for.coverage" %in% names (additional.args), additional.args$for.coverage, FALSE)
    # Get strata names
    region <- object@region
    sf.column <- attr(region@region, "sf_column")
    if(length(region@strata.name) > 0){
      strata.names <- region@strata.name
      strata.no <- length(region@strata.name)
    }else{
      strata.names <- region@region.name
      strata.no <- 1
    }
    #Get a vector of designs
    if(length(object@design) == 1){
      object@design <- rep(object@design, strata.no)
    }else{
      object@design <- object@design
    }
    #Calculate effort allocation if using only one design and only if spacing has not been specified.
    if(length(unique(object@design)) == 1  && length(object@spacing) == 0){
      if(length(object@effort.allocation) == 0){
        #Use area
        effort.allocation <- region@area/sum(region@area)
      }else{
        effort.allocation <- object@effort.allocation
      }
    }
    #Extract design parameters
    spacing <- object@spacing
    no.samplers <- object@no.samplers
    #Check if only has one has been
    if(length(spacing) == 1){
      spacing <- rep(spacing, strata.no)
      by.spacing <- rep(TRUE, strata.no)
    }else if(length(spacing) == strata.no){
      by.spacing <- ifelse(is.na(spacing), FALSE, TRUE)
    }else if(length(spacing) == 0){
      by.spacing = rep(FALSE, strata.no)
    }
    #If spacing has not been provided for any
    if(all(!by.spacing)){
      #If only a total number of samplers has been provided (and there is only one design)
      if(length(no.samplers) == 1 && length(object@design) == 1){
        #Calculate spacing from the number of desired samplers across whole region
        spacing <- sum(abs(region@area))^0.5 / object@no.samplers^0.5
        spacing <- rep(spacing, strata.no)
      }else if(length(no.samplers) == strata.no){
        spacing <- apply(matrix(c(region@area, object@no.samplers), ncol = 2), FUN = function(x){abs(x[1])^0.5 / x[2]^0.5}, MARGIN = 1)
      }
    #If spacing has been provided for some but not all need to calculate spacing for others
    }else if(any(!by.spacing) && !all(!by.spacing)){
      for(strat in seq(along = strata.names)){
        if(!by.spacing[strat]){
          spacing[strat] <- abs(region@area[strat])^0.5 / object@no.samplers[strat]^0.5
        }
      }
    }
    #Store all lines in a list
    transects <- list()
    polys <- list()
    #Iterate over strata calling the appropriate method for the design.
    #Main grid generation
    for (strat in seq(along = region@region[[sf.column]])) {
      if(object@design[strat] %in% c("systematic")){
        temp <- generate.systematic.points(design = object, strata.id = strat, spacing = spacing[strat], for.coverage = for.coverage)
        transects[[strat]] <- temp$transects
        polys[[strat]] <- temp$cover.polys
      #}else if(object@design[strat] == "random"){
      #  transects[[strat]] <- generate.random.points(object, strat, no.samplers[strat], line.length[strat], spacing[strat], by.spacing[strat])
      }else{
        message("This design is not supported at present")
        transects[[strat]] = NULL
      }
    }
    #Put transects into a point sf object defined by
    #the strata name
    #counter <- 1
    #transect.count <- 0
    # #Find first strata where there are transects
    # while(is.na(transects[[counter]]) && counter < length(transects)){
    #   counter <- counter + 1
    #   cat(counter)
    # }
    #If there are some transects somewhere
    # if(!is.na(transects[[counter]][[1]])){
    #   transect.count <- dim(transects[[counter]])[1]
    #   temp <- sf::st_sfc(transects[[counter]])
    #   #Now add in transects from other strata
    #   if(length(transects) > counter){
    #     for(strat in (counter+1):length(transects)){
    #       if(!is.na(transects[[strat]][[1]])){
    #         transect.count <- transect.count + dim(transects[[strat]])[1]
    #         temp <- c(temp, sf::st_sfc(transects[[strat]]))
    #       }
    #     }
    #   }
    cov.areas <- sampler.count <- numeric(0)
    transect.count <- 0
    strata.id <- character(0)
    for(strat in seq(along = transects)){
      if(strat == 1 ){
        temp <- sf::st_sfc(transects[[strat]])
        temp.poly <- sf::st_sfc(polys[[strat]])
        transect.count <- length(transects[[strat]])
        strata.id <- rep(strata.names[strat], length(transects[[strat]]))
      }else{
        temp <- c(temp, sf::st_sfc(transects[[strat]]))
        temp.poly <- c(temp.poly, sf::st_sfc(polys[[strat]]))
        transect.count <- transect.count + length(transects[[strat]])
        strata.id <- c(strata.id, rep(strata.names[strat], length(transects[[strat]])))
      }
      cov.areas[strat] <- sum(unlist(lapply(polys[[strat]], FUN = sf::st_area)))
      sampler.count[strat] <- length(transects[[strat]])
    }
    if(for.coverage){
      all.transects <- sf::st_sf(data.frame(coverage.scores = rep(NA, transect.count), geom = temp))
      all.polys <- list()
    }else{
      all.transects <- sf::st_sf(data.frame(transect = 1:transect.count, strata = strata.id, geom = temp))
      all.polys <- sf::st_sf(data.frame(transect = 1:transect.count, strata = strata.id, geom = temp.poly))
    }
    #}else{
    #  all.transects <- list()
    #}
    #Make a survey object
    survey <- new(Class="Point.Transect", design = object@design, points = all.transects, no.samplers = transect.count, effort.allocation = object@effort.allocation, spacing = spacing, design.angle = object@design.angle, edge.protocol = object@edge.protocol, cov.area = cov.areas, cov.area.polys = all.polys)
    return(survey)
  }
)
