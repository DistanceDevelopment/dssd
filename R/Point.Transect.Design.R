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
#'  \item{\code{generate.transects}}{\code{signature=(object = "Point.Transect.Design", quiet = FALSE,  ...)}:
#'  generates a set of transects from the design.}
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
  definition=function(.Object, region, truncation, design, spacing, samplers, effort.allocation, design.angle, edge.protocol, coverage.grid){
    #Set slots
    .Object@region        <- region
    .Object@truncation    <- truncation
    .Object@design        <- design
    .Object@spacing       <- spacing
    .Object@samplers      <- samplers
    .Object@effort.allocation <- effort.allocation
    .Object@design.angle  <- design.angle
    .Object@edge.protocol <- edge.protocol
    .Object@coverage.grid <- coverage.grid
    .Object@coverage.scores <- numeric(0)
    .Object@design.statistics <- data.frame()
    #Check object is valid
    # valid <- try(validObject(.Object), silent = TRUE)
    # if(class(valid) == "try-error"){
    #   stop(attr(valid, "condition")$message, call. = FALSE)
    # }
    # return object
    return(.Object)
  }
)

# GENERIC METHODS DEFINITIONS --------------------------------------------

#' @rdname generate.transects-methods
#' @export
setMethod(
  f="generate.transects",
  signature="Point.Transect.Design",
  definition=function(object, quiet = FALSE, ...){
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
    #Store original angles
    orig.angles <- object@design.angle
    #Make sure these are restored incase of a crash
    on.exit(object@design.angle <- orig.angles)
    #Now generate random design angles
    n <- length(which(object@design.angle == -1))
    object@design.angle <- ifelse(object@design.angle == -1, runif(n,0,179.9999), object@design.angle)
    #Get a vector of designs
    if(length(object@design) == 1){
      object@design <- rep(object@design, strata.no)
      unique.design <- TRUE
    }else{
      if(length(unique(object@design)) == 1){
        unique.design <- TRUE
      }else{
        unique.design <- FALSE
      }
    }
    #Calculate effort allocation if using only one design and only if spacing has not been specified.
    if(length(unique(object@design)) == 1  && length(object@spacing) == 0){
      if(length(object@effort.allocation) == 0){
        #Use area
        effort.allocation <- region@area/sum(region@area)
        # if(unique(object@design) == "systematic"){
        #   #Calculate a global spacing
        #   spacing <-
        # }
      }else{
        effort.allocation <- object@effort.allocation
      }
    }
    #Extract design parameters
    spacing <- object@spacing
    samplers <- object@samplers
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
      if(length(samplers) == 1 && unique.design){
        if(unique(object@design) == "systematic" && length(object@effort.allocation) == 0){
          spacing <- sum(abs(region@area))^0.5 / object@samplers^0.5
          spacing <- rep(spacing, strata.no)
          object@spacing <- spacing
        }else if(unique(object@design) == "systematic" && length(object@effort.allocation) > 0){
          samplers <- effort.allocation*object@samplers
          spacing <- abs(region@area)^0.5 / samplers^0.5
        }else{
          #assign sampler numbers based on effort allocation
          samplers <- effort.allocation*object@samplers
        }
      }else if(length(samplers) == strata.no){
        spacing <- apply(matrix(c(region@area, object@samplers), ncol = 2), FUN = function(x){abs(x[1])^0.5 / x[2]^0.5}, MARGIN = 1)
      }else{
        #assign sampler numbers based on effort allocation
        samplers <- effort.allocation*object@samplers
      }
    #If spacing has been provided for some but not all need to calculate spacing for others
    }else if(any(!by.spacing) && !all(!by.spacing)){
      for(strat in seq(along = strata.names)){
        if(!by.spacing[strat]){
          spacing[strat] <- abs(region@area[strat])^0.5 / object@samplers[strat]^0.5
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
        temp <- generate.systematic.points(design = object, strata.id = strat, spacing = spacing[strat], samplers = samplers[strat], coverage.grid = for.coverage, quiet = quiet)
        transects[[strat]] <- temp$transects
        polys[[strat]] <- temp$cover.polys
      }else if(object@design[strat] == "random"){
        temp <- generate.random.points(object, strat, samplers = round(samplers[strat]), quiet = quiet)
        transects[[strat]] <- temp$transects
        polys[[strat]] <- temp$cover.polys
      }else{
        message("This design is not supported at present")
        transects[[strat]] = NULL
      }
    }
    if(length(transects) == 0){
      if(!quiet){
        warning("No samplers generated.", immediate. = T, call. = FALSE)
      }
      index <- numeric(0)
    }else{
      index <- which(sapply(transects, Negate(is.null)))
    }
    cov.areas <- sampler.count <- numeric(0)
    transect.count <- 0
    strata.id <- character(0)
    for(strat in seq(along = index)){
      if(strat == 1 ){
        temp <- sf::st_sfc(transects[[index[strat]]])
        temp.poly <- sf::st_sfc(polys[[index[strat]]])
        transect.count <- length(transects[[index[strat]]])
        strata.id <- rep(strata.names[index[strat]], length(transects[[index[strat]]]))
      }else{
        temp <- c(temp, sf::st_sfc(transects[[index[strat]]]))
        temp.poly <- c(temp.poly, sf::st_sfc(polys[[index[strat]]]))
        transect.count <- transect.count + length(transects[[index[strat]]])
        strata.id <- c(strata.id, rep(strata.names[index[strat]], length(transects[[index[strat]]])))
      }
      cov.areas[index[strat]] <- sum(unlist(lapply(polys[[index[strat]]], FUN = sf::st_area)))
      sampler.count[index[strat]] <- length(transects[[index[strat]]])
    }
    if(for.coverage){
      all.transects <- sf::st_sf(data.frame(coverage.scores = rep(NA, transect.count), geom = temp))
      all.polys <- list()
    }else{
      all.transects <- sf::st_sf(data.frame(transect = 1:transect.count, strata = strata.id, geom = temp))
      all.polys <- sf::st_sf(data.frame(transect = 1:transect.count, strata = strata.id, geom = temp.poly))
    }
    #Set crs
    region.crs <- sf::st_crs(region@region)
    sf::st_crs(all.transects) <- region.crs
    if(!for.coverage){
      sf::st_crs(all.polys) <- region.crs
    }
    #Make a survey object
    survey <- new(Class="Point.Transect", design = object@design, points = all.transects, samp.count = sampler.count, effort.allocation = object@effort.allocation, spacing = spacing, design.angle = object@design.angle, edge.protocol = object@edge.protocol, cov.area = cov.areas, cov.area.polys = all.polys, strata.area = region@area, strata.names <- strata.names)
    return(survey)
  }
)
