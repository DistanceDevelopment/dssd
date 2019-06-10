check.line.design <- function(object){
  #Check how many strata there are
  strata.count <- length(object@region@strata.name)
  #EFFORT ALLOCATION
  if(length(object@effort.allocation) > 0){
    if(sum(object@effort.allocation, na.rm = T) != 1){
      return("Effort allocation should either be omitted or sum to 1")
    }
    if(any(is.na(object@effort.allocation))){
      return("Sorry, effort allocation is only applied across all strata at present. NA values are not permitted.")
    }
  }
  #TRUNCATION
  if(length(object@truncation) > 1){
    warning("You have supplied more than one truncation value. Currently the same truncation value must be applied across the entire study region. Using only the first value supplied.")
    object@truncation <- object@truncation[1]
  }else if(object@truncation <= 0){
    return("The truncation distance must be > 0.")
  }
  #Check edge protocol
  if(length(object@edge.protocol) == 1){
    object@edge.protocol <- rep(object@edge.protocol, strata.count)
  }else if(length(object@edge.protocol) > 1 && length(object@edge.protocol) != strata.count){
    warning("Edge protocol argument has a different number of values than there are strata, only using the 1st value.")
    object@edge.protocol <- rep(object@edge.protocol[1], strata.count)
  }
  if(!all(object@edge.protocol %in% c("minus", "plus"))){
    warning("Some edge protocol option(s) not recognised using minus sampling for these strata.", call. = FALSE)
    index <- which(!(object@edge.protocol %in% c("minus", "plus")))
    object@edge.protocol[index] <- "minus"
  }
  #DESIGN ANGLE
  #Check the design angle
  if(length(object@design.angle) == 1){
    object@design.angle <- rep(object@design.angle, strata.count)
  }else if(length(object@design.angle) > 1 && length(object@design.angle) != strata.count){
    warning("Design angle argument has a different number of values than there are strata, only using the 1st value.")
    object@design.angle <- rep(object@design.angle[1], strata.count)
  }
  if(any(object@design.angle < 0 || object@design.angle >= 180)){
    return("The design angle should be >= 0 and < 180 degrees.")
  }
  #Check design
  if(length(object@design) == 1){
    object@design <- rep(object@design, strata.count)
  }else if(length(object@design) > 1 && length(object@design) != strata.count){
    warning("Design argument has a different number of values than there are strata, only using the 1st value.", call. = FALSE, immediate. = TRUE)
    object@design <- rep(object@design[1], strata.count)
  }
  if(any(!(object@design %in% c("random", "systematic", "eszigzag", "eszigzagcom")))){
    return(paste("Unrecognised designs: ", object@design, sep = ""))
  }
  if(all(object@design == "random") && length(object@spacing > 0)){
    object@spacing <- numeric(0)
  }
  #Check for spacing values
  spacing.for.all = FALSE
  if(length(object@spacing) == 1){
    object@spacing <- rep(object@spacing, strata.count)
    spacing.for.all = TRUE
    if(length(object@samplers) > 0){
      object@samplers <- numeric(0)
    }
    if(length(object@line.length) >0){
      object@line.length <- numeric(0)
    }
  }else if(length(object@spacing) > 1 && length(object@spacing) != strata.count){
    spacing.for.all = FALSE
  }else if(length(object@spacing) == strata.count && all(!is.na(object@spacing))){
    spacing.for.all = TRUE
  }else if(length(object@spacing) == strata.count && any(is.na(object@spacing))){
    spacing.for.all = FALSE
  }else if(length(object@spacing) > 1 && length(object@spacing) < strata.count){
    object@spacing <- c(object@spacing, rep(NA, (strata.count - length(object@spacing))))
  }
  #Return TRUE if they are all systematic and spacings have been provided for all
  if(all(object@design == "systematic") && spacing.for.all){
    return(object)
  }
  #If random selected then check that samplers or line.length has been supplied
  check.effort.allocation = FALSE
  if(all(object@design == "random")){
    if(length(object@samplers == 0) && length(object@line.length) == 0){
      object@samplers <- 20
      check.effort.allocation = TRUE
    }else if(length(object@samplers == 1) || length(object@line.length == 1)){
      check.effort.allocation = TRUE
    }else if(length(object@samplers == strata.count && !any(is.na(object@samplers)))){
      object@line.length <- numeric(0)
      return(object)
    }else if(length(object@line.length == strata.count && !any(is.na(object@line.length)))){
      object@samplers <- numeric(0)
      return(object)
    }else if(length(object@samplers) != strata.count){
      return("Incorrect number of sampler values provided, either provide one total or one value for each strata.")
    }
  }
  #Check effort allocation
  if(check.effort.allocation){
    if(length(object@effort.allocation > 0) && length(object@effort.allocation) != strata.count){
      return("Incorrect number of effort.allocation values supplied, should either be omitted or have the same number of values as there are strata.")
    }else{
      #All ok so return TRUE - random design with single value
      return(object)
    }
  }
  #Check if samplers has been supplied
  if((length(object@samplers) > 0 || length(object@line.length) > 0) && spacing.for.all){
    warning("Samplers/line.length argument being ignored as spacings were provided for all strata.", call. = FALSE, immediate. = TRUE)
    object@samplers <- numeric(0)
    object@line.length <- numeric(0)
  }
  #if there is a mixture of designs / design options
  for(i in 1:strata.count){
    if(object@design[i] == "random"){
      if(!is.numeric(object@samplers[i]) && !is.numeric(object@line.length[i])){
        return(paste("Strata ", i, " has a random design but a non numeric argument has been supplied for both the number of samplers and the line length.", sep = "" ))
      }
    }else if(object@design[i] %in% c("systematic", "eszigzag", "eszigzagcom")){
      if(!is.numeric(object@samplers[i]) && !is.numeric(object@spacing[i]) && !is.numeric(object@line.length[i])){
        return(paste("Strata ", i, " has a systematic design but a non numeric argument has been supplied for the number of samplers, the spacing and the line length.", sep = "" ))
      }
    }
  }
  return(object)
}
