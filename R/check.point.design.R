check.point.design <- function(object){
  # This functions checks the arguments of a point transect design.
  # Arguments:
  #    object - object of class Point.Transect.Design
  # Return:
  #    either the object which was passed in (potentially with some
  #    modifications) or a string value which will be displayed as an
  #    error
  #
  #Check how many strata there are
  strata.count <- length(object@region@strata.name)

  object <- check.design(object)
  # Check if it's now a character (i.e. error), if so return
  if(class(object) == "character"){
    return(object)
  }


  # CHECK DESIGNS
  # There should be one design value per stratum or a single global value supplied.
  # Designs must be either: random, systematic
  if(length(object@design) == 1){
    # Repeat global value for each stratum
    object@design <- rep(object@design, strata.count)
  }
  if(length(object@design) != strata.count){
    return("Design description argument has a different number of values than there are strata, please supply a single global value or one value per stratum.")
  }
  if(any(!(object@design %in% c("random", "systematic")))){
    index <- which(!object@design %in% c("random", "systematic"))
    return(paste("Unrecognised designs: ", paste(object@design[index], collapse = ", "), sep = ""))
  }


  # CHECK EFFORT PARAMETERS
  # Effort can be defined by either spacing, line.length or number of samplers.
  # Only one of these values must be supplied for each stratum or alternatively
  # a single global value. If multiple values are provided only the first
  # parameter of (spacing, line.length or number of samplers) will be used.
  # Spacing is not applicable for a random design.
  # All values defining effort must be > 0

  # Check if multiple types of effort values have been supplied
  spacing.len <- length(object@spacing)
  samplers.len <- length(object@samplers)
  # Store these as separate objects to allow NA only values (not allowed in numeric slots)
  spacing <- object@spacing
  samplers <- object@samplers
  # Turn 0 length into NA for if comparisons
  if(strata.count == 1){
    if(spacing.len == 0){
      spacing <- NA
    }
    if(samplers.len == 0){
      samplers <- NA
    }
  }

  # At least one type of effort descriptor must be supplied. If multiple types have
  # been provided the lengths of the values must be equal to the number of strata.
  if(spacing.len == 0 && samplers.len == 0){
    # If no values supplied use default of 20 samplers
    samplers <- 20
    samplers.len <- 1
  }
  if(spacing.len > 0 && samplers.len > 0){
    if(spacing.len > 0  && spacing.len != strata.count){
      return("You have supplied more than one effort descriptor (spacing, samplers). The number of values for each must be equal to the number of strata.")
    }
    if(samplers.len > 0  && samplers.len != strata.count){
      return("You have supplied more than one effort descriptor (spacing, samplers). The number of values for each must be equal to the number of strata.")
    }
  }

  # Check if only a single value has been provided for one of the effort descriptors
  # when there are multiple strata. Use this to set a flag for future checks
  global.value <- FALSE
  if(strata.count > 1){
    if(spacing.len == 1){
      global.value <- TRUE
      spacing <- rep(spacing, strata.count)
      if(samplers.len > 0){
        warning("Multiple global effort parameters have been supplied (spacing, samplers). Samplers will be ignored.", immediate. = TRUE, call. = FALSE)
        samplers <- numeric(0)
      }
    }else if(samplers.len == 1){
      global.value <- TRUE
    }
  }

  # If there is only a single value and it is spacing check that none of the designs
  # are random.
  if(global.value){
    if(spacing.len == 1 && any(object@design == "random")){
      return("Cannot specify a global spacing value when you have also selected a random design.")
    }
    # Otherwise iterate through the designs and effort parameters
  }else{
    # Iterate through strata and check
    for(i in 1:strata.count){
      # check that at least one effort has been provided for this stratum
      if(is.na(spacing[i]) && is.na(samplers[i])){
        return(paste("No sampler or spacing argument has been specified for strata ",i , ".", sep = "" ))
      }
      # Special checks for random design
      if(object@design[i] == "random" && !is.na(spacing[i])){
        if(is.na(samplers[i])){
          return(paste("Spacing is not a valid effort argument for the random design in stratum ", i, ", please supply samplers", sep = ""))
        }else{
          warning(paste("Spacing is not a valid effort argument for the random design in stratum ", i, ", it will be ignored.", sep = ""), immediate. = TRUE, call. = FALSE)
          spacing[i] <- NA
        }
      }
      # Now check that there is an effort parameter defined and give a warning if
      # multiple effort measures have been supplied.
      if(!is.na(samplers[i]) && !is.na(spacing[i])){
        warning("Both spacing and samplers have been supplied for stratum ",i,", samplers argument will be ignored.", immediate. = TRUE, call. = FALSE)
        samplers[i] <- NA
      }
    }
  }
  # Remove NA's when there is only one stratum
  if(strata.count == 1){
    if(is.na(spacing)){
      spacing <- numeric(0)
    }
    if(is.na(samplers)){
      samplers <- numeric(0)
    }
  }

  # Replace values back in object
  object@spacing <- spacing
  object@samplers <- samplers

  return(object)
}
