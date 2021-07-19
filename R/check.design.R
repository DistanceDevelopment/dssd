check.design <- function(object){
  # This functions checks the arguments of a design (relevant to both point and
  # line transect designs).
  # Arguments:
  #    object - object of class Survey.Design
  # Return:
  #    either the object which was passed in (potentially with some
  #    modifications) or a string value which will be displayed as an
  #    error
  #
  #Check how many strata there are
  strata.count <- length(object@region@strata.name)

  # CHECK EFFORT ALLOCATION
  # Effort allocation values if supplied should sum to 1 and there should
  # be no missing values and one value per stratum.
  if(length(object@effort.allocation) > 0){
    if(sum(object@effort.allocation, na.rm = T) != 1){
      return("Effort allocation should either be omitted or sum to 1.")
    }
    if(any(is.na(object@effort.allocation))){
      return("Sorry, effort allocation is only applied across all strata at present. NA values are not permitted.")
    }
    if(length(object@effort.allocation) != strata.count){
      return("The length of the effort allocation argument should be equal to the number of strata.")
    }
  }

  # CHECK TRUNCATION
  # Only one global truncation distance should be supplied and it must be
  # numeric and greater than 0.
  if(length(object@truncation) > 1){
    warning("You have supplied more than one truncation value. Currently the same truncation value must be applied across the entire study region. Using only the first value supplied.", call. = FALSE, immediate. = TRUE)
    object@truncation <- object@truncation[1]
  }else if(object@truncation <= 0){
    return("The truncation distance must be > 0.")
  }

  # CHECK EDGE PROTOCOL
  # Check that there is one edge protocol value per strata and that it is
  # either "plus" or "minus". If only one value is supplied this values is
  # assumed to apply globally across all strata.
  if(length(object@edge.protocol) == 1){
    # If only value repeat for all strata
    object@edge.protocol <- rep(object@edge.protocol, strata.count)
  }
  if(length(object@edge.protocol) != strata.count){
    return("Edge protocol argument has a different number of values than there are strata, please either supply a single global value or one value per stratum.")
  }
  if(any(!object@edge.protocol %in% c("minus", "plus"))){
    return("Edge protocol values must either be 'plus' or 'minus'.")
  }

  # DESIGN ANGLE
  # Either a single design angle should be supplied or one per stratum. All angles
  # should be between >= 0 and < 180 (0 is equilavent to 180) or equal to -1
  # indicating a random design angle.
  if(length(object@design.angle) == 1){
    # Repeat global value for each stratum
    object@design.angle <- rep(object@design.angle, strata.count)
  }
  if(length(object@design.angle) != strata.count){
    return("Design angle argument has a different number of values than there are strata, please supply a single global value or one value per stratum.")
  }
  if(any(is.na(object@design.angle))){
    return("NA values supplied for design angle. Please supply values >= 0 and < 180 or the value -1 to indicate a random design angle selection.")
  }
  # Check to see if any angles are < 0 or >= 180
  index.0 <- which(object@design.angle < 0)
  index.180 <- which(object@design.angle >= 180)
  # Check that if they are they are equal to -1
  if(any(object@design.angle[c(index.0, index.180)] != -1)){
    return("Design angle values must be >=0 and < 180 or the value -1 to indicate a random design angle selection.")
  }

  return(object)
}
