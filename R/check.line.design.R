check.line.design <- function(object){
  # This functions checks the arguments of a line transect design.
  # Arguments:
  #    object - object of class Line.Transect.Design
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

  # SEGMENTED GRID DESIGN CHECKS
  if("Segment.Transect.Design" %in% class(object)){

    # Find which strata have the segmented line design
    index <- which(object@design == "segmentedgrid")
    index.neg <- which(object@design != "segmentedgrid")

    # SEGMENT LENGTH
    # Either one global value or one value per strata. All values should be
    # numeric and greater than 0. Values should only be provided for strata
    # where the design is of type 'segmentedgrid'.
    if(length(object@seg.length) == 1){
      # Repeat global value for each stratum
      seg.length <- object@seg.length
      object@seg.length <- rep(seg.length, strata.count)
      object@seg.length[index.neg] <- NA
    }
    if(length(object@seg.length) != strata.count){
      return("Segment length argument has a different number of values than there are strata, please supply a single global value or one value per stratum.")
    }
    if(!is.numeric(object@seg.length)){
      return("All segment length values must be numeric.")
    }
    if(any(na.omit(object@seg.length <= 0))){
      return("All segment length values must be greater than zero.")
    }
    if(any(is.na(object@seg.length[index]))){
      return("NA values have been provided for segment length in strata where a segmented grid design has been selected.")
    }
    if(any(!is.na(object@seg.length[index.neg]))){
      warning("Non NA values have been provided for segment length in strata where a segmented grid design was NOT selected. These vaues will be ignored.", immediate. = TRUE, call. = FALSE)
      object@seg.length[index.neg] <- NA
    }

    # SEGMENT THRESHOLD
    # Either one global value or one value per strata. All values should be
    # numeric and between 0 and 100. Values should only be provided for strata
    # where the design is of type 'segmentedgrid'.
    if(length(object@seg.threshold) == 1){
      # Repeat global value for each stratum
      seg.threshold <- object@seg.threshold
      object@seg.threshold <- rep(seg.threshold, strata.count)
      object@seg.threshold[index.neg] <- NA
    }
    if(length(object@seg.threshold) != strata.count){
      return("Segment threshold argument has a different number of values than there are strata, please supply a single global value or one value per stratum.)")
    }
    if(!is.numeric(object@seg.threshold)){
      return("All segment threshold values must be numeric.")
    }
    if(any(na.omit(object@seg.threshold < 0)) || any(na.omit(object@seg.threshold > 100))){
      return("Values for segment threshold for segmented grid design must be between 0 and 100.")
    }
    if(any(is.na(object@seg.threshold[index]))){
      return("NA values have been provided for segment threshold in strata where a segmented grid design has been selected.")
    }
    if(any(!is.na(object@seg.threshold[index.neg]))){
      warning("Non NA values have been provided for segment threshold in strata where a segmented grid design was NOT selected. These vaues will be ignored.", immediate. = TRUE, call. = FALSE)
      object@seg.threshold[index.neg] <- NA
    }
  }

  # CHECK BOUNDING SHAPE
  # bounding shape must be supplied for strata where zigzag designs are
  # selected. Values must either be 'convex hull' or 'rectangle'.
  if(any(object@design %in% c("eszigzag", "eszigzagcom"))){

    # Find which strata have the segmented line design
    index <- which(object@design %in% c("eszigzag", "eszigzagcom"))
    index.neg <- which(!object@design %in% c("eszigzag", "eszigzagcom"))

    if(length(object@bounding.shape) == 1){
      # Repeat global value for each stratum
      bounding.shape <- object@bounding.shape
      object@bounding.shape <- rep(bounding.shape, strata.count)
      object@bounding.shape[index.neg] <- NA
    }
    if(length(object@bounding.shape) != strata.count){
      return("Bounding shape argument has a different number of values than there are strata, please supply a single global value or one value per stratum.")
    }
  if(any(!na.omit(object@bounding.shape) %in% c("convex.hull", "rectangle"))){
      return("All bounding shape values must either be 'convex.hull' or 'rectangle'.")
    }
    if(any(is.na(object@bounding.shape[index]))){
      return("NA values have been provided for bounding shape in strata where a zigzag design has been selected. Please supply valid values.")
    }
    if(any(!is.na(object@bounding.shape[index.neg]))){
      warning("Non NA values have been provided for bounding shape in strata where a zigzag design was NOT selected. These vaues will be ignored.", immediate. = TRUE, call. = FALSE)
      object@bounding.shape[index.neg] <- NA
    }
  }

  # CHECK DESIGNS
  # There should be one design value per stratum or a single global value supplied.
  # Designs must be either: random, systematic, eszigzag, eszigzagcom, segmentedgrid
  if(length(object@design) == 1){
    # Repeat global value for each stratum
    object@design <- rep(object@design, strata.count)
  }
  if(length(object@design) != strata.count){
    return("Design description argument has a different number of values than there are strata, please supply a single global value or one value per stratum.")
  }
  if(any(!(object@design %in% c("random", "systematic", "eszigzag", "eszigzagcom", "segmentedgrid")))){
    index <- which(!object@design %in% c("random", "systematic", "eszigzag", "eszigzagcom", "segmentedgrid"))
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
  line.len <- length(object@line.length)
  samplers.len <- length(object@samplers)
  # Store these as separate objects to allow NA only values (not allowed in numeric slots)
  spacing <- object@spacing
  line.length <- object@line.length
  samplers <- object@samplers
  # Turn 0 length into NA for if comparisons
  if(strata.count == 1){
    if(spacing.len == 0){
     spacing <- NA
    }
    if(line.len == 0){
      line.length <- NA
    }
    if(samplers.len == 0){
      samplers <- NA
    }
  }

  # At least one type of effort descriptor must be supplied. If multiple types have
  # been provided the lengths of the values must be equal to the number of strata.
  if(spacing.len == 0 && line.len == 0 && samplers.len == 0){
    # If no values supplied use default of 20 samplers
    samplers <- 20
    samplers.len <- 1
  }
  if(spacing.len > 0 && line.len > 0 ||
     spacing.len > 0 && samplers.len > 0 ||
     line.len > 0 && samplers.len > 0){
    if(spacing.len > 0  && spacing.len != strata.count){
      return("You have supplied more than one effort descriptor (spacing, line.length, samplers). The number of values for each must be equal to the number of strata.")
    }
    if(line.len > 0  && line.len != strata.count){
      return("You have supplied more than one effort descriptor (spacing, line.length, samplers). The number of values for each must be equal to the number of strata.")
    }
    if(samplers.len > 0  && samplers.len != strata.count){
      return("You have supplied more than one effort descriptor (spacing, line.length, samplers). The number of values for each must be equal to the number of strata.")
    }
  }

  # Check if only a single value has been provided for one of the effort descriptors
  # when there are multiple strata. Use this to set a flag for future checks
  global.value <- FALSE
  if(strata.count > 1){
    if(spacing.len == 1){
      global.value <- TRUE
      spacing <- rep(object@spacing, strata.count)
      if(line.len > 0 || samplers.len > 0){
        warning("Multiple global effort parameters have been supplied (spacing, line length, samplers). Spacing will be used and the others ignored.", immediate. = TRUE, call. = FALSE)
        line.length <- numeric(0)
        samplers <- numeric(0)
      }
    }else if(line.len == 1){
      global.value <- TRUE
      if(spacing.len > 0){
        warning("Both line length and samplers have been provided. The global line length will be used and samplers ignored.", immediate. = TRUE, call. = FALSE)
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
      if(is.na(spacing[i]) && is.na(line.length[i]) && is.na(samplers[i])){
        return(paste("No sampler, spacing or line.length arguments have been specified for stratum ",i , ".", sep = "" ))
      }
      # Special checks for random design
      if(object@design[i] == "random" && !is.na(spacing[i])){
        if(is.na(line.length[i]) && is.na(samplers[i])){
          return(paste("Spacing is not a valid effort argument for the random design in stratum ", i, ", please supply line length or samplers", sep = ""))
        }else{
          warning(paste("Spacing is not a valid effort argument for the random design in stratum ", i, ", it will be ignored.", sep = ""), immediate. = TRUE, call. = FALSE)
          spacing[i] <- NA
        }
      }
      # Now check that there is an effort parameter defined and give a warning if
      # multiple effort measures have been supplied.
      if(!is.na(samplers[i]) && !is.na(spacing[i]) && !is.na(line.length[i])){
        warning("Spacing, samplers and line.length have been supplied for stratum ",i,", samplers and line.length arguments will be ignored.", immediate. = TRUE, call. = FALSE)
        samplers[i] <- NA
        line.length[i] <- NA
      }else if(!is.na(samplers[i]) && !is.na(spacing[i])){
        warning("Both spacing and samplers have been supplied for stratum ",i,", samplers argument will be ignored.", immediate. = TRUE, call. = FALSE)
        samplers[i] <- NA
      }else if(!is.na(line.length[i]) && !is.na(spacing[i])){
        warning("Both spacing and line.length have been supplied for stratum  ",i,", line.length argument will be ignored.", immediate. = TRUE, call. = FALSE)
        line.length[i] <- NA
      }else if(!is.na(line.length[i]) && !is.na(samplers[i])){
        warning("Both sampers and line.length have been supplied for stratum ",i,", samplers argument will be ignored.", immediate. = TRUE, call. = FALSE)
        samplers[i] <- NA
      }
    }
  }
  # Remove NA's when there is only one stratum
  if(strata.count == 1){
    if(is.na(spacing)){
     spacing <- numeric(0)
    }
    if(is.na(line.length)){
      line.length <- numeric(0)
    }
    if(is.na(samplers)){
      samplers <- numeric(0)
    }
  }

  # Replace values back in object
  object@spacing <- spacing
  object@line.length <- line.length
  object@samplers <- samplers

  # Check if effort.allocation is redundant
  if(any(c(samplers.len, line.len) > 1) && length(object@effort.allocation) > 1){
    warning("Effort allocation argument redundant as you have supplied stratum specific effort values, it will be ignored.", immediate. = TRUE, call. = FALSE)
    object@effort.allocation <- numeric(0)
  }
  if(strata.count == 1  && length(object@effort.allocation) > 0){
    warning("Effort allocation argument redundant as there is only one stratum, it will be ignored.", immediate. = TRUE, call. = FALSE)
    object@effort.allocation <- numeric(0)
  }
  if(strata.count > 1 && # multiple strata
     samplers.len == 1 &&  # single value for samplers
     length(object@effort.allocation) == 0 && # no effort allocation values
     class(object) != "Segment.Transect.Design" && # not a segmented design
     # there are multiple designs or the design is random
     (length(unique(object@design)) > 1 || "random" %in% object@design)){
    warning("The default allocation of samplers to strata (i.e. the number of samplers per stratum are in proportion to stratum areas) may lead to an unequal effort design as average sampler lengths could vary between strata.", immediate. = TRUE, call. = FALSE)
  }
  if(spacing.len >= 1 && length(object@effort.allocation) != 0){
    warning("Effort allocation not applicable when effort is determined by spacing, it will be ignored.", immediate. = TRUE, call. = FALSE)
    object@effort.allocation <- numeric(0)
  }

  return(object)
}
