#' @importFrom sf st_zm
check.shape <- function(sf.shape, dist.for.win){
  #This function was added as there were some issues with shapefiles
  #created by Distance. There seemed to be some redundant information added.
  #Checks that object is of class sf
  
  # Remove ZM values if they exist
  sf.shape <- st_zm(sf.shape)
  # Check shapefile type
  sf.column <- attr(sf.shape, "sf_column")
  if(!any(c("sfc_POLYGON", "sfc_MULTIPOLYGON") %in% attributes(sf.shape[[sf.column]])$class)){
    stop("The shapefile you have provided is of incorrect type. The shapefile must contain polygons or multipolygons.", call. = FALSE)
  }
  if(dist.for.win){
    # check if the shapefile is ordered correctly strata names will have been 
    # supplied from Dist for Win in Link ID order but dssd uses shapefile ordering.
    # Returns the original shape if LinkID doesn't exist, or if the order is correct
    # Otherwise returns the updated shape with strata order based on LinkID
    if (is.null(sf.shape$LinkID)){
      #If there is no LinkID don't need to worry can't be in dist.for.windows!
      return(sf.shape)
    }else{
      LinkID <-sf.shape$LinkID
      index <- order(LinkID)
      compare <- index == (1:length(LinkID))
      #If any do not match
      if(any(!compare)){
        #Re-order strata
        new.shape <-sf.shape[index, ]
        warning("The LinkID values were not in sequential order in the shapefile attribute table, dssd is reordering the strata to match that which Distance for Windows uses. This is a necessary step if you are running simulations from Distance for Windows. If you are running simulations directly in R and would like to switch this option off please set dist.for.win to FALSE in make.region.", immediate. = TRUE, call. = FALSE)
        return(new.shape)
      }else{
        #If they are in the right order already don't need to do anything
        return(sf.shape)
      }
    }
  }
  return(sf.shape)
}
