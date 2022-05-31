line.coords.as.dataframe <- function(samplers){
  # Internal function which transforms the samplers into a
  # dataframe suitable for writing to csv file.
  all.transects <- data.frame(transect = NULL, strata = NULL, x.start = NULL, y.start = NULL, x.end = NULL, y.end = NULL)
  for(i in seq(along = samplers[[1]])){
    tr <- samplers$transect[i]
    st <- samplers$strata[i]
    if(inherits(samplers$geometry[i][[1]], "LINESTRING")){
      coords <- as.numeric(samplers$geometry[i][[1]])
      temp <- data.frame(transect = tr,
                         strata = st,
                         x.start = coords[1],
                         y.start = coords[3],
                         x.end = coords[2],
                         y.end = coords[4])
      all.transects <- rbind(all.transects,temp)
    }else if(inherits(samplers$geometry[i][[1]], "MULTILINESTRING")){
      for(j in seq(along = samplers$geometry[i][[1]])){
        coords <- as.numeric(samplers$geometry[i][[1]][[j]])
        temp <- data.frame(transect = tr,
                           strata = st,
                           x.start = coords[1],
                           y.start = coords[3],
                           x.end = coords[2],
                           y.end = coords[4])
        all.transects <- rbind(all.transects,temp)
      }
    }
  }
  return(all.transects)
}
