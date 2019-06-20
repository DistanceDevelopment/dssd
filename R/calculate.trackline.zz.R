calculate.trackline.zz <- function(transects){
  #Calculates the total trackline length (on and off effort) as
  #well as the total cyclic trackline length
  for(tr in seq(along = transects)){
    if(any(class(transects[[tr]]) == "MULTILINESTRING")){
      #Get total length of line
      last.index <- length(transects[[tr]])
      last.row.index <- nrow(transects[[tr]][[last.index]])
      total.line <- matrix(c(transects[[tr]][[1]][1,], transects[[tr]][[last.index]][last.row.index,]), byrow = TRUE, nrow = 2)
      transects[[tr]] <- sf::st_linestring(total.line)
    }
  }
  #Now add up line length
  track.length <- 0
  for(tr in seq(along = transects)){
    #Add on total on and off effort transect length
    track.length <- track.length + sf::st_length(transects[[tr]])
    #add on between transect distance
    if(tr > 1){
      #dist between two top points
      x.diff <- abs(transects[[tr]][1,1] - transects[[(tr-1)]][2,1])
      y.diff <- abs(transects[[tr]][1,2] - transects[[(tr-1)]][2,2])
      dist <- sqrt(x.diff^2 + y.diff^2)
      track.length <- track.length + dist
    }
  }
  max.index <- length(transects)
  x.diff <- abs(transects[[max.index]][2,1]- transects[[1]][1,1])
  y.diff <- abs(transects[[max.index]][2,2]- transects[[1]][1,2])
  cyclic.track.length <- track.length + sqrt(x.diff^2 + y.diff^2)
  return(list(trackline = track.length, cyclictrackline = cyclic.track.length))
}
