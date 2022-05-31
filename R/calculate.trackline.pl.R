calculate.trackline.pl <- function(transects){
  #Calculates the total trackline length (on and off effort) as
  #well as the total cyclic trackline length
  for(tr in seq(along = transects)){
    if(inherits(transects[[tr]], "MULTILINESTRING")){
      #Get total length of line
      last.index <- length(transects[[tr]])
      last.row.index <- nrow(transects[[tr]][[last.index]])
      total.line <- matrix(c(transects[[tr]][[1]][1,], transects[[tr]][[last.index]][last.row.index,]), byrow = TRUE, nrow = 2)
      transects[[tr]] <- sf::st_linestring(total.line)
    }
  }
  #Now add up line length
  top <- TRUE
  track.length <- 0
  for(tr in seq(along = transects)){
    #Add on total on and off effort transect length
    track.length <- track.length + sf::st_length(transects[[tr]])
    #add on between transect distance
    if(tr > 1){
      if(top){
        #dist between two top points
        x.diff <- abs(transects[[tr]][2,1] - transects[[(tr-1)]][2,1])
        y.diff <- abs(transects[[tr]][2,2] - transects[[(tr-1)]][2,2])
        dist <- sqrt(x.diff^2 + y.diff^2)
        track.length <- track.length + dist
        top <- FALSE
      }else{
        #dist between two bottom points
        x.diff <- abs(transects[[tr]][1,1] - transects[[(tr-1)]][1,1])
        y.diff <- abs(transects[[tr]][1,2] - transects[[(tr-1)]][1,2])
        dist <- sqrt(x.diff^2 + y.diff^2)
        track.length <- track.length + dist
        top <- TRUE
      }
    }
  }
  max.index <- length(transects)
  if(!top){
    x.diff <- abs(transects[[1]][1,1] - transects[[max.index]][2,1])
    y.diff <- abs(transects[[1]][1,2] - transects[[max.index]][2,2])
  }else{
    x.diff <- abs(transects[[1]][1,1] - transects[[max.index]][1,1])
    y.diff <- abs(transects[[1]][1,2] - transects[[max.index]][1,2])
  }
  cyclic.track.length <- track.length + sqrt(x.diff^2 + y.diff^2)
  return(list(trackline = track.length, cyclictrackline = cyclic.track.length))
}
