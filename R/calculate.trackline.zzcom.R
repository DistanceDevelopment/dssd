calculate.trackline.zzcom <- function(transects){
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
  setindex <- substr(names(transects), 4, 4)
  setA <- transects[which(setindex == "A")]
  setB <- transects[which(setindex == "B")]
  #Now add up line length
  track.length <- 0
  for(tr in seq(along = setA)){
    #Add on total on and off effort transect length
    track.length <- track.length + sf::st_length(setA[[tr]])
    #add on between transect distance
    if(tr > 1){
      #dist between two top points
      x.diff <- abs(setA[[tr]][1,1] - setA[[(tr-1)]][2,1])
      y.diff <- abs(setA[[tr]][1,2] - setA[[(tr-1)]][2,2])
      dist <- sqrt(x.diff^2 + y.diff^2)
      track.length <- track.length + dist
    }
  }
  #Now do same for set B
  for(tr in seq(along = setB)){
    #Add on total on and off effort transect length
    track.length <- track.length + sf::st_length(setB[[tr]])
    #add on between transect distance
    if(tr > 1){
      #dist between two top points
      x.diff <- abs(setB[[tr]][1,1] - setB[[(tr-1)]][2,1])
      y.diff <- abs(setB[[tr]][1,2] - setB[[(tr-1)]][2,2])
      dist <- sqrt(x.diff^2 + y.diff^2)
      track.length <- track.length + dist
    }
  }
  #Now add on off effort transit time between two sets of end points
  x.diff <- abs(setB[[1]][1,1] - setA[[1]][1,1])
  y.diff <- abs(setB[[1]][1,2] - setA[[1]][1,2])
  dist <- sqrt(x.diff^2 + y.diff^2)
  track.length <- track.length + dist
  #Now calculate cyclic by adding on distance between other set of end points
  final.index.A <- length(setA)
  final.index.B <- length(setB)
  x.diff <- abs(setB[[final.index.B]][2,1] - setA[[final.index.A]][2,1])
  y.diff <- abs(setB[[final.index.B]][2,2] - setA[[final.index.A]][2,2])
  dist <- sqrt(x.diff^2 + y.diff^2)
  cyclic.track.length <- track.length + dist
  return(list(trackline = track.length, cyclictrackline = cyclic.track.length))
}
