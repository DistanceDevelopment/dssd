calculate.trackline.segl <- function(transects){
  #This function relies on the segments being generated in the same direction along each of the sampler
  #lines.
  #Separate out the lines
  transect.names <- names(transects)
  IDmatrix <- matrix(unlist(strsplit(transect.names, split = "[.]")), ncol = 2, byrow = TRUE, dimnames = list(1:length(transect.names), c("samplerID", "segID")))
  IDdf <- as.data.frame(IDmatrix)
  IDdf[,1] <- as.numeric(IDdf[,1])
  IDdf[,2] <- as.numeric(IDdf[,2])

  samplerID <- unique(IDdf$samplerID)
  coords <- numeric(0)
  for(i in seq(along = samplerID)){
    # Find the relevant segments
    index <- which(IDdf$samplerID == samplerID[i])
    index <- index[c(1,length(index))]
    #If line x odd
    if((i %% 2) != 0){
      #Get the first coordinates of the first segment on line x
      if(inherits(transects[[index[1]]], "LINESTRING")){
        coords <- c(coords, transects[[index[1]]][1,])
      }else if(inherits(transects[[index[1]]], "MULTILINESTRING")){
        coords <- c(coords, transects[[index[1]]][[1]][1,])
      }
      #Get the last coordinate of the last segment on line x
      if(inherits(transects[[index[2]]], "LINESTRING")){
        coords <- c(coords, transects[[index[2]]][nrow(transects[[index[2]]]),])
      }else if(inherits(transects[[index[2]]], "MULTILINESTRING")){
        last.seg.part <- length(transects[[index[2]]])
        coords <- c(coords, transects[[index[2]]][[last.seg.part]][nrow(transects[[index[2]]][[last.seg.part]]),])
      }
    }else{ #If line x even
      #Get the last coordinate of the last segment
      if(inherits(transects[[index[2]]], "LINESTRING")){
        coords <- c(coords, transects[[index[2]]][nrow(transects[[index[2]]]),])
      }else if(inherits(transects[[index[2]]], "MULTILINESTRING")){
        last.seg.part <- length(transects[[index[2]]])
        coords <- c(coords, transects[[index[2]]][[last.seg.part]][nrow(transects[[index[2]]][[last.seg.part]]),])
      }
      #Get the first coordinate of the first segment
      if(inherits(transects[[index[1]]], "LINESTRING")){
        coords <- c(coords, transects[[index[1]]][1,])
      }else if(inherits(transects[[index[1]]], "MULTILINESTRING")){
        coords <- c(coords, transects[[index[1]]][[1]][1,])
      }
    }
  }
  #Make a multiline string
  coord.mat <- matrix(coords, ncol=2, byrow=TRUE)
  track.ls <- st_linestring(coord.mat)
  #Find length
  track.length <- st_length(track.ls)
  #Calculate additional length from end of last to beginning of first line
  coord.mat <- rbind(coord.mat, coord.mat[1,])
  cyclictrack.ls <- st_linestring(coord.mat)
  cyclic.track.length <- st_length(cyclictrack.ls)
  #last.row <- nrow(coord.mat)
  #track.length+ sqrt(abs(coord.mat[16,1]-coord.mat[1,1])^2+abs(coord.mat[16,2]-coord.mat[1,2])^2)
  #plot(cyclictrack.ls, add = T, col = 4)
  #plot(track.ls, add = T, col = 4)
  #for(i in 1){
  #  plot(transects[[i]], add = T, col = 3, lwd = 3)
  #}
  return(list(trackline = track.length, cyclictrackline = cyclic.track.length))
}
