#' @importFrom utils sessionInfo
#' @importFrom sf st_make_valid st_set_precision st_is_valid
mat.mult <- function(x,y){
  # Internal function for performing matrix multiplication with atlas checks for validity
  # reverse rotation
  unrotate <- x*y
  # if we are using atlas and the shape is not valid
  if(grepl("atlas", sessionInfo()$BLAS) && is.na(sf::st_is_valid(unrotate))){
    # validity flag
    is.valid <- FALSE
    # counter
    index <- 1
    # Precision values to try
    precision <- c(1e10,1e9,1e8,1e7,1e6,1e5,1e4)
    # turn it into and sfc shape
    tmp <- sf::st_sfc(unrotate)
    # Keep trying to fix across a range of precision values
    while(!is.valid && index <= length(precision)){
      tmp2 <- try(sf::st_make_valid(sf::st_set_precision(tmp, precision[index])), silent = TRUE)
      if(inherits(tmp2, "try-error")){
        # If the problem is not fixed try a lower precision
        index <- index + 1
      }else{
        # Otherwise it is fixed continue
        is.valid <- TRUE
        tmp <- tmp2
      }
    }
    # What if none of the precisions succeed?
    if(!is.valid){
      stop("Problem with matrix multiplication due to BLAS/ATLAS setup. dssd has tried to resolve this issue by reducing the precision to 1e4 (see ?sf::st_precision) but differences persist in what should be identical coordinates.", call. = FALSE)
    }
    # extract shape again
    unrotate <- tmp[[1]]
  }
  return(unrotate)
}
