#' @title Writes transects to file
#' @description This function will write the geographic information inside
#' an object inheriting from class Transect to file. Currently the only
#' options is to write to shapefile. It is basically a simple wrapper
#'  around the sf::st_write function. The sf::wt_write function can be
#'  used directly on the samplers slot of the Transect object if more
#'   options are required.
#' @param object and object inheriting from class Transect
#' @param dsn the data source name, for this simple function this is
#' a pathway and filename with a '.shp' extension.
#' @return invisibly the Transect object
#' @export
#' @author Laura Marshall
#' @examples
#' # Make the default design in the default study area
#' design <- make.design()
#' transects <- generate.transects(design)
#' write.transects(transects, dsn = paste0(tempdir(), "/", "transects.shp"))
write.transects <- function(object, dsn){
  sf::st_write(object@samplers, dsn)
  invisible(object)
}
