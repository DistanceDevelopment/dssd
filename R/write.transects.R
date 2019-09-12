#' @title Writes transects to file
#' @description This function will write the geographic information inside
#' an object inheriting from class Transect to file. Currently the only
#' options is to write to shapefile. It is basically a simple wrapper
#'  around the sf::st_write function. The sf::wt_write function can be
#'  used directly on the samplers slot of the Transect object if more
#'   options are required.
#' @param object an object inheriting from class Transect or an sf spatial
#' object extracted from a Transect object.
#' @param dsn the data source name, currently a filename with a 'shp'
#' or 'gpx' extension.
#' @param layer a character vector specifying the layer name, only
#' required for gpx files.
#' @param dataset.options a character vector of options, which vary by
#'  driver, and should be treated as experimental. Use to specify
#'  "GPX_USE_EXTENSIONS=yes" for writing gpx files.
#' @return invisibly the Transect object
#' @export
#' @author Laura Marshall
#' @details To write the transects to shapefile only the dsn is needed with
#' a 'shp' file extension. To write a gpx file you need to specify the dsn,
#' layer and dataset.options.
#'
#' Note that to write using the GPX driver the object you supply shoud be
#' the samplers extracted from the Transect object in graphical coordinates
#' with datum WGS84.
#' @examples
#' # Make the default design in the default study area
#' design <- make.design()
#' transects <- generate.transects(design)
#' write.transects(transects, dsn = paste0(tempdir(), "/", "transects.shp"))
write.transects <- function(object, dsn, layer = character(0), dataset.options = character(0), overwrite = FALSE, proj4string = character(0)){
  if(length(dsn) > 1){
    stop("Please supply only one dsn value", call. = FALSE)
  }
  #check which file type is being written
  dsn.parts <- strsplit(dsn, "[.]")
  dsn.ext <- dsn.parts[[1]][length(dsn.parts[[1]])]
  if(!(dsn.ext %in% c("shp","gpx"))){
    stop("dsn type not currently supported.", call. = FALSE)
  }
  if(dsn.ext == "shp"){
    if(inherits(object, "Transect")){
      sf::st_write(object@samplers, dsn)
    }else if(any(class(object) %in% c("sf","sfc"))){
      sf::st_write(object, dsn)
    }else{
      stop("Object of wrong class to write to shapefile.", call. = FALSE)
    }
  }else if(dsn.ext == "gpx"){
    if(length(layer) == 0 || length(dataset.options) == 0){
      stop("You must supply a layer and dataset options to write a gpx file. See documentation ", call. = FALSE)
    }else if(inherits(object, "Transect")){
      writeOGR(as(object@samplers, "Spatial"),
               dsn=dsn, layer=layer, driver="GPX",
               dataset_options=dataset.options)
    }else if(any(class(object) %in% c("sf","sfc"))){
      writeOGR(as(object, "Spatial"),
               dsn=dsn, layer=layer, driver="GPX",
               dataset_options=dataset.options)
    }else{
      stop("Object of wrong class to write to gpx file.", call. = FALSE)
    }
  }
  invisible(object)
}
