#' @title Writes transects to file
#' @description This function will write a set of transects to file, either as
#' a shapefile or gpx file, or it will write the transect coordinates (centre
#' points for point transects or end points for line transects) to a
#' comma-separated values 'csv' file or a text file 'txt' with tabular spacing
#' between columns. For line transects which have been split across geographical
#' features (such as islands or lakes) there will be two or more rows in the
#' csv / txt file with all rows having the same transect ID.
#' @param object an object inheriting from class Transect or an sf spatial
#' object extracted from a Transect object.
#' @param dsn the data source name, currently a filename with a 'shp'
#' 'csv', 'txt' or 'gpx' extension.
#' @param layer a character vector specifying the layer name, only
#' required for gpx files.
#' @param dataset.options a character vector of options, which vary by
#'  driver, and should be treated as experimental. Used to specify
#'  "GPX_USE_EXTENSIONS=yes" for writing gpx files.
#' @param overwrite whether or not existing files should be overwritten.
#' Only applicable when writing to gpx files.
#' @param proj4string The projection you wish the coordinates of the
#' output file to be in. Note, when writing to gpx file the transect
#' coordinates must be in latitude and longitude.
#' @return invisibly the Transect object
#' @export
#' @importFrom utils write.table write.csv
#' @importFrom sf as_Spatial
#' @importFrom rgdal writeOGR
#' @author Laura Marshall
#' @details To write the transects to shapefile only the dsn is needed with
#' a 'shp', 'csv' or 'txt' file extension. To write a gpx file you need to
#' specify the dsn, layer, dataset.options and usually a projection to
#' project the coordinates back into latitude and longitude.
#'
#' @examples
#' # Make the default design in the default study area
#' design <- make.design()
#' transects <- generate.transects(design)
#' write.transects(transects, dsn = paste0(tempdir(), "/", "transects.shp"))
#'
#' # Writing csv file example
#' write.transects(transects, dsn = paste0(tempdir(), "/", "transects.csv"))
#'
#' # Writing txt file example
#' write.transects(transects, dsn = paste0(tempdir(), "/", "transects.txt"))
#'
#' # Writing gpx file example - must project transect coords into lat/lon
#' #Load the unprojected shapefile
#' shapefile.name <- system.file("extdata", "TentsmuirUnproj.shp", package = "dssd")
#' sf.shape <- read_sf(shapefile.name)
#' # Check current coordinate reference system
#' orig.crs <- st_crs(sf.shape)
#' # Define a European Albers Equal Area projection
#' proj4string <- "+proj=aea +lat_1=56 +lat_2=62 +lat_0=50 +lon_0=-3 +x_0=0
#'                 +y_0=0 +ellps=intl +units=m"
#' # Project the study area on to a flat plane
#' projected.shape <- st_transform(sf.shape, crs = proj4string)
#' # Create the survey region in dssd
#' region.tm <- make.region(region.name = "Tentsmuir",
#'                          strata.name = c("Main Area", "Morton Lochs"),
#'                          shape = projected.shape)
#'
#' design <- make.design(region = region.tm,
#'                       transect.type = "line",
#'                       design = "systematic",
#'                       samplers = 20,
#'                       design.angle = 90)
#' survey <- generate.transects(design)
#' plot(region.tm, survey)
#'
#' write.transects(survey,
#'                 dsn = paste0(tempdir(), "/", "transects.gpx"),
#'                 layer = "lines",
#'                 dataset.options = "GPX_USE_EXTENSIONS=yes",
#'                 proj4string = orig.crs)
#'
write.transects <- function(object, dsn, layer = character(0), dataset.options = character(0), overwrite = FALSE, proj4string = character(0)){
  if(length(proj4string) > 0){
    if(is.na(sf::st_crs(object@samplers))){
      warning("No coordinate system found for survey transects. A coordinate system is only specified for transects if one was specified for the survey region. Cannot project survey transects.", immediate. = TRUE, call. = FALSE)
    }else{
      object@samplers <- sf::st_transform(object@samplers, proj4string)
    }
  }
  if(length(dsn) > 1){
    stop("Please supply only one dsn value", call. = FALSE)
  }
  #check which file type is being written
  dsn.parts <- strsplit(dsn, "[.]")
  dsn.ext <- dsn.parts[[1]][length(dsn.parts[[1]])]
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
      rgdal::writeOGR(as(object@samplers, "Spatial"),
               dsn=dsn, layer=layer, driver="GPX",
               dataset_options=dataset.options,
               overwrite_layer = overwrite)
    }else if(any(class(object) %in% c("sf","sfc"))){
      rgdal::writeOGR(as(object, "Spatial"),
               dsn=dsn, layer=layer, driver="GPX",
               dataset_options=dataset.options,
               overwrite_layer = overwrite)
    }else{
      stop("Object of wrong class to write to gpx file.", call. = FALSE)
    }
  }else if(dsn.ext == "csv"){
    if(class(object)[1] == "Point.Transect"){
      samplers <- point.coords.as.dataframe(object@samplers)
      write.csv(samplers, file = dsn, row.names = FALSE)
    }else if(class(object)[1] == "Line.Transect"){
      samplers <- line.coords.as.dataframe(object@samplers)
      write.csv(samplers, file = dsn, row.names = FALSE)
    }else if(any(class(object) %in% c("sf","sfc"))){
      sf.column <- attr(object, "sf_column")
      if(any(class(object[[sf.column]])) %in% c("sfc_LINESTRING","sfc_MULTILINESTRING","LINESTRING","MULTILINESTRING")){
        samplers <- line.coords.as.dataframe(object)
        write.csv(samplers, file = dsn, row.names = FALSE)
      }else if(any(class(object[[sf.column]])) %in% c("sfc_POINT","POINT")){
        samplers <- point.coords.as.dataframe(object)
        write.csv(samplers, file = dsn, row.names = FALSE)
      }
    }
  }else if(dsn.ext == "txt"){
    if(class(object)[1] == "Point.Transect"){
      samplers <- point.coords.as.dataframe(object@samplers)
      write.table(samplers, file = dsn, sep = "\t", row.names = FALSE)
    }else if(class(object)[1] == "Line.Transect"){
      samplers <- line.coords.as.dataframe(object@samplers)
      write.table(samplers, file = dsn, sep = "\t", row.names = FALSE)
    }else if(any(class(object) %in% c("sf","sfc"))){
      sf.column <- attr(object, "sf_column")
      if(any(class(object[[sf.column]])) %in% c("sfc_LINESTRING","sfc_MULTILINESTRING","LINESTRING","MULTILINESTRING")){
        samplers <- line.coords.as.dataframe(object)
        write.table(samplers, file = dsn, sep = "\t", row.names = FALSE)
      }else if(any(class(object[[sf.column]])) %in% c("sfc_POINT","POINT")){
        samplers <- point.coords.as.dataframe(object)
        write.table(samplers, file = dsn, sep = "\t", row.names = FALSE)
      }
    }
  }else{
    stop("dsn file extension not currently supported.", call. = FALSE)
  }
  invisible(object)
}
