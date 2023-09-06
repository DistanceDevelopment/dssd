#' @title Writes transects to file
#' @description This function will write a set of transects to file, either as
#' a shapefile or gpx file, or it will write the transect coordinates (centre
#' points for point transects or end points for line transects) to a
#' comma-separated values 'csv' file or a text file 'txt' with tabular spacing
#' between columns. For line transects which have been split across geographical
#' features (such as islands or lakes) there will be two or more rows in the
#' csv / txt file with all rows having the same transect ID.
#' @param object an object inheriting from class Transect. Alternatively, for 
#' all file types except gpx an sf spatial object can be supplied.
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
#' @importFrom methods as
#' @author Laura Marshall
#' @details To write the transects to file usually only the dsn is needed with
#' a 'shp', 'csv' or 'txt' file extension. To write a gpx file you need to
#' specify the dsn and a projection so allow the coordinates to be transformed.
#' back into latitude and longitude.
#'
#' @examples
#' # Note that for CRAN testing purposes all files written in example code must
#' # be written to a temporary directory, to view this location type tempdir().
#' # It is however advised that you replace the tempdir() commands in the code
#' # below to a more easily accessible directory to which the files will be
#' # written.
#'
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
#' sf.shape <- sf::read_sf(shapefile.name)
#' # Check current coordinate reference system
#' orig.crs <- sf::st_crs(sf.shape)
#' # Define a European Albers Equal Area projection
#' proj4string <- "+proj=aea +lat_1=56 +lat_2=62 +lat_0=50 +lon_0=-3 +x_0=0
#'                 +y_0=0 +ellps=intl +units=m"
#' # Project the study area on to a flat plane
#' projected.shape <- sf::st_transform(sf.shape, crs = proj4string)
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
#'                 proj4string = orig.crs)
#'
write.transects <- function(object, dsn, layer = NULL, dataset.options = character(0), overwrite = FALSE, proj4string = character(0)){
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
    }else if(inherits(object,"sf") || inherits(object,"sfc")){
      sf::st_write(object, dsn)
    }else{
      stop("Object of wrong class to write to shapefile.", call. = FALSE)
    }
  }else if(dsn.ext == "gpx"){
    if(inherits(object, "Transect")){
      sf.column <- attr(object@samplers, "sf_column")
      #Check that there is a specific geometry defined
      if(is(object@samplers[[sf.column]], "sfc_GEOMETRY")){
        if(inherits(object, "Point.Transect")){
          object@samplers <- sf::st_cast(object@samplers, "POINT")
        }else if(inherits(object, "Line.Transect")){
          object@samplers <- sf::st_cast(object@samplers, "MULTILINESTRING")
        }
      }
    }else{
      stop("Please supply an object of either class Point.Transect or Line.Transect to write to gpx file.", call. = FALSE)
    }
    # Write to gpx file
    sf::st_write(object@samplers[[sf.column]],
                 dsn=dsn, 
                 layer=layer)
  }else if(dsn.ext == "csv"){
    if(inherits(object, "Point.Transect")){
      samplers <- point.coords.as.dataframe(object@samplers)
      write.csv(samplers, file = dsn, row.names = FALSE)
    }else if(inherits(object, "Line.Transect")){
      samplers <- line.coords.as.dataframe(object@samplers)
      write.csv(samplers, file = dsn, row.names = FALSE)
    }else if(inherits(object,"sf") || inherits(object,"sfc")){
      sf.column <- attr(object, "sf_column")
      if(inherits(object[[sf.column]],"sfc_LINESTRING") || inherits(object[[sf.column]],"sfc_MULTILINESTRING") || inherits(object[[sf.column]],"LINESTRING") || inherits(object[[sf.column]],"MULTILINESTRING")){
        samplers <- line.coords.as.dataframe(object)
        write.csv(samplers, file = dsn, row.names = FALSE)
      }else if(inherits(object[[sf.column]], "sfc_POINT") || inherits(object[[sf.column]], "POINT")){
        samplers <- point.coords.as.dataframe(object)
        write.csv(samplers, file = dsn, row.names = FALSE)
      }
    }
  }else if(dsn.ext == "txt"){
    if(inherits(object, "Point.Transect")){
      samplers <- point.coords.as.dataframe(object@samplers)
      write.table(samplers, file = dsn, sep = "\t", row.names = FALSE)
    }else if(inherits(object, "Line.Transect")){
      samplers <- line.coords.as.dataframe(object@samplers)
      write.table(samplers, file = dsn, sep = "\t", row.names = FALSE)
    }else if(inherits(object,"sf") || inherits(object,"sfc")){
      sf.column <- attr(object, "sf_column")
        if(inherits(object[[sf.column]],"sfc_LINESTRING") || inherits(object[[sf.column]],"sfc_MULTILINESTRING") || inherits(object[[sf.column]],"LINESTRING") || inherits(object[[sf.column]],"MULTILINESTRING")){  
        samplers <- line.coords.as.dataframe(object)
        write.table(samplers, file = dsn, sep = "\t", row.names = FALSE)
      }else if(inherits(object[[sf.column]], "sfc_POINT") || inherits(object[[sf.column]], "POINT")){
        samplers <- point.coords.as.dataframe(object)
        write.table(samplers, file = dsn, sep = "\t", row.names = FALSE)
      }
    }
  }else{
    stop("dsn file extension not currently supported.", call. = FALSE)
  }
  invisible(object)
}
