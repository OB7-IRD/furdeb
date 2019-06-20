#' @name fao_area_overlay
#' @title Consistent spatial fao area overlay
#' @author Antoine Duparc, \email{antoine.duparc@@ird.fr}
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @description Consistent spatial fao area overlay for points, grids and polygons.
#' @param data R dataframe, with at least two columns with longitude and latitude values. Be careful! Your longitude and latitude data have to be in the WGS84 projection and coordinates in decimal degrees.
#' @param overlay_level Maximum fao area level of accuarcy that you want for classified your data (character value). By default, major fao fishing area are selected. Check the section details below.
#' @param longitude_name Longitude column name in your data (character value).
#' @param latitude_name Latitude column name in your data (character value).
#' @references \url{https://github.com/OB7-IRD/toolbox}
#' @return The function return your input dataframe with one or several columns (regarding specification in the argument "overlay_level") which contains fao area classification.
#' @details
#' For the argument "overlay_level", you can choose between 5 modalities (descending size classification):
#' \itemize{
#'  \item{major: }{major fao fishing area}
#'  \item{subarea: }{sub fao fishing area}
#'  \item{division: }{division fao fishing area}
#'  \item{subdivision: }{sub-division fao fishing area}
#'  \item{subunit: }{sub-unit fao fishing area}
#' }
#' All the items above your specification (thus contain it at higher levels) will be added in the output. For example, if you select "subarea", you will also have the information about the major area concerning.
#' If you want more informations visit http://www.fao.org/fishery/area/search/en
#' @examples
#' # Example for classification until division fao fishing area
#' tmp <- fao_area_overlay(data = data,
#'                         overlay_level = "division",
#'                         longitude_name = "longitude",
#'                         latitude_name = "latitude")
#' @export
fao_area_overlay <- function (data,
                              overlay_level = "major",
                              longitude_name,
                              latitude_name) {
  # Arguments checking ----
  if (missing(data) || ! is.data.frame(data)) {
    stop(paste0("Missing argument \"data\" or not a data frame.",
                "\n",
                "Please correct it before running the function."))
  }
  if (! overlay_level %in% c("major", "subarea", "division", "subdivision", "subunit")) {
    stop(paste0("Argument \"overlay_level\" not correct.",
                "\n",
                "Expected value: major, subarea, division, subdivision or subunit.",
                "\n",
                "Please correct it before running the function."))
  }
  if (missing(longitude_name) || ! is.character(longitude_name)) {
    stop(paste0("Missing argument \"longitude_name\" or not character value.",
                "\n",
                "Please correct it before running the function."))
  }
  if (missing(latitude_name) || ! is.character(latitude_name)) {
    stop(paste0("Missing argument \"latitude_name\" or not character value.",
                "\n",
                "Please correct it before running the function."))
  }
  cat("Be careful!",
      "\n",
      "You're spatial coordinates have to be in WGS84 projection",
      "\n",
      "Be patient! The function could be long")
  # Fao area shapefile importation ----
  tmp <- rgdal::readOGR(dsn = system.file("fao_area",
                                          "FAO_AREAS.shp",
                                          package = "toolbox"),
                        verbose = FALSE)
  # Data design ----
  sp::coordinates(data) <- c(longitude_name,
                             latitude_name)
  sp::proj4string(data) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # Data spatial overlay
  if (overlay_level == "major") {
    accuracy <- "F_AREA"
    names(accuracy) <- "MAJOR"
  } else {
    if (overlay_level == "subarea") {
      accuracy <- c("F_AREA", "F_SUBAREA")
      names(accuracy) <- c("MAJOR", "SUBAREA")
    } else {
      if (overlay_level == "division") {
        accuracy <- c("F_AREA", "F_SUBAREA", "F_DIVISION")
        names(accuracy) <- c("MAJOR", "SUBAREA", "DIVISION")
      } else {
        if (overlay_level == "subdivision") {
          accuracy <- c("F_AREA", "F_SUBAREA", "F_DIVISION", "F_SUBDIVIS")
          names(accuracy) <- c("MAJOR", "SUBAREA", "DIVISION", "SUBDIVISION")
        } else {
          if (overlay_level == "subunit") {
            accuracy <- c("F_AREA", "F_SUBAREA", "F_DIVISION", "F_SUBDIVIS", "F_SUBUNIT")
            names(accuracy) <- c("MAJOR", "SUBAREA", "DIVISION", "SUBDIVISION", "SUBUNIT")
          }
        }
      }
    }
  }
  tmp2 <- data.frame(matrix(NA, nrow = length(data), ncol = 0))
  for (step1 in names(accuracy)) {
    tmp_sub <- tmp[tmp$F_LEVEL == step1, ]
    tmp1 <- sp::over(data,
                     tmp_sub[,as.character(accuracy[step1])])
    if (class(data) == "SpatialPointsDataFrame") {
      data@data <- cbind(data@data,
                         tmp1, stringsAsFactors=F)
      if (step1 == names(accuracy)[length(accuracy)]) {
        data <- as.data.frame(data)
      }
    } else {
      if (class(data) == "SpatialPoints") {
        tmp2 <- cbind(tmp2, tmp1)
        if (step1 == names(accuracy)[length(accuracy)]) {
          data <- cbind(as.data.frame(data),
                        tmp2)
        }
      }
    }

  }
  for (step2 in as.character(accuracy)) {
    data[, step2] <- as.character(data[, step2])
  }
  names(data) <- tolower(names(data))
  return(data)
}
