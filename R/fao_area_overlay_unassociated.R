#' @name fao_area_overlay_unassociated
#' @title Consistent spatial unassociated fao area overlay
#' @author Antoine Duparc, \email{antoine.duparc@@ird.fr}
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @description Consistent spatial fao area overlay for points, grids and polygons. This function work similar than the function "fao_area_overlay" but with a tolerance for coordinates onshore (for the case of coastal fishing and coordinates estimated).
#' @param data R dataframe, with at least two columns with longitude and latitude values. Be careful! Your longitude and latitude data have to be in the WGS84 projection and coordinates in decimal degrees.
#' @param overlay_level Maximum fao area level of accuarcy that you want for classified your data (character value). By default, major fao fishing area are selected. Check the section details below.
#' @param longitude_name Longitude column name in your data (character value).
#' @param latitude_name Latitude column name in your data (character value).
#' @param tolerance Tolerance of maximum distance between coordinates and FAO area (in km, numerical value expected). By default, 500 km.
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
#' # Example for classification until division fao fishing area, with a tolerance of 50 km
#' tmp <- fao_area_overlay(data = data,
#'                         overlay_level = "division",
#'                         longitude_name = "longitude",
#'                         latitude_name = "latitude",
#'                         tolerance = 50)
#' @export
fao_area_overlay_unassociated <- function(data,
                                          overlay_level = "major",
                                          longitude_name,
                                          latitude_name,
                                          tolerance = 500) {
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
  if (! is.numeric(tolerance)) {
    stop(paste0("Missing argument \"tolerance\" or not numerical value.",
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
  tmp1 <- unique(data[, c(longitude_name, latitude_name)])
  sp::coordinates(tmp1) <- c(longitude_name,
                             latitude_name)
  sp::proj4string(tmp1) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  tmp2 <- as.data.frame(tmp1)
  tmp1 <- sp::spTransform(tmp1,
                          "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  tmp <- sp::spTransform(tmp,
                         proj4string(tmp1))
  # Data spatial overlay ----
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
  tmp2 <- cbind(tmp2, as.data.frame(tmp1))
  names(tmp2)[3:4] <- c("longitude_bis", "latitude_bis")
  for (step1 in names(accuracy)) {
    tmp_sub <- tmp[tmp$F_LEVEL == step1, ]
    tmp3 <- as.data.frame(rgeos::gDistance(tmp_sub[, as.character(accuracy[step1])],
                                           tmp1,
                                           byid = TRUE))
    colnames(tmp3) <- as.character(t(tmp_sub@data[as.character(accuracy[step1])]))
    nb_col_tmp3 <- dim(tmp3)[2]
    for (i in 1:dim(tmp3)[1]) {
      tmp3[i, "min_dist"] <- ifelse(min(tmp3[i, 1:nb_col_tmp3]) < (tolerance * 1000),
                                    names(which.min(tmp3[i, ])),
                                    "far_away")
    }
    tmp4 <- select(.data = tmp3, min_dist)
    names(tmp4) <- paste0(as.character(accuracy[step1]),
                          "_NEAR")
    tmp2 <- cbind(tmp2,
                  tmp4)
    if (step1 == names(accuracy)[length(accuracy)]) {
      data <- dplyr::inner_join(data,
                                tmp2,
                                by = c(latitude_name, longitude_name)) %>%
        select(-longitude_bis, -latitude_bis)
    }
  }
  names(data) <- tolower(names(data))
  return(data)
}
