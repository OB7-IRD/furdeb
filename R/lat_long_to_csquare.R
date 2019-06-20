#' @name lat_long_to_csquare
#' @title Latitude-longitude to c-square converter
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @description Latitude-longitude to c-square converter.
#' @param data R dataframe, with at least two columns with longitude and latitude values. Be careful! Your longitude and latitude data have to be in the WGS84 projection and coordinates in decimal degree.
#' @param grid_square Resolution for the global grid square (numeric value). You have just to provide the first value of the resolution. Check the section details below.
#' @param longitude_name Longitude column name in your data (character value).
#' @param latitude_name Latitude column name in your data (character value).
#' @param boundary_ajustement_factor Boundary adjustment factor is invoked for latitude values -90/90, longitude values -180/180, i.e. the limiting cases. The value does not matter unduly, so long as it is smaller than the size of the smallest square that will be requested.
#' @references \url{https://github.com/OB7-IRD/toolbox}
#' @return The function return your input dataframe with one more columns filled with the c-square value (according your specification in the "grid_square" argument).
#' @details
#' For the argument "grid_square", you can choose between 7 modalities:
#' \itemize{
#'  \item{10: }{for a grid with a resolution of 10x10 degrees}
#'  \item{5: }{for a grid with a resolution of 5x5 degrees}
#'  \item{1: }{for a grid with a resolution of 1x1 degrees}
#'  \item{0.5: }{for a grid with a resolution of 0.5x0.5 degrees}
#'  \item{0.1: }{for a grid with a resolution of 0.1x0.1 degrees}
#'  \item{0.05: }{for a grid with a resolution of 0.05x0.05 degrees}
#'  \item{0.01: }{for a grid with a resolution of 0.01x0.01 degrees}
#' }
#' This function have been developped regarding a MS Excel worksheet of Tony Rees (Tony.Rees@@csiro.au).
#' If you want more informations about C-square visit http://www.cmar.csiro.au/csquares/spec1-1.htm.
#' @examples
#' # Example for classification until division fao fishing area
#' tmp <- lat_long_to_csquare(data = balbaya_landing_rectangle,
#'                            grid_square = 0.5,
#'                            latitude_name = "latitude",
#'                            longitude_name = "longitude")
#' @export
lat_long_to_csquare <- function (data,
                                 grid_square,
                                 latitude_name,
                                 longitude_name,
                                 boundary_ajustement_factor = 0.000001) {
  tmp <- as.data.frame(data)
  # Boundary case ajustement
  tmp$boundary_aju_lat <- trunc(x = (abs(tmp[,c(latitude_name)]) + 10) / 100) * boundary_ajustement_factor
  tmp$boundary_aju_lon <- trunc(x = (abs(tmp[,c(longitude_name)]) + 20) / 200) * boundary_ajustement_factor
  # Absolute latitude and longtitude value
  tmp$abs_lat <- abs(x = tmp[,c(latitude_name)]) - tmp$boundary_aju_lat
  tmp$abs_lon <- abs(x = tmp[,c(longitude_name)]) - tmp$boundary_aju_lon
  # First cycle
  tmp$global_q <- 4 - (2 * trunc(x = 1 + (tmp[,c(longitude_name)] / 200)) - 1) * (2 * trunc(x = 1 + (tmp[,c(latitude_name)] / 200)) + 1)
  tmp$c1_lat <- trunc(x = tmp$abs_lat / 10)
  tmp$c1_lon <- trunc(x = tmp$abs_lon / 10)
  tmp$c1_lat_remain <- round(x = tmp$abs_lat - (tmp$c1_lat * 10),
                             7)
  tmp$c1_lon_remain <- round(x = tmp$abs_lon - (tmp$c1_lon * 10),
                             7)
  tmp$c1 <- (tmp$global_q * 1000) + (tmp$c1_lat * 100) + tmp$c1_lon
  # Second cycle
  tmp$global_intq1 <- (2 * trunc(x = tmp$c1_lat_remain * 0.2)) + trunc(x = tmp$c1_lon_remain * 0.2) + 1
  tmp$c2_lat <- trunc(x = tmp$c1_lat_remain)
  tmp$c2_lon <- trunc(x = tmp$c1_lon_remain)
  tmp$c2_lat_remain <- round(x = (tmp$c1_lat_remain - tmp$c2_lat) * 10,
                             7)
  tmp$c2_lon_remain <- round(x = (tmp$c1_lon_remain - tmp$c2_lon) * 10,
                             7)
  tmp$c2 <- (tmp$global_intq1 * 100) + (tmp$c2_lat * 10) + tmp$c2_lon
  # Third cycle
  tmp$global_intq2 <- (2 * trunc(x = tmp$c2_lat_remain * 0.2)) + trunc(x = tmp$c2_lon_remain * 0.2) + 1
  tmp$c3_lat <- trunc(x = tmp$c2_lat_remain)
  tmp$c3_lon <- trunc(x = tmp$c2_lon_remain)
  tmp$c3_lat_remain <- round(x = (tmp$c2_lat_remain - tmp$c3_lat) * 10,
                             digits = 7)
  tmp$c3_lon_remain <- round(x = (tmp$c2_lon_remain - tmp$c3_lon) * 10,
                             7)
  tmp$c3 <- (tmp$global_intq2 * 100) + (tmp$c3_lat * 10) + tmp$c3_lon
  # Fourth cycle
  tmp$global_intq3 <- (2 * trunc(x = tmp$c3_lat_remain * 0.2)) + trunc(x = tmp$c3_lon_remain * 0.2) + 1
  tmp$c4_lat <- trunc(x = tmp$c3_lat_remain)
  tmp$c4_lon <- trunc(x = tmp$c3_lon_remain)
  tmp$c4_lat_remain <- round(x = (tmp$c3_lat_remain - tmp$c4_lat) * 10,
                             7)
  tmp$c4_lon_remain <- round(x = (tmp$c3_lon_remain - tmp$c4_lon) * 10,
                             7)
  tmp$c4 <- (tmp$global_intq3 * 100) + (tmp$c4_lat * 10) + tmp$c4_lon
  # C-square design
  if (grid_square == 10) {
    tmp[, paste0("grid_square_", grid_square)] <- tmp$c1
  } else {
    if (grid_square == 5) {
      tmp[, paste0("grid_square_", grid_square)] <- paste(tmp$c1, substr(tmp$c2, 1, 1), sep = ":")
    } else {
      if (grid_square == 1) {
        tmp[, paste0("grid_square_", grid_square)] <- paste(tmp$c1, tmp$c2, sep = ":")
      } else {
        if (grid_square == 0.5) {
          tmp[, paste0("grid_square_", grid_square)] <- paste(tmp$c1, tmp$c2, substr(tmp$c3, 1, 1), sep = ":")
        } else {
          if (grid_square == 0.1) {
            tmp[, paste0("grid_square_", grid_square)] <- paste(tmp$c1, tmp$c2, tmp$c3, sep = ":")
          } else {
            if (grid_square == 0.05) {
              tmp[, paste0("grid_square_", grid_square)] <- paste(tmp$c1, tmp$c2, tmp$c3, substr(tmp$c4, 1, 1), sep = ":")
            } else {
              if (grid_square == 0.01) {
                tmp[, paste0("grid_square_", grid_square)] <- paste(tmp$c1, tmp$c2, tmp$c3, tmp$c4, sep = ":")
              }
            }
          }
        }
      }
    }
  }
  tmp <- tmp[, -c((dim(data)[2] + 1) : (dim(tmp)[2] - 1))]
  return(tmp)
}
