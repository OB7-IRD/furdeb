#' @name lat_long_to_csquare
#' @title Latitude-longitude to c-square converter
#' @description Latitude-longitude to c-square converter.
#' @param data R data with at least two columns with longitude and latitude values. Be careful! Your longitude and latitude data have to be in the WGS84 projection and coordinates in decimal degree.
#' @param grid_square {\link[base]{numeric}} expected. Resolution for the global grid square. You have just to provide the first value of the resolution. Check the section details below.
#' @param longitude_name {\link[base]{character}} expected. Longitude column name in your data.
#' @param latitude_name {\link[base]{character}} expected. Latitude column name in your data.
#' @param boundary_ajustement_factor {\link[base]{numeric}} expected. By default 0.000001. Boundary adjustment factor is invoked for latitude values -90/90, longitude values -180/180, i.e. the limiting cases. The value does not matter unduly, so long as it is smaller than the size of the smallest square that will be requested.
#' @return The function return your input data frame with one more columns filled with the c-square value (according your specification in the "grid_square" argument).
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
#' \dontrun{
#' tmp <- lat_long_to_csquare(data = balbaya_landing_rectangle,
#'                            grid_square = 0.5,
#'                            latitude_name = "latitude",
#'                            longitude_name = "longitude")}
#' @importFrom codama r_type_checking
#' @export
lat_long_to_csquare <- function(data,
                                grid_square,
                                latitude_name,
                                longitude_name,
                                boundary_ajustement_factor = 0.000001) {
  # 1 - Arguments verification ----
  # data argument checking
  if (missing(x = data)) {
    return(format(x = Sys.time(),
                  "%Y-%m-%d %H:%M:%S"),
           " - Error, invalid \"data\" argument.\n")
  }
  # grid_square argument checking
  if (codama::r_type_checking(r_object = grid_square,
                              type = "numeric",
                              allowed_value = c(10, 5, 1, 0.5, 0.1, 0.05, 0.01),
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = grid_square,
                                   type = "numeric",
                                   allowed_value = c(10, 5, 1, 0.5, 0.1, 0.05, 0.01),
                                   output = "message"))
  }
  # latitude_name argument checking
  if (codama::r_type_checking(r_object = latitude_name,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = latitude_name,
                                   type = "character",
                                   output = "message"))
  }
  # longitude_name argument checking
  if (codama::r_type_checking(r_object = longitude_name,
                              type = "character",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = longitude_name,
                                   type = "character",
                                   output = "message"))
  }
  # boundary_ajustement_factor argument checking
  if (codama::r_type_checking(r_object = boundary_ajustement_factor,
                              type = "numeric",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = boundary_ajustement_factor,
                                   type = "numeric",
                                   output = "message"))
  }
  # 2 - Process ----
  data <- as.data.frame(data)
  # boundary case ajustement
  data$boundary_aju_lat <- trunc(x = (abs(data[, c(latitude_name)]) + 10) / 100) * boundary_ajustement_factor
  data$boundary_aju_lon <- trunc(x = (abs(data[, c(longitude_name)]) + 20) / 200) * boundary_ajustement_factor
  # absolute latitude and longtitude value
  data$abs_lat <- abs(x = data[, c(latitude_name)]) - data$boundary_aju_lat
  data$abs_lon <- abs(x = data[, c(longitude_name)]) - data$boundary_aju_lon
  # first cycle
  data$global_q <- 4 - (2 * trunc(x = 1 + (data[, c(longitude_name)] / 200)) - 1) * (2 * trunc(x = 1 + (data[, c(latitude_name)] / 200)) + 1)
  data$c1_lat <- trunc(x = data$abs_lat / 10)
  data$c1_lon <- trunc(x = data$abs_lon / 10)
  data$c1_lat_remain <- round(x = data$abs_lat - (data$c1_lat * 10),
                              7)
  data$c1_lon_remain <- round(x = data$abs_lon - (data$c1_lon * 10),
                              7)
  data$c1 <- (data$global_q * 1000) + (data$c1_lat * 100) + data$c1_lon
  # second cycle
  data$global_intq1 <- (2 * trunc(x = data$c1_lat_remain * 0.2)) + trunc(x = data$c1_lon_remain * 0.2) + 1
  data$c2_lat <- trunc(x = data$c1_lat_remain)
  data$c2_lon <- trunc(x = data$c1_lon_remain)
  data$c2_lat_remain <- round(x = (data$c1_lat_remain - data$c2_lat) * 10,
                              7)
  data$c2_lon_remain <- round(x = (data$c1_lon_remain - data$c2_lon) * 10,
                              7)
  data$c2 <- (data$global_intq1 * 100) + (data$c2_lat * 10) + data$c2_lon
  # third cycle
  data$global_intq2 <- (2 * trunc(x = data$c2_lat_remain * 0.2)) + trunc(x = data$c2_lon_remain * 0.2) + 1
  data$c3_lat <- trunc(x = data$c2_lat_remain)
  data$c3_lon <- trunc(x = data$c2_lon_remain)
  data$c3_lat_remain <- round(x = (data$c2_lat_remain - data$c3_lat) * 10,
                              digits = 7)
  data$c3_lon_remain <- round(x = (data$c2_lon_remain - data$c3_lon) * 10,
                              7)
  data$c3 <- (data$global_intq2 * 100) + (data$c3_lat * 10) + data$c3_lon
  # fourth cycle
  data$global_intq3 <- (2 * trunc(x = data$c3_lat_remain * 0.2)) + trunc(x = data$c3_lon_remain * 0.2) + 1
  data$c4_lat <- trunc(x = data$c3_lat_remain)
  data$c4_lon <- trunc(x = data$c3_lon_remain)
  data$c4_lat_remain <- round(x = (data$c3_lat_remain - data$c4_lat) * 10,
                              7)
  data$c4_lon_remain <- round(x = (data$c3_lon_remain - data$c4_lon) * 10,
                              7)
  data$c4 <- (data$global_intq3 * 100) + (data$c4_lat * 10) + data$c4_lon
  # c-square design
  if (grid_square == 10) {
    data[, paste0("grid_square_",
                  grid_square)] <- data$c1
  } else {
    if (grid_square == 5) {
      data[, paste0("grid_square_",
                    grid_square)] <- paste(data$c1,
                                           substr(data$c2,
                                                  1,
                                                  1),
                                           sep = ":")
    } else {
      if (grid_square == 1) {
        data[, paste0("grid_square_",
                      grid_square)] <- paste(data$c1,
                                             data$c2,
                                             sep = ":")
      } else {
        if (grid_square == 0.5) {
          data[, paste0("grid_square_",
                        grid_square)] <- paste(data$c1,
                                               data$c2,
                                               substr(data$c3,
                                                      1,
                                                      1),
                                               sep = ":")
        } else {
          if (grid_square == 0.1) {
            data[, paste0("grid_square_",
                          grid_square)] <- paste(data$c1,
                                                 data$c2,
                                                 data$c3,
                                                 sep = ":")
          } else {
            if (grid_square == 0.05) {
              data[, paste0("grid_square_",
                            grid_square)] <- paste(data$c1,
                                                   data$c2,
                                                   data$c3,
                                                   substr(data$c4,
                                                          1,
                                                          1),
                                                   sep = ":")
            } else {
              if (grid_square == 0.01) {
                data[, paste0("grid_square_",
                              grid_square)] <- paste(data$c1,
                                                     data$c2,
                                                     data$c3,
                                                     data$c4,
                                                     sep = ":")
              }
            }
          }
        }
      }
    }
  }
  data <- data[, -c((dim(data)[2] + 1) : (dim(data)[2] - 1))]
  return(data)
}
