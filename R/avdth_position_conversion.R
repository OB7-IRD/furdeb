#' @name avdth_position_conversion
#' @title AVDTH database position conversion
#' @description Convert position format in AVDTH to decimal format.
#' @param data {\link[base]{data.frame}} expected. A R data frame.
#' @param latitude {\link[base]{character}} expected. Column name of latitude data.
#' @param longitude {\link[base]{character}} expected. Column name of longitude data.
#' @param quadrant {\link[base]{character}} expected. Column name of quadrant data.
#' @return This function add two column to the input data frame, longitude_dec and latitude_dec, with longitude and latitude data in decimal format.
#' @export
avdth_position_conversion <- function(data,
                                      latitude,
                                      longitude,
                                      quadrant) {
  # 1 - Arguments verification ----
  codama::r_type_checking(r_object = data,
                          type = "data.frame")
  codama::r_type_checking(r_object = latitude,
                          type = "character",
                          length = 1L)
  if (! latitude %in% names(x = data)) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Column \"",
         latitude,
         "\" not present in the input data.")
  }
  codama::r_type_checking(r_object = longitude,
                          type = "character",
                          length = 1L)
  if (! longitude %in% names(x = data)) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Column \"",
         longitude,
         "\" not present in the input data.")
  }
  codama::r_type_checking(r_object = quadrant,
                          type = "character",
                          length = 1L)
  if (! quadrant %in% names(data)) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Column \"",
         quadrant,
         "\" not present in the input data.")
  }
  # 2 - Global process ----
  data[, "longitude_dec"] <- ifelse(data[, quadrant] %in% c(1, 2),
                                    trunc(data[, longitude] * (10 ^ -2)) + ((data[, longitude] * (10 ^ -2) - trunc(data[, longitude] * (10 ^ -2))) / 60 * 100),
                                    as.numeric(paste0("-",
                                                      trunc(data[, longitude] * (10 ^ -2)) + ((data[, longitude] * (10 ^ -2) - trunc(data[, longitude] * (10 ^ -2))) / 60 * 100))))
  data[, "latitude_dec"] <- ifelse(data[, quadrant] %in% c(1, 4),
                                   trunc(data[, latitude] * (10 ^ -2)) + ((data[, latitude] * (10 ^ -2) - trunc(data[, latitude] * (10 ^ -2))) / 60 * 100),
                                   as.numeric(paste0("-",
                                                     trunc(data[, latitude] * (10 ^ -2)) + ((data[, latitude] * (10 ^ -2) - trunc(data[, latitude] * (10 ^ -2))) / 60 * 100))))
  return(data)
}
