#' @name avdth_position_conversion_dec
#' @title AVDTH position conversion to decimal value
#' @description Convert position format in AVDTH to decimal format.
#' @param data (data.frame) A R data frame.
#' @param latitude (character) Column name of latitude data.
#' @param longitude (character) Column name of longitude data.
#' @param quadrant (character) Column name of quadrant data.
#' @return This function add two column to the input data frame, longitude_dec and latitude_dec, with longitude and latitude data in decimal format.
#' @export
avdth_position_conversion_dec <- function (data,
                                           latitude,
                                           longitude,
                                           quadrant) {
  if (missing(data)
      || ! is.data.frame(data)) {
    stop("invalid \"data\" argument")
  }

  if (missing(latitude)
      || ! is.character(latitude)
      || ! latitude %in% names(data)) {
    stop("invalid \"latitude\" argument")
  }

  if (missing(longitude)
      || ! is.character(longitude)
      || ! longitude %in% names(data)) {
    stop("invalid \"longitude\" argument")
  }

  if (missing(quadrant)
      || ! is.character(quadrant)
      || ! quadrant %in% names(data)) {
    stop("invalid \"quadrant\" argument")
  }

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
