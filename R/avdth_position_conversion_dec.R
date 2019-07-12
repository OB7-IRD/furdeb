#' @name avdth_position_conversion_dec
#' @title AVDTH position conversion to decimal value
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @description Convert position format in AVDTH to decimal format.
#' @param data A R data frame.
#' @param latitude Column name of latitude data in text format.
#' @param longitude Column name of longitude data in text format.
#' @param quadrant Column name of quadrant data in text format.
#' @references \url{https://github.com/OB7-IRD/furdeb}
#' @return This function add two column to the input data frame, longitude_dec and latitude_dec, with longitude and latitude data in decimal format.
#' @export
avdth_position_conversion_dec <- function (data,
                                           latitude,
                                           longitude,
                                           quadrant) {
  if (missing(data)
      || ! is.data.frame(data)) {
    stop("Missing argument \"data\" or invalid format (data frame expected)\nPlease correct it before continuing")
  }

  if (missing(latitude)
      || ! is.character(latitude)
      || ! latitude %in% names(data)) {
    stop("Missing argument \"latitude\" or invalid format (character expected) or not present in the data frame\nPlease correct it before continuing")
  }

  if (missing(longitude)
      || ! is.character(longitude)
      || ! longitude %in% names(data)) {
    stop("Missing argument \"longitude\" or invalid format (character expected) or not present in the data frame\nPlease correct it before continuing")
  }

  if (missing(quadrant)
      || ! is.character(quadrant)
      || ! quadrant %in% names(data)) {
    stop("Missing argument \"quadrant\" or invalid format (character expected) or not present in the data frame\nPlease correct it before continuing")
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
