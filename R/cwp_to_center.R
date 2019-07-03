#' @name cwp_to_center
#' @title Conversion of cwp to latitude and longitude (related to cwp centroid)
#' @author Antoine Duparc, \email{antoie.duparc@@ird.fr}
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @description Conversion of CWP to latitude and longitude (in decimal degrees). Be careful, latitude and longitude are related to the cwp centroid.
#' @param data A R data frame with at least one column with cwp data.
#' @param cwp_name Column name of data data in text format.
#' @param cwp_length Length of cwp. For example, for a square of 1°x1° enter 1.
#' @references \url{https://github.com/OB7-IRD/toolbox}
#' @return This function add four columns to the input data frame: cwp, quadrat, longitude_dec and latitude_dec (with longitude and latitude data in decimal format).
#' @export
cwp_to_center <- function(data,
                          cwp_name,
                          cwp_length)
                        {
  if (missing(data)
      || ! is.data.frame(data)) {
    stop("Missing argument \"data\" or invalid format (data frame expected)\nPlease correct it before continuing")
  }
  if (missing(cwp_name)
      || ! is.character(cwp_name)
      || ! cwp_name %in% names(data)) {
    stop("Missing argument \"cwp_name\" or invalid format (character expected) or not present in the data frame\nPlease correct it before continuing")
  }
  if (missing(cwp_length)
      || ! is.numeric(cwp_length)) {
    stop("Missing argument \"cwp_length\" or invalid format (numeric expected)\nPlease correct it before continuing")
  }
  tmp <- unique(data.frame(cwp = data[, cwp_name])) %>%
    rowwise() %>%
    mutate(quadrat = as.numeric(substring(cwp,
                                          first = 1,
                                          last = 1)),
           latitude_dec = ifelse(quadrat %in% c(2, 3),
                                 - (as.numeric(substring(cwp,
                                                         first = 2,
                                                         last = 3)) + (cwp_length / 2)),
                                 as.numeric(substring(cwp,
                                                      first = 2,
                                                      last = 3) + (cwp_length / 2))),
           longitude_dec = ifelse(quadrat %in% c(3, 4),
                                  - (as.numeric(substring(cwp,
                                                          first = 4,
                                                          last = 6)) + (cwp_length / 2)),
                                  as.numeric(substring(cwp,
                                                       first = 4,
                                                       last = 6)) + (cwp_length / 2))) %>%
    right_join(data,
               by = c("cwp" = cwp_name))
  return(tmp)
}
