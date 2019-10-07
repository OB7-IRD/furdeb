#' @name cwp_to_center
#' @title Conversion of cwp to latitude and longitude (related to cwp centroid)
#' @description Conversion of CWP to latitude and longitude (in decimal degrees). Be careful, latitude and longitude are related to the cwp centroid.
#' @param data (data.frame) A R data frame with at least one column with cwp data.
#' @param cwp_name (character) Column name of cwp data.
#' @param cwp_length (numeric) Length of cwp. For example, for a square of 1°x1° enter 1.
#' @return This function add four columns to the input data frame: cwp, quadrat, longitude_dec and latitude_dec (with longitude and latitude data in decimal format).
#' @export
#' @importFrom dplyr rowwise mutate right_join
cwp_to_center <- function(data,
                          cwp_name,
                          cwp_length)
                        {
  if (missing(data)
      || ! is.data.frame(data)) {
    stop("invalid \"data\" argument")
  }
  if (missing(cwp_name)
      || ! is.character(cwp_name)
      || ! cwp_name %in% names(data)) {
    stop("invalid \"cwp_name\" argument")
  }
  if (missing(cwp_length)
      || ! is.numeric(cwp_length)) {
    stop("invalid \"cwp_length\" argument")
  }
  backup <- getOption("scipen")
  options(scipen = 999)
  tmp <- unique(data.frame(cwp = data[, cwp_name])) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(quadrat = as.numeric(substring(cwp,
                                                 first = 1,
                                                 last = 1)),
                  latitude_dec = ifelse(quadrat %in% c(2, 3),
                                        - (as.numeric(substring(cwp,
                                                                first = 2,
                                                                last = 3)) + (cwp_length / 2)),
                                        as.numeric(substring(cwp,
                                                             first = 2,
                                                             last = 3)) + (cwp_length / 2)),
                  longitude_dec = ifelse(quadrat %in% c(3, 4),
                                         - (as.numeric(substring(cwp,
                                                                 first = 4,
                                                                 last = 6)) + (cwp_length / 2)),
                                         as.numeric(substring(cwp,
                                                              first = 4,
                                                              last = 6)) + (cwp_length / 2))) %>%
    dplyr::right_join(data,
                      by = c("cwp" = cwp_name))
  options(scipen = backup)
  return(tmp)
}
