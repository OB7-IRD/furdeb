#' @name latitude_longitude_to_cwp
#' @title Conversion of latitude and longitude (in decimal degrees) to cwp.
#' @description Conversion of latitude and longitude (in decimal degrees) to cwp.
#' @param data (data.frame) A R data frame with at least two column with latitude and longtitude data.
#' @param latitude_name (character) Column name of latitude data (display in decimal degrees).
#' @param longtitude_name (character) Column name of longitude data (display in decimal degrees).
#' @param cwp_length (numeric) Length of cwp. For example, for a square of 1°x1° enter 1.
latitude_longitude_to_cwp <- function(data,
                                      latitude_name,
                                      longtitude_name,
                                      cwp_length) {
  if (missing(data)
      || class(data) != "data.frame") {
    stop("invalid \"data\" argument, class \"data.frame\" expected.\n")
  } else if (missing(latitude_name)
             || class(latitude_name) != "character"
             || length(latitude_name) != 1) {
    stop("invalid \"latitude_name\" argument, class \"character\" expected with one value.\n")
  } else if (missing(longtitude_name)
             || class(longtitude_name) != "character"
             || length(longtitude_name) != 1) {
    stop("invalid \"longtitude_name\" argument, class \"character\" expected with one value.\n")
  } else if (missing(cwp_length)
             || class(cwp_length) != "numeric"
             || length(cwp_length) != 1) {
    stop("invalid \"cwp_length\" argument, class \"numeric\" expected with one value.\n")
  } else {
    backup_scipen <- getOption("scipen")
    on.exit(options(scipen = backup_scipen))
    options(scipen = 999)

    tmp <- unique(data.frame(latitude = data[, latitude_name],
                             longitude = data[, longitude_name])) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(quadrat = as.integer(x = ifelse(test = longitude > 0,
                                                    yes = ,
                                                    no = )),
                    quadrat = as.numeric(substring(cwp,
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

    quad<-ifelse(lon>0,
                 ifelse(lat>0,
                        1,
                        2),
                 ifelse(lat>0,
                        4,
                        3)) # define quadrant
    lat_tmp<-ifelse(quad %in% c(1,4),sprintf("%02d",abs(mtrunc(lat,base))),sprintf("%02d",abs(mroundup(lat,base))))
    lon_tmp<-ifelse(quad %in% c(1,2),sprintf("%03d",abs(mtrunc(lon,base))),sprintf("%03d",abs(mroundup(lon,base))))
    return(paste(quad,lat_tmp,lon_tmp,sep=""))


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
  }
}
