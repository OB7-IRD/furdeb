#' @name ocean_code_to_name
#' @title Ocean(s) name(s) creation
#' @description Ocean(s) name(s) creation in relation with oceans referential of the IRD Ob7 (Observatory of Exploited Tropical Pelagic Ecosystems).
#' @param ocean_code {\link[base]{numeric}} expected. Ocean(s) code(s).
#' @return A list with ocean name(s), ocean character code(s), maximum and minimum latitude and longitude of ocean(s) selected.
#' @examples
#' ocean_code_to_name(ocean_code = c(1, 2, 3))
#' @export
#' @importFrom dplyr last
#' @importFrom stringr str_split
ocean_code_to_name <- function (ocean_code) {
  # arguments verification ----
  if (missing(ocean_code) || any(abs(ocean_code - trunc(ocean_code)) > 0) || any(! ocean_code %in% c(1, 2, 3, 4, 5, 6))) {
    stop("invalid \"ocean_code\" argument")
  }
  # function ----
  if (length(ocean_code) == 1) {
    if (ocean_code == 1) {
      ocean_name <- "Atlantic Ocean"
      ocean_code_chr <- "AO"
      ocean_lon_min_max <- c(-40, 20)
      ocean_lat_min_max <- c(-25, 25)
    } else if (ocean_code == 2) {
      ocean_name <- "Indian Ocean"
      ocean_code_chr <- "IO"
      ocean_lon_min_max <- c(20, 120)
      ocean_lat_min_max <- c(-35, 20)
    } else if (ocean_code == 3) {
      ocean_name <- "West Pacific Ocean"
      ocean_code_chr <- "WPO"
      ocean_lon_min_max <- stop("need to define ", ocean_name, " borderlines")
      ocean_lat_min_max <- stop("need to define ", ocean_name, " borderlines")
    } else if (ocean_code == 4) {
      ocean_name <- "East Pacific Ocean"
      ocean_code_chr <- "EPO"
      ocean_lon_min_max <- stop("need to define ", ocean_name, " borderlines")
      ocean_lat_min_max <- stop("need to define ", ocean_name, " borderlines")
    } else if (ocean_code == 5) {
      ocean_name <- "Pacific Ocean"
      ocean_code_chr <- "PO"
      ocean_lon_min_max <- stop("need to define ", ocean_name, " borderlines")
      ocean_lat_min_max <- stop("need to define ", ocean_name, " borderlines")
    } else if (ocean_code == 6) {
      ocean_name <- "Undetermined"
      ocean_code_chr <- "UND"
      ocean_lon_min_max <- c(-180, 180)
      ocean_lat_min_max <- c(-90, 90)
    }
  } else {
    ocean_name <- NULL
    ocean_code_chr <- NULL
    ocean_lon_min_max <- NULL
    ocean_lat_min_max <- NULL
    for (i in ocean_code) {
      if (i == 1) {
        ocean_name <- ifelse(is.null(ocean_name),
                             "Atlantic, ",
                             paste0(ocean_name, "Atlantic, "))
        ocean_code_chr <- ifelse(is.null(ocean_code_chr),
                                 "AO, ",
                                 paste0(ocean_code_chr, "AO, "))
        ocean_lon_min_max <- append(ocean_lon_min_max, c(-40, 20))
        ocean_lat_min_max <- append(ocean_lat_min_max, c(-25, 25))
      } else if (i == 2) {
        ocean_name <- ifelse(is.null(ocean_name),
                             "Indian, ",
                             paste0(ocean_name, "Indian, "))
        ocean_code_chr <- ifelse(is.null(ocean_code_chr),
                                 "IO, ",
                                 paste0(ocean_code_chr, "IO, "))
        ocean_lon_min_max <- append(ocean_lon_min_max, c(20, 120))
        ocean_lat_min_max <- append(ocean_lat_min_max, c(-35, 20))
      } else if (i == 3) {
        ocean_name <- ifelse(is.null(ocean_name),
                             "West Pacific, ",
                             paste0(ocean_name, "West Pacific, "))
        ocean_code_chr <- ifelse(is.null(ocean_code_chr),
                                 "WPO, ",
                                 paste0(ocean_code_chr, "WPO, "))
        ocean_lon_min_max <- stop("need to define West Pacific Ocean borderlines")
        ocean_lat_min_max <- stop("need to define West Pacific Ocean borderlines")
      } else if (i == 4) {
        ocean_name <- ifelse(is.null(ocean_name),
                             "East Pacific, ",
                             paste0(ocean_name, "East Pacific, "))
        ocean_code_chr <- ifelse(is.null(ocean_code_chr),
                                 "EPO, ",
                                 paste0(ocean_code_chr, "EPO, "))
        ocean_lon_min_max <- stop("need to define East Pacific Ocean borderlines")
        ocean_lat_min_max <- stop("need to define East Pacific Ocean borderlines")
      } else if (i == 5) {
        ocean_name <- ifelse(is.null(ocean_name),
                             "Pacific, ",
                             paste0(ocean_name, "Pacific, "))
        ocean_code_chr <- ifelse(is.null(ocean_code_chr),
                                 "PO, ",
                                 paste0(ocean_code_chr, "PO, "))
        ocean_lon_min_max <- stop("need to define Pacific Ocean borderlines")
        ocean_lat_min_max <- stop("need to define Pacific Ocean borderlines")
      } else if (i == 6) {
        ocean_name <- ifelse(is.null(ocean_name),
                             "Undetermined, ",
                             paste0(ocean_name, "Undetermined, "))
        ocean_code_chr <- ifelse(is.null(ocean_code_chr),
                                 "UND, ",
                                 paste0(ocean_code_chr, "UND, "))
        ocean_lon_min_max <- append(ocean_lon_min_max, c(-180, 180))
        ocean_lat_min_max <- append(ocean_lat_min_max, c(-90, 90))
      }
      if (i == dplyr::last(ocean_code)) {
        tmp <- stringr::str_split(string = ocean_name, pattern = ", ", simplify = TRUE)
        tmp <- tmp[tmp != ""]
        ocean_name <- paste0(paste(tmp[tmp != dplyr::last(tmp)],
                                   collapse = ", "),
                             " and ",
                             dplyr::last(tmp),
                             " Oceans")
        tmp <- stringr::str_split(string = ocean_code_chr, pattern = ", ", simplify = TRUE)
        tmp <- tmp[tmp != ""]
        ocean_code_chr <- paste0(paste(tmp[tmp != dplyr::last(tmp)],
                                       collapse = ", "),
                                 " and ",
                                 dplyr::last(tmp))
        ocean_lon_min_max <- c(min(ocean_lon_min_max), max(ocean_lon_min_max))
        ocean_lat_min_max <- c(min(ocean_lat_min_max), max(ocean_lat_min_max))
      }
    }
  }
  ocean <- list("ocean_name" = ocean_name,
                "ocean_code_chr" = ocean_code_chr,
                "ocean_lon_min_max" = ocean_lon_min_max,
                "ocean_lat_min_max" = ocean_lat_min_max)
  return(ocean)
}
