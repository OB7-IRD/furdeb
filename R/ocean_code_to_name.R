#' @name ocean_code_to_name
#' @title Ocean(s) name(s) creation
#' @description Ocean(s) name(s) creation in relation with oceans referential of the IRD Ob7 (Observatory of Exploited Tropical Pelagic Ecosystems).
#' @param ocean_code (numeric) Ocean(s) code(s).
#' @return A character vector in relation with the ocean(s) code(s) provided.
#' @examples
#' ocean_code_to_name(ocean_code = c(1, 2, 3))
#' @export
#' @importFrom dplyr last
#' @importFrom stringr str_split
ocean_code_to_name <- function (ocean_code) {
  # Arguments verification ----
  if (missing(ocean_code) || ! is.numeric(ocean_code)) {
    stop("invalid \"ocean_code\" argument")
  }
  # Function ----
  if (length(ocean_code) == 1) {
    if (ocean_code == 1) {
      ocean_name <- "Atlantic Ocean"
      ocean_code_chr <- "AO"
    } else {
      if (ocean_code == 2) {
        ocean_name <- "Indian Ocean"
        ocean_code_chr <- "IO"
      } else {
        if (ocean_code == 3) {
          ocean_name <- "West Pacific Ocean"
          ocean_code_chr <- "WPO"
        } else {
          if (ocean_code == 4) {
            ocean_name <- "East Pacific Ocean"
            ocean_code_chr <- "EPO"
          } else {
            if (ocean_code == 5) {
              ocean_name <- "Pacific Ocean"
              ocean_code_chr <- "PO"
            } else {
              if (ocean_code == 6) {
                ocean_name <- "Undetermined"
                ocean_code_chr <- "UND"
              }
            }
          }
        }
      }
    }
  } else {
    ocean_name <- NULL
    ocean_code_chr <- NULL
    for (i in ocean_code) {
      if (i == 1) {
        ocean_name <- ifelse(is.null(ocean_name),
                             "Atlantic, ",
                             paste0(ocean_name, "Atlantic, "))
        ocean_code_chr <- ifelse(is.null(ocean_code_chr),
                                 "AO, ",
                                 paste0(ocean_code_chr, "AO, "))
      } else {
        if (i == 2) {
          ocean_name <- ifelse(is.null(ocean_name),
                               "Indian, ",
                               paste0(ocean_name, "Indian, "))
          ocean_code_chr <- ifelse(is.null(ocean_code_chr),
                                   "IO, ",
                                   paste0(ocean_code_chr, "IO, "))
        } else {
          if (i == 3) {
            ocean_name <- ifelse(is.null(ocean_name),
                                 "West Pacific, ",
                                 paste0(ocean_name, "West Pacific, "))
            ocean_code_chr <- ifelse(is.null(ocean_code_chr),
                                     "WPO, ",
                                     paste0(ocean_code_chr, "WPO, "))
          } else {
            if (i == 4) {
              ocean_name <- ifelse(is.null(ocean_name),
                                   "East Pacific, ",
                                   paste0(ocean_name, "East Pacific, "))
              ocean_code_chr <- ifelse(is.null(ocean_code_chr),
                                       "EPO, ",
                                       paste0(ocean_code_chr, "EPO, "))
            } else {
              if (i == 5) {
                ocean_name <- ifelse(is.null(ocean_name),
                                     "Pacific, ",
                                     paste0(ocean_name, "Pacific, "))
                ocean_code_chr <- ifelse(is.null(ocean_code_chr),
                                         "PO, ",
                                         paste0(ocean_code_chr, "PO, "))
              } else {
                if (i == 6) {
                  ocean_name <- ifelse(is.null(ocean_name),
                                       "Undetermined, ",
                                       paste0(ocean_name, "Undetermined, "))
                  ocean_code_chr <- ifelse(is.null(ocean_code_chr),
                                           "UND, ",
                                           paste0(ocean_code_chr, "UND, "))
                }
              }
            }
          }
        }
      }
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
    }
  }
  ocean <- c("ocean_name" = ocean_name,
             "ocean_code_chr" = ocean_code_chr)
  return(ocean)
}
