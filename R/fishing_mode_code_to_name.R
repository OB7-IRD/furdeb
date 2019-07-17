#' @title Fishing mode(s) name(s) creation
#' @description Fishing mode(s) name(s) creation in relation with fishing mode referential of the IRD Ob7 (Observatory of Exploited Tropical Pelagic Ecosystems).
#' @name fishing_mode_to_name
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @param fishing_mode_code Fishing mode code(s) code(s) in numerical values.
#' @references \url{https://github.com/OB7-IRD/furdeb}
#' @return A character vector in relation with the fishing mode(s) code(s) provided.
#' @export
#' @examples
#' tmp <- fishing_mode_to_name(ocean_code = c(1, 2, 3))
fishing_mode_to_name <- function (fishing_mode_code) {
  # Arguments verification ----
  if (missing(fishing_mode_code) || ! is.numeric(fishing_mode_code)) {
    stop("Missing argument \"fishing_mode_code\" or not numeric value(s).",
         "\n",
         "Please correct it before running the function.")
  }

  # Function ----
  if (length(fishing_mode_code) == 1) {
    if (fishing_mode_code == 1) {
      fishing_mode_name <- "floating object"
    } else {
      if (fishing_mode_code == 2) {
        fishing_mode_name <- "free school"
      } else {
        if (fishing_mode_code == 3) {
          fishing_mode_name <- "undetermined school"
        }
      }
    }
  } else {
    fishing_mode_name <- NULL
    for (i in fishing_mode_code) {
      if (i == 1) {
        fishing_mode_name <- ifelse(is.null(fishing_mode_name),
                                    "floating object, ",
                                    paste0(fishing_mode_name, "floating object, "))
      } else {
        if (i == 2) {
          fishing_mode_name <- ifelse(is.null(fishing_mode_name),
                                      "free school, ",
                                      paste0(fishing_mode_name, "free school, "))
        } else {
          if (i == 3) {
            fishing_mode_name <- ifelse(is.null(fishing_mode_name),
                                        "undetermined school, ",
                                        paste0(fishing_mode_name, "undetermined school, "))
          }
        }
      }
    }
    if (i == dplyr::last(fishing_mode_code)) {
      tmp <- stringr::str_split(string = fishing_mode_name, pattern = ", ", simplify = TRUE)
      tmp <- tmp[tmp != ""]
      fishing_mode_name <- paste0(paste(tmp[tmp != dplyr::last(tmp)],
                                        collapse = ", "),
                                  " and ",
                                  dplyr::last(tmp))
    }
  }
  return(fishing_mode_name)
}
