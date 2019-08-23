#' @title Fishing mode(s) name(s) creation
#' @description Fishing mode(s) name(s) creation in relation with fishing mode referential of the IRD Ob7 (Observatory of Exploited Tropical Pelagic Ecosystems).
#' @name fishing_mode_to_name
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @param fishing_mode_code Fishing mode code(s) code(s) in numerical values.
#' @param detail If you want to display all the fishing modes name or a shortcut with "all fishing modes". By default TRUE.
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
      fishing_mode_code_chr <- "FOB"
    } else {
      if (fishing_mode_code == 2) {
        fishing_mode_name <- "free school"
        fishing_mode_code_chr <- "FSC"
      } else {
        if (fishing_mode_code == 3) {
          fishing_mode_name <- "undetermined school"
          fishing_mode_code_chr <- "UND"
        }
      }
    }
  } else {
    fishing_mode_name <- NULL
    fishing_mode_code_chr <- NULL
    for (i in fishing_mode_code) {
      if (i == 1) {
        fishing_mode_name <- ifelse(is.null(fishing_mode_name),
                                    "floating object, ",
                                    paste0(fishing_mode_name, "floating object, "))
        fishing_mode_code_chr <- ifelse(is.null(fishing_mode_code_chr),
                                        "FOB, ",
                                        paste0(fishing_mode_code_chr, "FOB, "))
      } else {
        if (i == 2) {
          fishing_mode_name <- ifelse(is.null(fishing_mode_name),
                                      "free school, ",
                                      paste0(fishing_mode_name, "free school, "))
          fishing_mode_code_chr <- ifelse(is.null(fishing_mode_code_chr),
                                          "FSC, ",
                                          paste0(fishing_mode_code_chr, "FSC, "))
        } else {
          if (i == 3) {
            fishing_mode_name <- ifelse(is.null(fishing_mode_name),
                                        "undetermined school, ",
                                        paste0(fishing_mode_name, "undetermined school, "))
            fishing_mode_code_chr <- ifelse(is.null(fishing_mode_code_chr),
                                            "UND, ",
                                            paste0(fishing_mode_code_chr, "UND, "))
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
      tmp <- stringr::str_split(string = fishing_mode_code_chr, pattern = ", ", simplify = TRUE)
      tmp <- tmp[tmp != ""]
      fishing_mode_code_chr <- paste0(paste(tmp[tmp != dplyr::last(tmp)],
                                            collapse = ", "),
                                      " and ",
                                      dplyr::last(tmp))
    }
  }
  fishing_mode <- c("fishing_mode_name" = fishing_mode_name,
                    "fishing_mode_code_chr" = fishing_mode_code_chr)
  return(fishing_mode)
}
