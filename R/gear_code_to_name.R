#' @name gear_code_to_name
#' @title Gear(s) name(s) creation
#' @description Gear(s) name(s) creation in relation with gears referential of the IRD Ob7 (Observatory of Exploited Tropical Pelagic Ecosystems).
#' @param gear_code (numeric) Gear(s) code(s).
#' @return A character vector in relation with the gear(s) code(s) provided.
#' @examples
#' gear_code_to_name(gear_code = c(1, 2, 3))
#' @export
#' @importFrom dplyr last
#' @importFrom stringr str_split
gear_code_to_name <- function(gear_code) {
  # Arguments verification ----
  if (missing(gear_code) || ! is.numeric(gear_code)) {
    stop("invalid \"gear_code\" argument")
  }
  # Function ----
  if (length(gear_code) == 1) {
    if (gear_code == 1) {
      gear_name <- "Purse seine"
      gear_code_chr <- "PS"
    } else {
      if (gear_code == 2) {
        gear_name <- "Bail boat"
        gear_code_chr <- "BB"
      } else {
        if (gear_code == 3) {
          gear_name <- "Longliner"
          gear_code_chr <- "LL"
        } else {
          if (gear_code == 4) {
            gear_name <- "Supply"
            gear_code_chr <- "NK"
          } else {
            if (gear_code == 5) {
              gear_name <- "Mix"
              gear_code_chr <- "MIS"
            }
          }
        }
      }
    }
  } else {
    gear_name <- NULL
    gear_code_chr <- NULL
    for (i in gear_code) {
      if (i == 1) {
        gear_name <- ifelse(is.null(gear_name),
                            "Purse seine, ",
                            paste0(gear_name, "Purse seine, "))
        gear_code_chr <- ifelse(is.null(gear_code_chr),
                                "PS, ",
                                paste0(gear_code_chr, "PS, "))
      } else {
        if (i == 2) {
          gear_name <- ifelse(is.null(gear_name),
                              "Bail boat, ",
                              paste0(gear_name, "Bail boat, "))
          gear_code_chr <- ifelse(is.null(gear_code_chr),
                                  "BB, ",
                                  paste0(gear_code_chr, "BB, "))
        } else {
          if (i == 3) {
            gear_name <- ifelse(is.null(gear_name),
                                "Longliner, ",
                                paste0(gear_name, "Longliner, "))
            gear_code_chr <- ifelse(is.null(gear_code_chr),
                                    "LL, ",
                                    paste0(gear_code_chr, "LL, "))
          } else {
            if (i == 4) {
              gear_name <- ifelse(is.null(gear_name),
                                  "Supply, ",
                                  paste0(gear_name, "Supply, "))
              gear_code_chr <- ifelse(is.null(gear_code_chr),
                                      "NK, ",
                                      paste0(gear_code_chr, "NK, "))
            } else {
              if (i == 5) {
                gear_name <- ifelse(is.null(gear_name),
                                    "Mix, ",
                                    paste0(gear_name, "Mix, "))
                gear_code_chr <- ifelse(is.null(gear_code_chr),
                                        "MIS, ",
                                        paste0(gear_code_chr, "MIS, "))
              }
            }
          }
        }
      }
    }
    if (i == dplyr::last(gear_code)) {
      tmp <- stringr::str_split(string = gear_name, pattern = ", ", simplify = TRUE)
      tmp <- tmp[tmp != ""]
      gear_name <- paste0(paste(tmp[tmp != dplyr::last(tmp)],
                                 collapse = ", "),
                           " and ",
                           dplyr::last(tmp))
      tmp <- stringr::str_split(string = gear_code_chr, pattern = ", ", simplify = TRUE)
      tmp <- tmp[tmp != ""]
      gear_code_chr <- paste0(paste(tmp[tmp != dplyr::last(tmp)],
                                    collapse = ", "),
                              " and ",
                              dplyr::last(tmp))
    }
  }
  gear <- c("gear_name" = gear_name,
            "gear_code_chr" = gear_code_chr)
  return(gear)
}
