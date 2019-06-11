#' @title Interpolate values into a SQL string query
#' @description Interpolate values into a SQL string query.
#' @name sql_inset
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @param db_type Type of database related to the query. For now, the function accepts only "access" and "postgresql" databases.
#' @param replacement Replacement argument for matched pattern. If you want to specify multiple arguments, use the function c().
#' @param pattern Character string containing a string expression to be matched.
#' @param query A character vector where matches are sought, or an object which can be coerced by as.character to a character vector.
#' @references \url{https://github.com/OB7-IRD/toolbox}
#' @return A vector contain the query with replacement value(s).
#' @examples
#' final_query <- sql_inset(db_type = "access", replacement = 2017, pattern = "year_interpolate", query = intial_query)
#' @export
sql_inset <- function (db_type,
                       replacement,
                       pattern,
                       query) {
  type <- mode(x = replacement)
  if (type == "numeric") {
    if (length(replacement) == 1) {
      tmp <- replacement
    } else {
      tmp <- NULL
      for (i in 1:length(replacement)) {
        if (is.null(tmp)) {
          tmp <- replacement[i]
        } else {
          tmp <- paste(tmp,
                       replacement[i],
                       sep = ', ')
        }
        if (i == length(replacement)) {
          tmp <- paste0("(",
                        tmp,
                        ")")
        }
      }
    }
  } else {
    if (type == "character") {
      if (length(replacement) == 1) {
        if (db_type == "access") {
          tmp <- paste0("'",
                        replacement,
                        "'")
        } else {
          if (db_type == "postgresql") {
            tmp <- paste0("\"",
                          replacement,
                          "\"")
          } else {
            stop("Your kind database type is not supported yet.")
          }
        }
      } else {
        if (db_type == "access") {
          tmp <- NULL
          for (i in 1:length(replacement)) {
            if (is.null(tmp)) {
              tmp <- paste0("'",
                            replacement[i],
                            "'")
            } else {
              tmp <- paste(tmp,
                           paste0("'",
                                  replacement[i],
                                  "'"),
                           sep = ', ')
            }
          }
        } else {
          if (db_type == "postgresql") {
            tmp <- NULL
            for (i in 1:length(replacement)) {
              if (is.null(tmp)) {
                tmp <- paste0("\"",
                              replacement[i],
                              "\"")
              } else {
                tmp <- paste(tmp,
                             paste0("\"",
                                    replacement[i],
                                    "\""),
                             sep = ', ')
              }
            }
          } else {
            stop("Your kind database type is not supported yet.")
          }
        }
        tmp <- paste0("(",
                      tmp,
                      ")")
      }
    } else {
      stop(paste0("Be careful, replacement argument if not a numeric/character types.",
                  "\n",
                  "Currently type is ",
                  type,
                  ".",
                  "\n",
                  "(use function c() if you want to combines multiple arguments)."))
    }
  }
  query_final <- gsub(pattern = pattern,
                      replacement = tmp,
                      x = query)
  return(query_final)
}
