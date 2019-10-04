#' @name sql_inset
#' @title Interpolate values into a SQL string query
#' @description Interpolate values into a SQL string query.
#' @param db_type Type of database related to the query. For now, the function accepts only "access" or "postgresql" databases.
#' @param replacement Replacement argument for matched pattern (R vector). If you want to specify multiple arguments, use the function c().
#' @param pattern Character string containing a string expression to be matched.
#' @param query A character vector where matches are sought, or an object which can be coerced by as.character to a character vector.
#' @return A vector contain the query with replacement value(s).
#' @examples
#' For a query on an Access database
#' \dontrun{
#' #' final_query <- sql_inset(db_type = "access",
#'                          replacement = 2017,
#'                          pattern = "year_interpolate",
#'                          query = intial_query)}
#' @export
sql_inset <- function (db_type,
                       replacement,
                       pattern,
                       query) {
  # Arguments verification ----
  if (missing(db_type) || ! db_type %in% c("access", "postgresql")) {
    stop("Missing argument \"db_type\" or value inside not allowed.",
         "\n",
         "For now, the function accepts only \"access\" or \"postgresql\" databases.",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(replacement)) {
    stop("Missing argument \"replacement\".",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(pattern) || ! is.character(pattern)) {
    stop("Missing argument \"pattern\" or value inside not character.",
         "\n",
         "Please correct it before running the function.")
  }
  if (missing(query) || ! is.character(pattern)) {
    stop("Missing argument \"query\" or value inside not character.",
         "\n",
         "Please correct it before running the function.")
  }

  # Function ----
  type <- mode(x = replacement)
  if (type == "numeric") {
    if (length(replacement) == 1) {
      tmp <- paste0("(",
                    replacement,
                    ")")
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
        tmp <- paste0("('",
                      replacement,
                      "')")
      } else {
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
          if (i == length(replacement)) {
            tmp <- paste0("(",
                          tmp,
                          ")")
          }
        }
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
