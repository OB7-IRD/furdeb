#' @name data_extraction
#' @title Function for data extraction
#' @description Function for data extraction and design from a file (csv or txt) or a database.
#' @param type {\link[base]{character}} expected. Mandatory. Type of process for data extraction. You can choose between "csv_txt" or "database".
#' @param file_path {\link[base]{character}} expected. Mandatory. File path of the csv, txt or sql file.
#' @param database_connection {\link[base]{list}} expected. Mandatory for type "sql". By default NULL.
#'  Output list of furdeb connection functions to one or more databases (like {\link[furdeb]{access_dbconnection}} or {\link[furdeb]{postgresql_dbconnection}}).
#' @param anchor {\link[base]{list}} expected. Optional for type "sql". By default NULL. List of values to interpolate in a SQL string query.
#'  Each list element have to follow this format: \code{anchor=list(name_arg1 = value(s), names_arg2=value(s),...)}.
#'  Be aware that the values typing format influence the writing of the sql in the query.
#'  For example, an integer or numeric value doesn't have any quote, rather than a date or string value.
#' @param column_name {\link[base]{character}} expected. Optional for type csv_txt. By default NULL. Column name(s).
#' @param column_type {\link[base]{character}} expected. Optional for type csv_txt. By default NULL. Column type(s).
#'  You can use the same format that the argument "col_types" of the function {\link[readr]{read_delim}}.
#' @param export_path_directory {\link[base]{character}} expected. Optional. By default NULL. Directory path associated for the export (in csv).
#' @return The function return a tibble.
#' @export
data_extraction <- function(type,
                            file_path,
                            database_connection = NULL,
                            anchor = NULL,
                            column_name = NULL,
                            column_type = NULL,
                            export_path_directory = NULL) {
  message(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Start process of data extraction.",
          sep = "")
  # 1 - Arguments verifications ----
  message(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Start arguments verifications.",
          sep = "")
  codama::r_type_checking(r_object = type,
                              length = 1L,
                              type = "character",
                              allowed_value = c("csv_txt",
                                                "database"))
  codama::file_path_checking(file_path = file_path,
                                 extension = c("csv",
                                               "sql",
                                               "txt"))
  codama::r_type_checking(r_object = database_connection,
                                 type = "list")
  if (type == "database") {
    if (length(database_connection) == 2 && !is.list(database_connection[[2]])) {
      # Specific argument verification for simple database
      if (paste0(class(x = database_connection[[2]]),
                 collapse = " ") != "PqConnection" && paste0(class(x = database_connection[[2]]),
                                                                     collapse = " ") != "JDBCConnection") {
        stop(format(x = Sys.time(),
                    format = "%Y-%m-%d %H:%M:%S"),
             " - Invalid \"database_connection\" argument. Class \"PqConnection\" or \"JDBCConnection\" expected for the second element of the connection")
      }
    } else {
      # Specific argument verification for multiple database
      for (number_database_connection in seq(from = 1, to = length(database_connection))){
        codama::r_type_checking(r_object = database_connection[[number_database_connection]],
                                length = 2L,
                                type = "list")
        if (paste0(class(x = database_connection[[number_database_connection]][[2]]),
                   collapse = " ") != "PqConnection" && paste0(class(x = database_connection[[number_database_connection]][[2]]),
                                                                       collapse = " ") != "JDBCConnection") {
          stop(format(x = Sys.time(),
                      format = "%Y-%m-%d %H:%M:%S"),
               " - Invalid \"database_connection\" argument. Class \"PqConnection\" or \"JDBCConnection\" expected for the second element of the connection number ", number_database_connection)
        }
      }
    }
  }
  codama::r_type_checking(r_object = anchor,
                                 type = "list")
  codama::r_type_checking(r_object = column_name,
                                 type = "character")
  codama::r_type_checking(r_object = column_type,
                                 type = "character")
  codama::r_type_checking(r_object = export_path_directory,
                                 type = "character")
  message(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Successful arguments verifications.",
          sep = "")
  # 2 - Global process ----
  if (type == "csv_txt") {
    # process for csv/txt data extraction
    if (dplyr::last(x = unlist(x = strsplit(x = file_path,
                                            split = "[.]"))) == "sql") {
      stop(format(x = Sys.time(),
                  format = "%Y-%m-%d %H:%M:%S"),
           " - \"file_path\" argument \"",
           dplyr::last(x = unlist(x = strsplit(x = file_path,
                                               split = "[.]"))),
           "\" not valid for type \"",
           type,
           "\".",
           sep = "")
    }
    if ((! is.null(x = column_name)
         && ! is.null(x = column_type))
        || (! is.null(x = column_name))) {
      tryCatch(expr = data_extracted <- readr::read_delim(file = file_path,
                                                          show_col_types = FALSE,
                                                          col_names = column_name,
                                                          col_types = as.list(column_type),
                                                          skip = 1),
               warning = function(thug_life) {
                 print(readr::problems(readr::read_delim(file = file_path,
                                                         show_col_types = FALSE,
                                                         col_names = column_name,
                                                         col_types = as.list(column_type),
                                                         skip = 1)))
                 stop(format(x = Sys.time(),
                             format = "%Y-%m-%d %H:%M:%S"),
                      " - One or more parsing issues, check details above.",
                      sep = "")
               })
    } else if (! is.null(x = column_type)) {
      tryCatch(expr = data_extracted <- readr::read_delim(file = file_path,
                                                          show_col_types = FALSE,
                                                          col_types = as.list(column_type)),
               warning = function(thug_life) {
                 print(readr::problems(readr::read_delim(file = file_path,
                                                         show_col_types = FALSE,
                                                         col_types = as.list(column_type))))
                 stop(format(x = Sys.time(),
                             format = "%Y-%m-%d %H:%M:%S"),
                      " - One or more parsing issues, check details above.",
                      sep = "")
               })
    } else {
      tryCatch(expr = data_extracted <- readr::read_delim(file = file_path,
                                                          show_col_types = FALSE),
               warning = function(thug_life) {
                 print(readr::problems(readr::read_delim(file = file_path,
                                                         show_col_types = FALSE)))
                 stop(format(x = Sys.time(),
                             format = "%Y-%m-%d %H:%M:%S"),
                      " - One or more parsing issues, check details above.",
                      sep = "")
               })
    }
    if (! is.null(x = column_name)
        && (length(x = data_extracted) != length(x = column_name))) {
      warning(format(x = Sys.time(),
                     format = "%Y-%m-%d %H:%M:%S"),
              " - The number of columns provided in the \"column_name\" argument is not equal to the number of variables available in the input file.",
              sep = "")
    }
    data_extracted_final <- data_extracted
  } else if (type == "database") {
    # process for database extraction
    database_query <- paste(readLines(con = file_path),
                            collapse = "\n")
    if (! is.null(x = anchor)) {
      if (length(x = dplyr::symdiff(x = stringr::str_extract_all(string = database_query,
                                                                 pattern = "(?<=\\?)[^\\)|[:blank:]|[:space:]]+",
                                                                 simplify = TRUE),
                                    y = names(anchor))) != 0) {
        stop(format(x = Sys.time(),
                    format = "%Y-%m-%d %H:%M:%S"),
             " - Value(s) in the \"anchor\" argument is different from anchor(s) in the sql query.",
             sep = "")
      }
    }
    for (anchor_id in names(x = anchor)) {
      if (is.integer(x = anchor[[anchor_id]])
          || is.numeric(x = anchor[[anchor_id]])) {
        anchor[[anchor_id]] <- DBI::SQL(paste0(anchor[[anchor_id]],
                                               collapse = ", "))
      } else if (is.character(x = anchor[[anchor_id]])
                 || lubridate::is.Date(x = anchor[[anchor_id]])) {
        anchor[[anchor_id]] <- DBI::SQL(paste0("'",
                                               paste0(anchor[[anchor_id]],
                                                      collapse = "', '"),
                                               "'"))
      } else {
        stop(format(x = Sys.time(),
                    format = "%Y-%m-%d %H:%M:%S"),
             " - Anchor format \"",
             paste0(class(x = anchor[[anchor_id]]),
                    collapse = ", "),
             "\" not integrated in the process yet.\n",
             "Check the anchor \"",
             anchor_id, "\".",
             sep = "")
      }
    }
    number_database_connection <- 1
    while (number_database_connection <= length(database_connection)) {
      if (length(database_connection) == 2 && !is.list(database_connection[[2]])) {
        # Connection for simple database
        database_conn <- database_connection
      } else {
        # Connection for multiple database
        database_conn <- database_connection[[number_database_connection]]
      }
      if (!is.null(x = anchor)) {
        if (length(x = dplyr::symdiff(x = stringr::str_extract_all(string = database_query,
                                                                   pattern = "(?<=\\?)[^\\)|[:blank:]|[:space:]]+",
                                                                   simplify = TRUE),
                                      y = names(anchor))) != 0) {
          stop(format(x = Sys.time(),
                      format = "%Y-%m-%d %H:%M:%S"),
               " - Value(s) in the \"anchor\" argument is different from anchor(s) in the sql query.",
               sep = "")
        }
        sql_interpolate_query <- paste0("DBI::sqlInterpolate(conn = database_conn[[2]], sql = database_query, ")
        for (anchor_id in names(x = anchor)) {
          sql_interpolate_query <- paste0(sql_interpolate_query,
                                          anchor_id,
                                          " = anchor[[",
                                          stringr::str_which(string = names(x = anchor),
                                                             pattern = anchor_id), "]], ")
        }
        sql_interpolate_query <- paste0(stringr::str_extract(string = sql_interpolate_query,
                                                             pattern = ".+(?=,[:blank:]$)"),
                                        ")")
        sql_interpolate_query_expression <- parse(text = sql_interpolate_query)
        database_query_final <- eval(sql_interpolate_query_expression)
      } else {
        database_query_final <- database_query
      }
      data_extracted <- DBI::dbGetQuery(conn = database_conn[[2]],
                                        statement = database_query_final)
      if (nrow(data_extracted) > 0) {
        if (exists("data_extracted_final")) {
          data_extracted_final <- dplyr::full_join(data_extracted_final, data_extracted, by = colnames(data_extracted_final))
        } else {
          data_extracted_final <- data_extracted
        }
      }
      if (length(database_connection) == 2 && !is.list(database_connection[[2]])) {
        # Connection for simple database
        number_database_connection <- number_database_connection + 2
      } else {
        # Connection for multiple database
        number_database_connection <- number_database_connection + 1
      }
    }
    # If all extractions have returned an empty array
    if (!exists("data_extracted_final")) {
      data_extracted_final <- data_extracted
    }
  }
  data_extracted_final <- tibble::as_tibble(x = data_extracted_final)
  # 3 - Extraction ----
  if (! is.null(x = export_path_directory)) {
    message(format(x = Sys.time(),
                   format = "%Y-%m-%d %H:%M:%S"),
            " - Start data extraction.",
            sep = "")
    utils::write.csv2(x = data_extracted_final,
                      file = file.path(export_path_directory,
                                       paste0(format(as.POSIXct(Sys.time()),
                                                     "%Y%m%d_%H%M%S"),
                                              "_data_extracted_",
                                              stringr::str_extract(string = "eu_countries.cover.csv",
                                                                   pattern = "[^.]+"))),
                      row.names = FALSE)
    message(format(x = Sys.time(),
                   format = "%Y-%m-%d %H:%M:%S"),
            " - Successful data extraction.",
            sep = "")
  }
  message(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Successful process of data extraction.",
          sep = "")
  return(data_extracted_final)
}
