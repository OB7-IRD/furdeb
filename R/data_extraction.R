#' @name data_extraction
#' @title Data extraction
#' @description Extracts a dataset from a database or csv
#' @param type {\link[base]{character}} expected. dabase or csv.
#' @param data_connection {\link[base]{list}} expected. Output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the data_extraction function.
#' @param sql_name {\link[base]{character}}. Name of the sql file, in fishi.
#' @param csv_path {\link[base]{character}}. Path of the csv.
#' @param time_period {\link[base]{integer}} expected. Period identification in year.
#' @param country {\link[base]{integer}} expected. Country codes identification. [\href{https://ob7-ird.github.io/fishi/articles/Referentials.html}{Country referentials}]
#' @param vessel_type {\link[base]{integer}} expected. Vessel type codes identification. [\href{https://ob7-ird.github.io/fishi/articles/Referentials.html}{Vessel_type referentials}]
#' @param vessel_type_select {\link[base]{character}} expected. engin or vessel_type.
#' @param ocean {\link[base]{integer}} expected. Ocean codes identification. [\href{https://ob7-ird.github.io/fishi/articles/Referentials.html}{Oscean referentials}]
#' @param csv_output TRUE to obtain a csv document. FALSE by default.
#' @return The function return ggplot R plot.
#' @export
#' @importFrom
data_extraction <- function(type,
                            file_path,
                            database_connection = NULL,
                            anchor = NULL,
                            column_name = NULL,
                            column_type = NULL,
                            export_path = NULL) {
  cat(format(x = Sys.time(),
             format = "%Y-%m-%d %H:%M:%S"),
      " - Start process of data extraction.\n",
      sep = "")
  # 0 - Local binding global variables ----

  # 1 - Arguments verifications ----
  cat(format(x = Sys.time(),
             format = "%Y-%m-%d %H:%M:%S"),
      " - Start arguments verifications.\n",
      sep = "")
  # type verification
  if (codama::r_type_checking(r_object = type,
                              length = 1L,
                              type = "character",
                              allowed_value = c("csv_txt",
                                                "database"),
                              output = "logical") != TRUE) {
    codama::r_type_checking(r_object = type,
                            length = 1L,
                            type = "character",
                            allowed_value = c("csv",
                                              "database"),
                            output = "message")
  }
  # file_path verification
  if (codama::file_path_checking(file_path = file_path,
                                 extension = c("csv",
                                               "sql",
                                               "txt"),
                                 output = "logical") != TRUE) {
    codama::file_path_checking(file_path = file_path,
                               extension = c("csv",
                                             "sql",
                                             "txt"),
                               output = "message")
  }
  # database_connection verification
  if ((! is.null(x = database_connection))
      && codama::r_type_checking(r_object = database_connection,
                                 length = 2L,
                                 type = "list",
                                 output = "logical") != TRUE) {
    codama::r_type_checking(r_object = database_connection,
                            length = 2L,
                            type = "list",
                            output = "message")
  }
  # anchor verification
  if ((! is.null(x = anchor))
      && codama::r_type_checking(r_object = anchor,
                                 type = "character",
                                 output = "logical") != TRUE) {
    codama::r_type_checking(r_object = anchor,
                            type = "character",
                            output = "message")
  }
  # column_name verification
  if ((! is.null(x = column_name))
      && codama::r_type_checking(r_object = column_name,
                                 type = "character",
                                 output = "logical") != TRUE) {
    codama::r_type_checking(r_object = column_name,
                            type = "character",
                            output = "message")
  }
  # column_type verification
  if ((! is.null(x = column_type))
      && codama::r_type_checking(r_object = column_type,
                                 type = "character",
                                 output = "logical") != TRUE) {
    codama::r_type_checking(r_object = column_type,
                            type = "character",
                            output = "message")
  }
  # export_path verification
  if ((! is.null(x = export_path))
      && codama::r_type_checking(r_object = export_path,
                                 type = "character",
                                 output = "logical") != TRUE) {
    codama::r_type_checking(r_object = export_path,
                            type = "character",
                            output = "message")
  }
  cat(format(x = Sys.time(),
             format = "%Y-%m-%d %H:%M:%S"),
      " - Successful arguments verifications.\n",
      sep = "")
  # 2 - Global process ----
  if (type == "csv_txt") {
    # process for csv/txt data extraction
    if (dplyr::last(x = unlist(x = strsplit(x = file_path,
                                            split = "[.]"))) == "sql") {
      stop(format(x = Sys.time(),
                  format = "%Y-%m-%d %H:%M:%S"),
           " - Error, \"file_path\" argument \"",
           dplyr::last(x = unlist(x = strsplit(x = file_path,
                                               split = "[.]"))),
           "\" not valid for type \"",
           type,
           "\".\n",
           sep = "")
    }
    if ((! is.null(x = column_name)
         & ! is.null(x = column_type))
        | (! is.null(x = column_name))) {
      tryCatch(expr = data_extracted <- readr::read_delim(file = file_path,
                                                          show_col_types = FALSE,
                                                          col_names = column_name,
                                                          col_types = as.list(column_type),
                                                          skip = 1),
               warning = function(thug_life) {
                 cat(format(x = Sys.time(),
                            format = "%Y-%m-%d %H:%M:%S"),
                     " - Error, one or more parsing issues, call readr::problems(data_extraction R object name) for details.\n",
                     sep = "")

               })
    } else if (! is.null(x = column_type)) {
      tryCatch(expr = data_extracted <- readr::read_delim(file = file_path,
                                                          show_col_types = FALSE,
                                                          col_types = as.list(column_type)),
               warning = function(thug_life) {
                 cat(format(x = Sys.time(),
                            format = "%Y-%m-%d %H:%M:%S"),
                     " - Error, one or more parsing issues, call readr::problems(data_extraction R object name) for details.\n",
                     sep = "")
               })
    } else {
      data_extracted <- readr::read_delim(file = file_path,
                                          show_col_types = FALSE)
    }
  } else if (type == "database") {

  }

  if (type == "database") {
    # Choose database
    if (data_connection[[1]] == "balbaya") {
      extraction_sql <- paste(readLines(con = system.file("sql",
                                                          sql_name,
                                                          package = "fishi")),
                              collapse = "\n")
    } else if (data_connection[[1]] == "avdth") {
      extraction_sql <- paste(readLines(con = system.file("sql",
                                                          sql_name,
                                                          package = "fishi")),
                              collapse = "\n")
    } else if (data_connection[[1]] == "sardara") {
      extraction_sql <- paste(readLines(con = system.file("sql",
                                                          sql_name,
                                                          package = "fishi")),
                              collapse = "\n")
    } else if (data_connection[[1]] == "observe_test") {
      extraction_sql <- paste(readLines(con = system.file("sql",
                                                          sql_name,
                                                          package = "fishi")),
                              collapse = "\n")
    } else if (data_connection[[1]] == "t3_prod") {
      extraction_sql <- paste(readLines(con = system.file("sql",
                                                          sql_name,
                                                          package = "fishi")),
                              collapse = "\n")
    } else if (data_connection[[1]] == "vms") {
      extraction_sql <- paste(readLines(con = system.file("sql",
                                                          sql_name,
                                                          package = "fishi")),
                              collapse = "\n")
    } else {
      stop(format(x = Sys.time(),
                  format = "%Y-%m-%d %H:%M:%S"),
           " - Indicator not developed yet for this \"data_connection\" argument.\n",
           sep = "")
    }
    # Extraction sql final
    if (data_connection[[1]] == "sardara") {
      time_period <- c((time_period):(time_period - 5))
      extraction_sql_final <- DBI::sqlInterpolate(conn        = data_connection[[2]],
                                                  sql         = extraction_sql,
                                                  time_period = DBI::SQL(paste(time_period,
                                                                               collapse = ", ")),
                                                  country     = DBI::SQL(paste(country,
                                                                               collapse = ", ")),
                                                  ocean       = DBI::SQL(paste(ocean,
                                                                               collapse = ", ")))
    } else if (data_connection[[1]] == "vms") {
      extraction_sql_final <- DBI::sqlInterpolate(conn        = data_connection[[2]],
                                                  sql         = extraction_sql,
                                                  time_period = DBI::SQL(paste(time_period,
                                                                               collapse = ", ")),
                                                  country     = DBI::SQL(paste(country,
                                                                               collapse = ", ")))
    } else {
      # Vessel type select
      if (vessel_type_select == "engin") {
        extraction_sql <- gsub(pattern = "\n\t.*\\(\\?vessel_type\\)",
                               replacement = "",
                               x = extraction_sql,
                               perl = TRUE)
        extraction_sql_final <- DBI::sqlInterpolate(conn        = data_connection[[2]],
                                                    sql         = extraction_sql,
                                                    time_period = DBI::SQL(paste(time_period,
                                                                                 collapse = ", ")),
                                                    country     = DBI::SQL(paste(country,
                                                                                 collapse = ", ")),
                                                    engin = DBI::SQL(paste(vessel_type,
                                                                           collapse = ", ")),
                                                    ocean       = DBI::SQL(paste(ocean,
                                                                                 collapse = ", "))
        )
      } else if (vessel_type_select == "vessel_type") {
        extraction_sql <- gsub(pattern = "\n\t.*\\(\\?engin\\)",
                               replacement = "",
                               x = extraction_sql,
                               perl = TRUE)
        extraction_sql_final <- DBI::sqlInterpolate(conn        = data_connection[[2]],
                                                    sql         = extraction_sql,
                                                    time_period = DBI::SQL(paste(time_period,
                                                                                 collapse = ", ")),
                                                    country     = DBI::SQL(paste(country,
                                                                                 collapse = ", ")),
                                                    vessel_type = DBI::SQL(paste(vessel_type,
                                                                                 collapse = ", ")),
                                                    ocean       = DBI::SQL(paste(ocean,
                                                                                 collapse = ", ")))
      } else {
        stop(format(x = Sys.time(),
                    format = "%Y-%m-%d %H:%M:%S"),
             " - Indicator not developed yet for this \"vessel_type_select\" argument.\n",
             sep = "")
      }
    }
    database <- dplyr::tibble(DBI::dbGetQuery(conn      = data_connection[[2]],
                                              statement = extraction_sql_final))
    if (csv_output == TRUE) {
      utils::write.csv2(database,
                        file = "database_fishi.csv",
                        row.names = FALSE)
    }
    return(database)
  }
  # 3 - Extraction ----
  if (! is.null(x = export_path)) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Start data extraction.\n",
        sep = "")


    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Successful data extraction.\n",
        sep = "")
  }
  cat(format(x = Sys.time(),
             format = "%Y-%m-%d %H:%M:%S"),
      " - Successful process of data extraction.\n",
      sep = "")
}
