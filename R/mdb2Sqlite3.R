#' @name mdb2Sqlite3
#' @title Converting an Access database to an SQLite3 database
#' @description Convert Access database to sqlite3 database.
#' @param access_con Object with Access database connection identification.
#' @param target_dir Directory where the SQLite3 file will be created.
#' @param target_file_name SQLite3 file name (for example: database_name.sqlite3).
#' @param verbose Should the script be verbose.
#' @return The function return an SQLite3 database in the location directory. The file path of the SQLite3 file is also available.
#' @section Specification:
#' Should work on windows and on linux (need testing for this OS). For linux the "mdb-tools" must be installed.
#' @export
#' @importFrom RSQLite dbConnect SQLite dbGetQuery
#' @importFrom DBI dbWriteTable dbDisconnect
mdb2Sqlite3 <- function(access_con,
                        target_dir,
                        target_file_name,
                        verbose = TRUE) {
  if (missing(access_con)) {
    stop("Argument access_con not found\nInvalid connection with Access database")
  }
  if (missing(target_dir)) {
    stop("Target directory not found")
  }
  if (missing(target_file_name)) {
    stop("Name of the SQLite database not found")
  } else {
    sqlite_file_path <- file.path(target_dir,
                                  target_file_name,
                                  fsep = "\\")
  }
  if (file.exists(sqlite_file_path)) {
    stop(paste0("Target SQLite file ",
                sqlite_file_path,
                " already exist. \nPlease remove it."))
  }
  if (! require(RSQLite,
                quietly = TRUE)) {
    stop("\n***\nPlease install RSQLite library with:\ninstall.packages(\"RSQLite\")\n***")
  }
  sysname <- Sys.info()[['sysname']]
  if (sysname == "Windows") {

    mdb.conn <- access_con
    sqlite.conn <- RSQLite::dbConnect(RSQLite::SQLite(),
                                     dbname = sqlite_file_path)
    tables <- as.vector(t(RJDBC::dbGetQuery(mdb.conn, "SELECT Name
                                                      FROM MsysObjects
                                                      WHERE
                                                        LEFT(Name,1)<>'~'
                                                        AND LEFT(Name,4)<>'MSys'
                                                        AND Type=1
                                                      ORDER BY Name;")))
    for (curr_table in tables) {
      if (verbose) {
        message("Processing ",
                curr_table,
                " table")
      }
      curr_df <- RJDBC::dbGetQuery(mdb.conn,
                                  paste("SELECT * FROM \"",
                                        curr_table,
                                        "\"",
                                        sep = ""))
      DBI::dbWriteTable(conn = sqlite.conn,
                        name = curr_table,
                        curr_df)
    }
    DBI::dbDisconnect(sqlite.conn,
                      mdb.conn)
  }
  if (sysname == "Linux") {
    schema_file_path <- tempfile(fileext = ".sql")
    cmd1 <- paste0("mdb-schema --no-relations --drop-table ",
                   mdb_file_path,
                   " sqlite > ",
                   schema_file_path)
    system(cmd1)
    cmd2 <- paste0("sqlite3 ",
                   sqlite_file_path,
                   " < ",
                   schema_file_path)
    system(cmd2)
    cmd3 <- paste0("mdb-tables -1 ",
                   mdb_file_path)
    tables <- system(cmd3,
                     intern = TRUE)
    tmp_file_path <- tempfile(fileext = ".sql")
    for (curr_table in tables) {
      if (verbose) {
        message("Processing ",
                curr_table,
                " table")
      }
      cat("BEGIN;",
          file = tmp_file_path)
      cmd_table <- paste0("mdb-export -D '%Y-%m-%d %H:%M:%S' -R ';\n' -I sqlite ",
                         mdb_file_path,
                         " ",
                         curr_table,
                         " >> ",
                         tmp_file_path)
      system(cmd_table)
      cat("COMMIT;",
          file = tmp_file_path,
          append = TRUE)
      cmd_sqlite <- paste0("sqlite3 ",
                          sqlite_file_path,
                          " < ",
                          tmp_file_path)
      system(cmd_sqlite)
    }
  }
  if (verbose) {
    message("Done.\nYou can use it now. Example:\n***")
    message("library(\"RSQLite\")")
    message("sqlite.conn <- dbConnect(drv=RSQLite::SQLite(), dbname=\"",
            sqlite_file_path,
            "\")")
    message("table.df <- dbGetQuery(conn=sqlite.conn, statement=\"SELECT * FROM ",
            curr_table,
            " LIMIT 10;\")")
    message("dbDisconnect(sqlite.conn)")
    message("***")
  }
  return(sqlite_file_path)
}
