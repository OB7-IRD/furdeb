#' @name postgresql_dbconnection
#' @title Connection to a PostgreSQL database
#' @description This function able to establish a connection with a PostgreSQL database.
#' @param db_user {\link[base]{character}} expected. Login or username for the database connection.
#' @param db_password {\link[base]{character}} expected. Password for the database connection.
#' @param db_dbname {\link[base]{character}} expected. Name/identification of the database.
#' @param db_host {\link[base]{character}} expected. Host adress of the database.
#' @param db_port {\link[base]{integer}} expected. Identification of the port.
#' @return The function return a list.
#' @export
postgresql_dbconnection <- function(db_user,
                                    db_password,
                                    db_dbname,
                                    db_host,
                                    db_port) {
  # 1 - Arguments verification ----
  codama::r_type_checking(r_object = db_user,
                          type = "character",
                          length = 1L)
  codama::r_type_checking(r_object = db_password,
                          type = "character",
                          length = 1L)
  codama::r_type_checking(r_object = db_dbname,
                          type = "character",
                          length = 1L)
  codama::r_type_checking(r_object = db_host,
                          type = "character",
                          length = 1L)
  if ((codama::r_type_checking(r_object = db_port,
                               type = "integer",
                               length = 1L,
                               output = "logical")
       || codama::r_type_checking(r_object = db_port,
                                  type = "numeric",
                                  length = 1L,
                                  output = "logical")) != TRUE) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Invalid \"db_port\", type integer or numeric expected.")
  }
  # 2 - Global process ----
  postgresql_db_connection <- list("database_name" = db_dbname,
                                   "connection_information" = RPostgreSQL::dbConnect(DBI::dbDriver("PostgreSQL"),
                                                                                     user = db_user,
                                                                                     password = db_password,
                                                                                     dbname = db_dbname,
                                                                                     host = db_host,
                                                                                     port = as.integer(x = db_port)))
  return(postgresql_db_connection)
}
