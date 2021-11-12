#' @name postgresql_dbconnection
#' @title Connection to a PostgreSQL database
#' @description This function able to establish a connection with a PostgreSQL database.
#' @param db_user {\link[base]{character}} expected. Login or username for the database connection.
#' @param db_password {\link[base]{character}} expected. Password for the database connection.
#' @param db_dbname {\link[base]{character}} expected. Name/identification of the database.
#' @param db_host {\link[base]{character}} expected. Host adress of the database.
#' @param db_port {\link[base]{numeric}} expected. Identification of the port.
#' @return The function return a list.
#' @export
#' @importFrom RPostgreSQL dbConnect
#' @importFrom DBI dbDriver
postgresql_dbconnection <- function(db_user,
                                    db_password,
                                    db_dbname,
                                    db_host,
                                    db_port) {
  # Connection to PostgreSQL database ----
  postgresql_db_connection <- list("database_name" <- db_dbname,
                                   "connection_information" <- RPostgreSQL::dbConnect(DBI::dbDriver("PostgreSQL"),
                                                                                      user = db_user,
                                                                                      password = db_password,
                                                                                      dbname = db_dbname,
                                                                                      host = db_host,
                                                                                      port = db_port))
  return(postgresql_db_connection)
}
