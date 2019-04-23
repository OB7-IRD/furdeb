#' @name db_connection
#' @title Connection to a PostgreSQL database
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @description This function able to establish a connection with a PostgreSQL database.
#' @param db_user Login or username for the database connection.
#' @param db_password Password for the database connection.
#' @param db_dbname Name/identification of the database.
#' @param db_host Host adress of the database.
#' @param db_port Identification of the port.
#' @references \url{https://github.com/OB7-IRD/toolbox}
#' @return The function return a format class PosstgreSQLConnection in the R's global envrionment.
#' @export
db_connection <- function(db_user,
                          db_password,
                          db_dbname,
                          db_host,
                          db_port) {
  # Connection to PostgreSQL database ----
  db_connection <- RPostgreSQL::dbConnect(DBI::dbDriver("PostgreSQL"),
                                          user = db_user,
                                          password = db_password,
                                          dbname = db_dbname,
                                          host = db_host,
                                          port = db_port)
  return(db_connection)
}
