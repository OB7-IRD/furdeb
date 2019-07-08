#' @name access_dbconnection
#' @title Connection with Access database (with a JDBC driver)
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @description Generate a common connection with an Access database using a JDBC driver.
#' @param jdbc_access_driverclass Identification of the Access driver class (for example com.hxtt.sql.access.AccessDriver).
#' @param jdbc_access_driver_loc Location of the Access JDBC driver (for example Directory_1\%Directory_2\%Directory_X\%access_JDBCdriver.jar).
#' @param access_db_loc Location of the Access database (for example Directory_1\%Directory_2\%Directory_X\%access_DB.mdb).
#' @references \url{https://github.com/OB7-IRD/furdeb}
#' @return The function return a R object with Access database identification of connection.
#' @export
access_dbconnection <- function(jdbc_access_driverclass,
                                jdbc_access_driver_loc,
                                access_db_loc) {
  # Initializing Access JDBC driver ----
  access_jdbc_driver <- RJDBC::JDBC(driverClass = jdbc_access_driverclass,
                                    classPath = jdbc_access_driver_loc)
  # Connection to Access database ----
  access_jdbc_connection <- RJDBC::dbConnect(access_jdbc_driver,
                                             paste("jdbc:access:/",
                                                   access_db_loc,
                                                   sep = ""))
  return(access_jdbc_connection)
}
