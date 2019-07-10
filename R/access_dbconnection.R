#' @name access_dbconnection
#' @title Connection with Access database (with a JDBC driver)
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @description Generate a common connection with an Access database using a JDBC driver.
#' @param driver_name Access' driver name. By default "u_can_access". You can also choose the driver "access_jdbc42".
#' @param access_db_loc Location of the Access database (for example Directory_1\%Directory_2\%Directory_X\%access_DB.mdb).
#' @references \url{https://github.com/OB7-IRD/furdeb}
#' @return The function return a R object with Access database identification of connection.
#' @export
access_dbconnection <- function(driver_name = "u_can_access",
                                access_db_loc) {
  # Driver name verification
  if (! driver_name %in% c("u_can_access", "access_jdbc42")) {
    stop(paste0("Missing argument \"driver_name\" or not supported yet.",
                "\n",
                "You could choose between: \"u_can_access\" and \"access_jdbc42\".",
                "\n",
                "Please correct it before running the function."))
  }

  if (driver_name == "access_jdbc42") {
    # Initializing Access JDBC driver ----
    access_jdbc_driver <- RJDBC::JDBC(driverClass = "com.hxtt.sql.access.AccessDriver",
                                      classPath = system.file("access_jdbc42",
                                                              "Access_JDBC42.jar",
                                                              package = "furdeb"))

    # Connection to Access database ----
    access_jdbc_connection <- RJDBC::dbConnect(access_jdbc_driver,
                                               paste0("jdbc:access:/",
                                                      access_db_loc))
  } else {
    if (driver_name == "u_can_access") {
      # Initializing Access JDBC driver ----
      dependencies <- c(system.file("u_can_access",
                                    "jackcess-2.1.11.jar",
                                    package = "furdeb"),
                        system.file("u_can_access",
                                    "hsqldb.jar",
                                    package = "furdeb"),
                        system.file("u_can_access",
                                    "commons-logging-1.1.3.jar",
                                    package = "furdeb"),
                        system.file("u_can_access",
                                    "commons-lang-2.6.jar",
                                    package = "furdeb"))

      rJava::.jinit(classpath = dependencies)

      access_jdbc_driver <- RJDBC::JDBC(driverClass = "net.ucanaccess.jdbc.UcanaccessDriver",
                                        classPath = system.file("u_can_access",
                                                                "ucanaccess-4.0.4.jar",
                                                                package = "furdeb"))
      # Connection to Access database ----
      access_jdbc_connection <- RJDBC::dbConnect(access_jdbc_driver,
                                                 paste0("jdbc:ucanaccess://",
                                                        access_db_loc))
    }
  }
  return(access_jdbc_connection)
}
