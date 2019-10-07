#' @name access_dbconnection
#' @title Connection with Access database
#' @description Generate a common connection with an Access database using a JDBC driver.
#' @param driver_name (character) Access' driver name. By default "u_can_access" (free driver). You can also choose the driver "access_jdbc42" (paid driver).
#' @param access_db_path (character) Path of the Access database.
#' @param access_jdbc42_driver_path (character, optional) Path of the access_jdbc42 driver (.jar file).
#' @return The function return a R object with Access database identification of connection.
#' @details
#' Difference between drivers "u_can_access" and "access_jdbc42":
#' \itemize{
#'  \item{"u_can_access": }{the main advantage is it's a free java JDBC driver. To understand briefly the process behind, the Access database is converted in HSQLDB system. This conversion could take a long time, especially if the database if large. Furthermore, after the conversion, queries should be faster than if there run on an Access database.}
#'  \item{"access_jdbc42": }{this driver is paying. In opposition with the "u_can_access" driver, the Access database is not converted. The connection should be faster but the queries could be longer.}
#' }
#' @export
#' @importFrom RJDBC JDBC dbConnect
#' @importFrom rJava .jinit
access_dbconnection <- function(driver_name = "u_can_access",
                                access_db_path,
                                access_jdbc42_driver_path) {
  # Driver name verification ----
  driver_name <- match.arg(arg = driver_name,
                           choices = c("u_can_access", "access_jdbc42"))
  # Access DB path verification ----
  if (! is.character(access_db_path)) {
    stop("invalid \"access_db_path\" argument")
  }

  if (driver_name == "access_jdbc42") {
    # Driver path verification ----
    if (missing(access_jdbc42_driver_path) || ! is.character(access_jdbc42_driver_path)) {
      stop("invalid \"access_jdbc42_driver_path\" argument")
    }
    # Initializing Access JDBC driver ----
    access_jdbc_driver <- RJDBC::JDBC(driverClass = "com.hxtt.sql.access.AccessDriver",
                                      classPath = access_jdbc42_driver_path)

    # Connection to Access database ----
    access_jdbc_connection <- RJDBC::dbConnect(access_jdbc_driver,
                                               paste0("jdbc:access:/",
                                                      access_db_path))
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
                                                        access_db_path))
    }
  }
  return(access_jdbc_connection)
}
