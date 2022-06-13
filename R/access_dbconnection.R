#' @name access_dbconnection
#' @title Connection with Access database
#' @description Generate a common connection with an Access database using a JDBC driver.
#' @param driver_name {\link[base]{character}} expected. Access' driver name. By default "u_can_access" (free driver). You can also choose the driver "access_jdbc42" (paid driver).
#' @param access_db_path {\link[base]{character}} expected. Path of the Access database.
#' @param access_jdbc42_driver_path {\link[base]{character}} expected. Optional. Path of the access_jdbc42 driver (.jar file).
#' @return The function return a R object with Access database identification of connection.
#' @details
#' Difference between drivers "u_can_access" and "access_jdbc42":
#' \itemize{
#'  \item{"u_can_access": }{the main advantage is it's a free java JDBC driver. To understand briefly the process behind, the Access database is converted in HSQLDB system. This conversion could take a long time, especially if the database if large. Furthermore, after the conversion, queries should be faster than if there run on an Access database.}
#'  \item{"access_jdbc42": }{this driver is paying. In opposition with the "u_can_access" driver, the Access database is not converted. The connection should be faster but the queries could be longer.}
#' }
#' @importFrom RJDBC JDBC dbConnect
#' @export
access_dbconnection <- function(driver_name = "u_can_access",
                                access_db_path,
                                access_jdbc42_driver_path) {
  # driver name verification ----
  driver_name <- match.arg(arg = driver_name,
                           choices = c("u_can_access",
                                       "access_jdbc42"))
  # access DB path verification ----
  if (! is.character(access_db_path)) {
    stop("invalid \"access_db_path\" argument.\n")
  }
  # function for access_jdbc42 driver ----
  if (driver_name == "access_jdbc42") {
    # driver path verification
    if (missing(access_jdbc42_driver_path)
        || ! is.character(access_jdbc42_driver_path)) {
      stop("invalid \"access_jdbc42_driver_path\" argument.\n")
    }
    # initializing Access JDBC driver
    access_jdbc_driver <- RJDBC::JDBC(driverClass = "com.hxtt.sql.access.AccessDriver",
                                      classPath = access_jdbc42_driver_path)

    # connection to Access database
    access_jdbc_connection <- RJDBC::dbConnect(access_jdbc_driver,
                                               paste0("jdbc:access:/",
                                                      access_db_path))
  } else {
    # function for u_can_access driver ----
    if (driver_name == "u_can_access") {
      # initializing Access JDBC driver
      class_path <- c(system.file("u_can_access",
                                  "jackcess-3.0.1.jar",
                                  package = "furdeb"),
                      system.file("u_can_access",
                                  "hsqldb-2.5.0.jar",
                                  package = "furdeb"),
                      system.file("u_can_access",
                                  "commons-logging-1.2.jar",
                                  package = "furdeb"),
                      system.file("u_can_access",
                                  "commons-lang3-3.8.1.jar",
                                  package = "furdeb"),
                      system.file("u_can_access",
                                  "ucanaccess-5.0.1.jar",
                                  package = "furdeb"))
      access_jdbc_driver <- RJDBC::JDBC(driverClass = "net.ucanaccess.jdbc.UcanaccessDriver",
                                        classPath = class_path)
      # connection to Access database
      access_jdbc_connection <- RJDBC::dbConnect(access_jdbc_driver,
                                                 paste0("jdbc:ucanaccess://",
                                                        access_db_path))
    }
  }
  return(access_jdbc_connection)
}
