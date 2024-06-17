#' @name access_dbconnection
#' @title Connection with Access database
#' @description Generate a common connection with an Access database using a JDBC driver.
#' @param driver_name {\link[base]{character}} expected. By default "u_can_access" (free driver). Access driver name. You can also choose the driver "access_jdbc42" (paid driver).
#' @param access_database_file_path {\link[base]{character}} expected. file path of the Access database (.mdb or .accdb expected).
#' @param access_jdbc42_driver_file_path {\link[base]{character}} expected. By default NULL. File path of the access_jdbc42 driver (.jar file expected). Mandatory if "access_jdbc42" is selected in the argument "driver_name".
#' @return The function return a R object with Access database identification of connection.
#' @details
#' Difference between drivers "u_can_access" and "access_jdbc42":
#' \itemize{
#'  \item{"u_can_access": }{the main advantage is it's a free java JDBC driver. To understand briefly the process behind, the Access database is converted in HSQLDB system. This conversion could take a long time, especially if the database if large. Furthermore, after the conversion, queries should be faster than if there run on an Access database.}
#'  \item{"access_jdbc42": }{this driver is paying. In opposition with the "u_can_access" driver, the Access database is not converted. The connection should be faster but the queries could be longer.}
#' }
#' @export
access_dbconnection <- function(driver_name = "u_can_access",
                                access_database_file_path,
                                access_jdbc42_driver_file_path = NULL) {
  # 1 - Arguments verification ----
  codama::r_type_checking(r_object = driver_name,
                          type = "character",
                          length = 1L,
                          allowed_value = c("u_can_access",
                                            "access_jdbc42"))
  codama::r_type_checking(r_object = access_database_file_path,
                          type = "character",
                          length = 1L)
  codama::file_path_checking(file_path = access_database_file_path,
                             extension = c("mdb",
                                           "accdb"))
  if (driver_name == "access_jdbc42") {
    codama::r_type_checking(r_object = access_jdbc42_driver_file_path,
                            type = "character",
                            length = 1L)
    codama::file_path_checking(file_path = access_jdbc42_driver_file_path,
                               extension = "jar")
  }
  # 2 - Global process ----
  if (driver_name == "access_jdbc42") {
    # initializing access_jdbc42 JDBC driver
    access_jdbc_driver <- RJDBC::JDBC(driverClass = "com.hxtt.sql.access.AccessDriver",
                                      classPath = access_jdbc42_driver_file_path)
    # connection to access database
    access_jdbc_connection <- RJDBC::dbConnect(access_jdbc_driver,
                                               paste0("jdbc:access:/",
                                                      access_database_file_path))
  } else {
    # function for  driver
    # initializing "u_can_access" JDBC driver
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
    # connection to access database
    access_jdbc_connection <- RJDBC::dbConnect(access_jdbc_driver,
                                               paste0("jdbc:ucanaccess://",
                                                      access_database_file_path))
  }
  return(list("database_name" = "access_database",
              "connection_information" = access_jdbc_connection))
}
