#' @name configuration_file
#' @title Configuration file
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @description Apply several options to R from a configuration file (able to create one if necessary).
#' @param new_configtype A logical vector: TRUE, FALSE, T or F. Provide false value is you want to use an existing configuration file. By default, the function create a new configuration file (=TRUE).
#' @param path_configtype Path (character) of the configuration file that you want to use.
#' @references \url{https://github.com/OB7-IRD/furdeb}
#' @return The function returns a list of configuration objects. If you have create a new configuration file, you can export your own configuration file (.csv format with separator ";").
#' @examples
#' # If you want to create a new configuration file
#' configuration_file()
#' # If you want to use an existing configuration file
#' configuration_file(new_configtype = FALSE,
#'                    path_configtype = "path_of_your_own_configuration_file")
#' @export
configuration_file <- function(new_configtype=TRUE,
                               path_configtype) {
  # Arguments verification ----
  if (! is.logical(new_configtype)) {
    stop("Missing argument \"new_configtype\" or value inside not TRUE or FALSE.",
         "\n",
         "Please correct it before running the function.")
  }
  if (new_configtype %in% c(FALSE, F) & (missing(path_configtype) || ! is.character(path_configtype))) {
    stop("Missing argument \"path_configtype\" or not correct path for the configuration file.",
         "\n",
         "Please correct it before running the function.")
  }

  if (new_configtype %in% c(FALSE, F)) {
    # Use configuration file ----
    # Create vector of configuration error ----
    error_list <- vector()
    # Import configuration file ----
    tmp <- read.table(file = path_configtype,
                      header = TRUE,
                      sep = ";")
    # Location of working directory ----
    if (length(as.character(tmp[which(tmp[, 2] == "location_of_working_directory"), 3])) == 0) {
      error_list <- c(error_list, "location_of_working_directory", "\n")
    } else {
      work_path <- (as.character(tmp[which(tmp[, 2] == "location_of_working_directory"), 3]))
    }
    # Location of output directory ----
    if (length(as.character(tmp[which(tmp[, 2] == "location_of_output_directory"), 3])) != 0) {
      output_loc <- as.character(tmp[which(tmp[, 2] == "location_of_output_directory"), 3])
    }
    # Location of functions directory ----
    if (length(as.character(tmp[which(tmp[, 2] == "location_of_functions_directory"), 3])) != 0) {
      functions_loc <- as.character(tmp[which(tmp[, 2] == "location_of_functions_directory"), 3])
    }
    # Location of turbobat file ----
    if (length(as.character(tmp[which(tmp[, 2] == "location_of_turbobat_file"), 3])) != 0) {
      turbobat_loc <- as.character(tmp[which(tmp[, 2] == "location_of_turbobat_file"), 3])
    }
    # Location of queries directory ----
    if (length(as.character(tmp[which(tmp[, 2] == "location_of_queries_directory"), 3])) != 0) {
      queries_loc <- as.character(tmp[which(tmp[, 2] == "location_of_queries_directory"), 3])
    }
    # Define memory allowed for java ----
    if (length(as.character(tmp[which(tmp[, 2] == "memory_allowed_for_java"), 3])) == 0) {
      options(java.parameters = paste("-Xmx",
                                      as.character(tmp[which(tmp[, 2] == "memory_allowed_for_java"), 3]),
                                      sep = ""))
    }
    # Location of java folder ----
    if (length(as.character(tmp[which(tmp[, 2] == "location_of_java_folder"), 3])) == 0) {
      Sys.setenv(JAVA_HOME = as.character(tmp[which(tmp[, 2] == "location_of_java_folder"), 3]))
    }
    #Connexion with Access database ----
    #Initializing of Access JDBC driver
    if (length(as.character(tmp[which(tmp[, 2] == "location_of_access_jdbc_driver"), 3])) != 0) {
      jdbc_access_driver_loc <- as.character(tmp[which(tmp[, 2] == "location_of_access_jdbc_driver"), 3])
    }
    #Access driver's identification
    if (length(as.character(tmp[which(tmp[, 2] == "access_driver_class_identification"), 3])) != 0) {
      jdbc_access_driverclass <- as.character(tmp[which(tmp[, 2] == "access_driver_class_identification"), 3])
    }
    #Access database location
    if (length(as.character(tmp[which(tmp[, 2] == "location_of_access_db"), 3])) != 0) {
      access_db_loc <- as.character(tmp[which(tmp[, 2] == "location_of_access_db"), 3])
    }
    #Connexion with t3+ database ----
    #DB t3+ user identification
    if (length(as.character(tmp[which(tmp[, 2] == "t3plus_user_id"), 3])) != 0) {
      t3plus_user <- as.character(tmp[which(tmp[, 2] == "t3plus_user_id"), 3])
    }
    #DB t3+ password identification
    if (length(as.character(tmp[which(tmp[, 2] == "t3plus_password_id"), 3])) != 0) {
      t3plus_password <- as.character(tmp[which(tmp[, 2] == "t3plus_password_id"), 3])
    }
    #DB t3+ database name
    if (length(as.character(tmp[which(tmp[, 2] == "t3plus_dbname"), 3])) != 0) {
      t3plus_dbname <- as.character(tmp[which(tmp[, 2] == "t3plus_dbname"), 3])
    }
    #DB t3+ host identification
    if (length(as.character(tmp[which(tmp[, 2] == "t3plus_host"), 3])) != 0) {
      t3plus_host <- as.character(tmp[which(tmp[, 2] == "t3plus_host"), 3])
    }
    #DB t3+ host identification
    if (length(as.character(tmp[which(tmp[, 2] == "t3plus_port"), 3])) != 0) {
      t3plus_port <- as.character(tmp[which(tmp[, 2] == "t3plus_port"), 3])
    }
    #Connexion with observe database ----
    #DB observe identification
    if (length(as.character(tmp[which(tmp[, 2] == "observe_user_id"), 3])) != 0) {
      observe_user <- as.character(tmp[which(tmp[, 2] == "observe_user_id"), 3])
    }
    #DB observe password identification
    if (length(as.character(tmp[which(tmp[, 2] == "observe_password_id"), 3])) != 0) {
      observe_password <- as.character(tmp[which(tmp[, 2] == "observe_password_id"), 3])
    }
    #DB observe database name
    if (length(as.character(tmp[which(tmp[, 2] == "observe_dbname"), 3])) != 0) {
      observe_dbname <- as.character(tmp[which(tmp[, 2] == "observe_dbname"), 3])
    }
    #DB observe host identification
    if (length(as.character(tmp[which(tmp[, 2] == "observe_host"), 3])) != 0) {
      observe_host <- as.character(tmp[which(tmp[, 2] == "observe_host"), 3])
    }
    #DB observe host identification
    if (length(as.character(tmp[which(tmp[, 2] == "observe_port"), 3])) != 0) {
      observe_port <- as.character(tmp[which(tmp[, 2] == "observe_port"), 3])
    }
    #Connexion with balbaya database ----
    #DB balbaya identification
    if (length(as.character(tmp[which(tmp[, 2] == "balbaya_user_id"), 3])) != 0) {
      balbaya_user <- as.character(tmp[which(tmp[, 2] == "balbaya_user_id"), 3])
    }
    #DB balbaya password identification
    if (length(as.character(tmp[which(tmp[, 2] == "balbaya_password_id"), 3])) != 0) {
      balbaya_password <- as.character(tmp[which(tmp[, 2] == "balbaya_password_id"), 3])
    }
    #DB balbaya database name
    if (length(as.character(tmp[which(tmp[, 2] == "balbaya_dbname"), 3])) != 0) {
      balbaya_dbname <- as.character(tmp[which(tmp[, 2] == "balbaya_dbname"), 3])
    }
    #DB balbaya host identification
    if (length(as.character(tmp[which(tmp[, 2] == "balbaya_host"), 3])) != 0) {
      balbaya_host <- as.character(tmp[which(tmp[, 2] == "balbaya_host"), 3])
    }
    #DB balbaya host identification
    if (length(as.character(tmp[which(tmp[, 2] == "balbaya_port"), 3])) != 0) {
      balbaya_port <- as.character(tmp[which(tmp[, 2] == "balbaya_port"), 3])
    }
    #Connexion with sardara database ----
    #DB sardara identification
    if (length(as.character(tmp[which(tmp[, 2] == "sardara_user_id"), 3])) != 0) {
      sardara_user <- as.character(tmp[which(tmp[, 2] == "sardara_user_id"), 3])
    }
    #DB sardara password identification
    if (length(as.character(tmp[which(tmp[, 2] == "sardara_password_id"), 3])) != 0) {
      sardara_password <- as.character(tmp[which(tmp[, 2] == "sardara_password_id"), 3])
    }
    #DB sardara database name
    if (length(as.character(tmp[which(tmp[, 2] == "sardara_dbname"), 3])) != 0) {
      sardara_dbname <- as.character(tmp[which(tmp[, 2] == "sardara_dbname"), 3])
    }
    #DB sardara host identification
    if (length(as.character(tmp[which(tmp[, 2] == "sardara_host"), 3])) != 0) {
      sardara_host <- as.character(tmp[which(tmp[, 2] == "sardara_host"), 3])
    }
    #DB sardara host identification
    if (length(as.character(tmp[which(tmp[, 2] == "sardara_port"), 3])) != 0) {
      sardara_port <- as.character(tmp[which(tmp[, 2] == "sardara_port"), 3])
    }
    # Error list identification ----
    if (length(error_list) != 0) {
      stop("Be careful! Your configuration file is incomplete", "\n", "Please check this or these missing arguments :", "\n", error_list, call. = FALSE)
    }
    # Final disposal for extraction ----
    configfile <- list("work_path" = work_path)
    if (exists("output_loc")) {
      configfile <- c(configfile,
                      "output_loc" = output_loc)
    }
    if (exists("functions_loc")) {
      configfile <- c(configfile,
                      "functions_loc" = functions_loc)
    }
    if (exists("queries_loc")) {
      configfile <- c(configfile,
                      "queries_loc" = queries_loc)
    }
    if (exists("turbobat_loc")) {
      configfile <- c(configfile,
                      "turbobat_loc" = turbobat_loc)
    }
    if (exists("jdbc_access_driver_loc")) {
      configfile <- c(configfile,
                      "jdbc_access_driver_loc" = jdbc_access_driver_loc)
    }
    if (exists("jdbc_access_driverclass")) {
      configfile <- c(configfile,
                      "jdbc_access_driverclass" = jdbc_access_driverclass)
    }
    if (exists("access_db_loc")) {
      configfile <- c(configfile,
                      "access_db_loc" = access_db_loc)
    }
    if (exists("t3plus_user")) {
      configfile <- c(configfile,
                      "t3plus_user" = t3plus_user)
    }
    if (exists("t3plus_password")) {
      configfile <- c(configfile,
                      "t3plus_password" = t3plus_password)
    }
    if (exists("t3plus_dbname")) {
      configfile <- c(configfile,
                      "t3plus_dbname" = t3plus_dbname)
    }
    if (exists("t3plus_host")) {
      configfile <- c(configfile,
                      "t3plus_host" = t3plus_host)
    }
    if (exists("t3plus_port")) {
      configfile <- c(configfile,
                      "t3plus_port" = t3plus_port)
    }
    if (exists("observe_user")) {
      configfile <- c(configfile,
                      "observe_user" = observe_user)
    }
    if (exists("observe_password")) {
      configfile <- c(configfile,
                      "observe_password" = observe_password)
    }
    if (exists("observe_dbname")) {
      configfile <- c(configfile,
                      "observe_dbname" = observe_dbname)
    }
    if (exists("observe_host")) {
      configfile <- c(configfile,
                      "observe_host" = observe_host)
    }
    if (exists("observe_port")) {
      configfile <- c(configfile,
                      "observe_port" = observe_port)
    }
    if (exists("balbaya_user")) {
      configfile <- c(configfile,
                      "balbaya_user" = balbaya_user)
    }
    if (exists("balbaya_password")) {
      configfile <- c(configfile,
                      "balbaya_password" = balbaya_password)
    }
    if (exists("balbaya_dbname")) {
      configfile <- c(configfile,
                      "balbaya_dbname" = balbaya_dbname)
    }
    if (exists("balbaya_host")) {
      configfile <- c(configfile,
                      "balbaya_host" = balbaya_host)
    }
    if (exists("balbaya_port")) {
      configfile <- c(configfile,
                      "balbaya_port" = balbaya_port)
    }
    if (exists("sardara_user")) {
      configfile <- c(configfile,
                      "sardara_user" = sardara_user)
    }
    if (exists("sardara_password")) {
      configfile <- c(configfile,
                      "sardara_password" = sardara_password)
    }
    if (exists("sardara_dbname")) {
      configfile <- c(configfile,
                      "sardara_dbname" = sardara_dbname)
    }
    if (exists("sardara_host")) {
      configfile <- c(configfile,
                      "sardara_host" = sardara_host)
    }
    if (exists("sardara_port")) {
      configfile <- c(configfile,
                      "sardara_port" = sardara_port)
    }
    return(configfile)
  } else {
    # Create a new configuration file ----
    tmpnew <- as.data.frame(matrix(nrow = 0, ncol = 3))
    colnames(tmpnew) <- c("section",
                          "sub_section",
                          "value")
    # Location of working directory ----
    cat("Selection of working directory", "\n")
    tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_for_r_environnement"
    tmpnew[dim(tmpnew)[1], 2] <- "location_of_working_directory"
    if (as.character(Sys.info()['sysname']) == "Windows") {
      tmpnew[dim(tmpnew)[1], 3] <- choose.dir(default = getwd(),
                                              caption = "Select working directory")
    } else {
      if (as.character(Sys.info()['sysname']) == "Darwin") {
        choose.dir <- function() {
          system("osascript -e 'tell app \"R\" to POSIX path of (choose folder with prompt \"Choose Folder:\")' > /tmp/R_folder",
                 intern = FALSE, ignore.stderr = TRUE)
          p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
          return(ifelse(length(p), p, NA))
        }
        tmpnew[dim(tmpnew)[1], 3] <- choose.dir()
      } else {
        stop("Your OS is not supported yet")
      }
    }
    work_path <- tmpnew[dim(tmpnew)[1], 3]
    # Location of output directory ----
    cat("Do you need an output directory?", "\n")
    cat("(yes, no)", "\n")
    output_answer <- readLines(n = 1)
    while (!(output_answer %in% c("yes", "no"))) {
      cat("Be careful! Your answer is not correct", "\n")
      cat("Do you need an output directory?", "\n")
      cat("(yes, no)", "\n")
      output_answer <- readLines(n = 1)
    }
    if (output_answer == "yes") {
      cat("Selection of output directory", "\n")
      tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_for_r_environnement"
      tmpnew[dim(tmpnew)[1], 2] <- "location_of_output_directory"
      if (as.character(Sys.info()['sysname']) == "Windows") {
        tmpnew[dim(tmpnew)[1], 3] <- choose.dir(default = work_path,
                                                caption = "Select output directory")
      } else {
        if (as.character(Sys.info()['sysname']) == "Darwin") {
          choose.dir <- function() {
            system("osascript -e 'tell app \"R\" to POSIX path of (choose folder with prompt \"Choose Folder:\")' > /tmp/R_folder",
                   intern = FALSE, ignore.stderr = TRUE)
            p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
            return(ifelse(length(p), p, NA))
          }
          tmpnew[dim(tmpnew)[1], 3] <- choose.dir()
        } else {
          stop("Your OS is not supported yet")
        }
      }
      output_loc <- tmpnew[dim(tmpnew)[1], 3]
    }
    # Location of functions directory ----
    cat("Do you need to use functions from a directory?", "\n")
    cat("(yes, no)", "\n")
    functions_answer <- readLines(n = 1)
    while (!(functions_answer %in% c("yes", "no"))) {
      cat("Be careful! Your answer is not correct", "\n")
      cat("Do you need to use functions from a directory?", "\n")
      cat("(yes, no)", "\n")
      functions_answer <- readLines(n = 1)
    }
    if (functions_answer == "yes") {
      cat("Selection of functions directory", "\n")
      tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_for_r_environnement"
      tmpnew[dim(tmpnew)[1], 2] <- "location_of_functions_directory"
      if (as.character(Sys.info()['sysname']) == "Windows") {
        tmpnew[dim(tmpnew)[1], 3] <- choose.dir(default = work_path,
                                                caption = "Select functions directory")
      } else {
        if (as.character(Sys.info()['sysname']) == "Darwin") {
          choose.dir <- function() {
            system("osascript -e 'tell app \"R\" to POSIX path of (choose folder with prompt \"Choose Folder:\")' > /tmp/R_folder",
                   intern = FALSE, ignore.stderr = TRUE)
            p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
            return(ifelse(length(p), p, NA))
          }
          tmpnew[dim(tmpnew)[1], 3] <- choose.dir()
        } else {
          stop("Your OS is not supported yet")
        }
      }
      functions_loc <- tmpnew[dim(tmpnew)[1], 3]
    }
    # Location of turbobat file ----
    cat("Do you need to use the turbobat file?", "\n")
    cat("(yes, no)", "\n")
    turbobat_answer <- readLines(n = 1)
    while (!(turbobat_answer %in% c("yes", "no"))) {
      cat("Be careful! Your answer is not correct", "\n")
      cat("Do you need to use the turbobat file?", "\n")
      cat("(yes, no)", "\n")
      turbobat_answer <- readLines(n = 1)
    }
    if (turbobat_answer == "yes") {
      cat("Selection turbobat location", "\n")
      tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_for_r_environnement"
      tmpnew[dim(tmpnew)[1], 2] <- "location_of_turbobat_file"
      tmpnew[dim(tmpnew)[1], 3] <- file.choose()
      turbobat_loc <- tmpnew[dim(tmpnew)[1], 3]
    }
    cat("Do you want to initiate at least one database connection?", "\n")
    cat("(yes, no)", "\n")
    db_answer <- readLines(n = 1)
    while (!(db_answer %in% c("yes", "no"))) {
      cat("Be careful! Your answer is not correct", "\n")
      cat("Do you want to initiate at least one database connection?", "\n")
      cat("(yes, no)", "\n")
      db_answer <- readLines(n = 1)
    }
    if (db_answer == "yes") {
      # Location of queries directory ----
      cat("Do you use sql file(s) for your queries on the database?", "\n")
      cat("(yes, no)", "\n")
      queries_answer <- readLines(n = 1)
      while (!(queries_answer %in% c("yes", "no"))) {
        cat("Be careful! Your answer is not correct", "\n")
        cat("Do you use sql file(s) for your queries on the database?", "\n")
        cat("(yes, no)", "\n")
        queries_answer <- readLines(n = 1)
      }
      if (queries_answer == "yes") {
        cat("Selection of queries directory", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_for_r_environnement"
        tmpnew[dim(tmpnew)[1], 2] <- "location_of_queries_directory"
        if (as.character(Sys.info()['sysname']) == "Windows") {
          tmpnew[dim(tmpnew)[1], 3] <- choose.dir(default = work_path,
                                                  caption = "Select functions directory")
        } else {
          if (as.character(Sys.info()['sysname']) == "Darwin") {
            choose.dir <- function() {
              system("osascript -e 'tell app \"R\" to POSIX path of (choose folder with prompt \"Choose Folder:\")' > /tmp/R_folder",
                     intern = FALSE, ignore.stderr = TRUE)
              p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
              return(ifelse(length(p), p, NA))
            }
            tmpnew[dim(tmpnew)[1], 3] <- choose.dir()
          } else {
            stop("Your OS is not supported yet")
          }
        }
        queries_loc <- tmpnew[dim(tmpnew)[1], 3]
      }
      # Define memory allowed for java ----
      cat("Define memory allowed for java (by default 512mo)", "\n")
      cat("This step must be performed prior to loading any packages", "\n")
      cat("For information, the value need is numeric value of memory allowed follow by unit, g (for gigabyte) or m (for megabyte)", "\n")
      tmpnew[dim(tmpnew)[1] + 1, 1] <- "configuration_for_java"
      tmpnew[dim(tmpnew)[1], 2] <- "memory_allowed_for_java"
      tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
      options(java.parameters = paste("-Xmx",
                                      as.character(tmpnew[dim(tmpnew)[1], 3]),
                                      sep = ""))
      # Location of java folder ----
      cat("Selection of java folder", "\n")
      tmpnew[dim(tmpnew)[1] + 1, 1] <- "configuration_for_java"
      tmpnew[dim(tmpnew)[1], 2] <- "location_of_java_folder"
      if (as.character(Sys.info()['sysname']) == "Windows") {
        tmpnew[dim(tmpnew)[1], 3] <- choose.dir(caption = "Select java folder")
      } else {
        if (as.character(Sys.info()['sysname']) == "Darwin") {
          choose.dir <- function() {
            system("osascript -e 'tell app \"R\" to POSIX path of (choose folder with prompt \"Choose Folder:\")' > /tmp/R_folder",
                   intern = FALSE, ignore.stderr = TRUE)
            p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
            return(ifelse(length(p), p, NA))
          }
          tmpnew[dim(tmpnew)[1], 3] <- choose.dir()
        } else {
          stop("Your OS is not supported yet")
        }
      }
      Sys.setenv(JAVA_HOME = as.character(tmpnew[dim(tmpnew)[1], 3]))
      # Connexion with Access database ----
      cat("Do you want to establish a connexion with an Access database?", "\n")
      cat("(yes, no)", "\n")
      access_answer <- readLines(n = 1)
      while (!(access_answer %in% c("yes", "no"))) {
        cat("Be careful! Your answer is not correct", "\n")
        cat("Do you want to establish a connexion with an Access database?", "\n")
        cat("(yes, no)", "\n")
        access_answer <- readLines(n = 1)
      }
      if (access_answer == "yes") {
        #Initializing of Access JDBC driver
        cat("Select the location of Access JDBC driver", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_access_db"
        tmpnew[dim(tmpnew)[1], 2] <- "location_of_access_jdbc_driver"
        tmpnew[dim(tmpnew)[1], 3] <- file.choose()
        jdbc_access_driver_loc <- tmpnew[dim(tmpnew)[1], 3]
        #Access driver's identification
        cat("Enter the driver class of Access JDBC driver", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_access_db"
        tmpnew[dim(tmpnew)[1], 2] <- "access_driver_class_identification"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        jdbc_access_driverclass <- tmpnew[dim(tmpnew)[1], 3]
        #Access database location
        cat("Select the location of Access database", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_access_db"
        tmpnew[dim(tmpnew)[1], 2] <- "location_of_access_db"
        tmpnew[dim(tmpnew)[1], 3] <- file.choose()
        access_db_loc <- tmpnew[dim(tmpnew)[1], 3]
      }
      # Connexion with t3+ database ----
      cat("Do you want to establish a connexion with the T3+ database?", "\n")
      cat("(yes, no)", "\n")
      t3plus_answer <- readLines(n = 1)
      while (!(t3plus_answer %in% c("yes", "no"))) {
        cat("Be careful! Your answer is not correct", "\n")
        cat("Do you want to establish a connexion with the T3+ database?", "\n")
        cat("(yes, no)", "\n")
        t3plus_answer <- readLines(n = 1)
      }
      if (t3plus_answer == "yes") {
        #DB t3+ user identification
        cat("T3+ user login", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_t3plus_db"
        tmpnew[dim(tmpnew)[1], 2] <- "t3plus_user_id"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        t3plus_user <- tmpnew[dim(tmpnew)[1], 3]
        #DB t3+ password identification
        cat("T3+ password", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_t3plus_db"
        tmpnew[dim(tmpnew)[1], 2] <- "t3plus_password_id"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        t3plus_password <- tmpnew[dim(tmpnew)[1], 3]
        #DB t3+ database name
        cat("T3+ database name", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_t3plus_db"
        tmpnew[dim(tmpnew)[1], 2] <- "t3plus_dbname"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        t3plus_dbname <- tmpnew[dim(tmpnew)[1], 3]
        #DB t3+ host identification
        cat("T3+ host identification", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_t3plus_db"
        tmpnew[dim(tmpnew)[1], 2] <- "t3plus_host"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        t3plus_host <- tmpnew[dim(tmpnew)[1], 3]
        #DB t3+ port identification
        cat("T3+ port identification", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_t3plus_db"
        tmpnew[dim(tmpnew)[1], 2] <- "t3plus_port"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        t3plus_port <- tmpnew[dim(tmpnew)[1], 3]
      }
      # Connexion with observe database ----
      cat("Do you want to establish a connexion with the observe database?", "\n")
      cat("(yes, no)", "\n")
      observe_answer <- readLines(n = 1)
      while (!(observe_answer %in% c("yes", "no"))) {
        cat("Be careful! Your answer is not correct", "\n")
        cat("Do you want to establish a connexion with the observe database?", "\n")
        cat("(yes, no)", "\n")
        observe_answer <- readLines(n = 1)
      }
      if (observe_answer == "yes") {
        #DB observe user identification
        cat("observe user login", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_observe_db"
        tmpnew[dim(tmpnew)[1], 2] <- "observe_user_id"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        observe_user <- tmpnew[dim(tmpnew)[1], 3]
        #DB observe password identification
        cat("observe password", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_observe_db"
        tmpnew[dim(tmpnew)[1], 2] <- "observe_password_id"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        observe_password <- tmpnew[dim(tmpnew)[1], 3]
        #DB observe database name
        cat("observe database name", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_observe_db"
        tmpnew[dim(tmpnew)[1], 2] <- "observe_dbname"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        observe_dbname <- tmpnew[dim(tmpnew)[1], 3]
        #DB observe host identification
        cat("observe host identification", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_observe_db"
        tmpnew[dim(tmpnew)[1], 2] <- "observe_host"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        observe_host <- tmpnew[dim(tmpnew)[1], 3]
        #DB observe port identification
        cat("observe port identification", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_observe_db"
        tmpnew[dim(tmpnew)[1], 2] <- "observe_port"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        observe_port <- tmpnew[dim(tmpnew)[1], 3]
      }
      # Connexion with balbaya database ----
      cat("Do you want to establish a connexion with the balbaya database?", "\n")
      cat("(yes, no)", "\n")
      balbaya_answer <- readLines(n = 1)
      while (!(balbaya_answer %in% c("yes", "no"))) {
        cat("Be careful! Your answer is not correct", "\n")
        cat("Do you want to establish a connexion with the balbaya database?", "\n")
        cat("(yes, no)", "\n")
        balbaya_answer <- readLines(n = 1)
      }
      if (balbaya_answer == "yes") {
        #DB balbaya user identification
        cat("balbaya user login", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_balbaya_db"
        tmpnew[dim(tmpnew)[1], 2] <- "balbaya_user_id"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        balbaya_user <- tmpnew[dim(tmpnew)[1], 3]
        #DB balbaya password identification
        cat("balbaya password", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_balbaya_db"
        tmpnew[dim(tmpnew)[1], 2] <- "balbaya_password_id"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        balbaya_password <- tmpnew[dim(tmpnew)[1], 3]
        #DB balbaya database name
        cat("balbaya database name", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_balbaya_db"
        tmpnew[dim(tmpnew)[1], 2] <- "balbaya_dbname"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        balbaya_dbname <- tmpnew[dim(tmpnew)[1], 3]
        #DB balbaya host identification
        cat("balbaya host identification", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_balbaya_db"
        tmpnew[dim(tmpnew)[1], 2] <- "balbaya_host"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        balbaya_host <- tmpnew[dim(tmpnew)[1], 3]
        #DB balbaya port identification
        cat("balbaya port identification", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_balbaya_db"
        tmpnew[dim(tmpnew)[1], 2] <- "balbaya_port"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        balbaya_port <- tmpnew[dim(tmpnew)[1], 3]
      }
      # Connexion with sardara database ----
      cat("Do you want to establish a connexion with the sardara database?", "\n")
      cat("(yes, no)", "\n")
      sardara_answer <- readLines(n = 1)
      while (!(sardara_answer %in% c("yes", "no"))) {
        cat("Be careful! Your answer is not correct", "\n")
        cat("Do you want to establish a connexion with the sardara database?", "\n")
        cat("(yes, no)", "\n")
        sardara_answer <- readLines(n = 1)
      }
      if (sardara_answer == "yes") {
        #DB sardara user identification
        cat("sardara user login", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_sardara_db"
        tmpnew[dim(tmpnew)[1], 2] <- "sardara_user_id"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        sardara_user <- tmpnew[dim(tmpnew)[1], 3]
        #DB sardara password identification
        cat("sardara password", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_sardara_db"
        tmpnew[dim(tmpnew)[1], 2] <- "sardara_password_id"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        sardara_password <- tmpnew[dim(tmpnew)[1], 3]
        #DB sardara database name
        cat("sardara database name", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_sardara_db"
        tmpnew[dim(tmpnew)[1], 2] <- "sardara_dbname"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        sardara_dbname <- tmpnew[dim(tmpnew)[1], 3]
        #DB sardara host identification
        cat("sardara host identification", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_sardara_db"
        tmpnew[dim(tmpnew)[1], 2] <- "sardara_host"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        sardara_host <- tmpnew[dim(tmpnew)[1], 3]
        #DB sardara port identification
        cat("sardara port identification", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_of_sardara_db"
        tmpnew[dim(tmpnew)[1], 2] <- "sardara_port"
        tmpnew[dim(tmpnew)[1], 3] <- readLines(n = 1)
        sardara_port <- tmpnew[dim(tmpnew)[1], 3]
      }
    }
    # Extract configuration file informations ----
    cat("Do you want to export the configuration file?", "\n")
    cat("Be careful! Existing file in the folder will delete.", "\n")
    cat("If you will said yes, the configuration file will be extract in the working directory cited above.", "\n")
    cat("(yes, no)", "\n")
    final_answer <- readLines(n = 1)
    while (!(final_answer %in% c("yes", "no"))) {
      cat("Be careful! The answer is not correct", "\n")
      cat("Do you want to export the configuration file?", "\n")
      cat("Be careful! Existing file in the folder will delete.", "\n")
      cat("If you will said yes, the configuration file will be extract in the working directory cited above.", "\n")
      cat("(yes, no)", "\n")
      final_answer <- readLines(n = 1)
    }
    if (final_answer == "yes") {
      write.csv2(tmpnew,
                 file = file.path(work_path,
                                  "configfile.csv",
                                  fsep = "\\"),
                row.names = FALSE)
      cat(paste0("File extracted in: ", work_path))
    }
    # Final disposal for extraction ----
    configfile <- list("work_path" = work_path)
    if (exists("output_loc")) {
      configfile <- c(configfile,
                      "output_loc" = output_loc)
    }
    if (exists("functions_loc")) {
      configfile <- c(configfile,
                      "functions_loc" = functions_loc)
    }
    if (exists("queries_loc")) {
      configfile <- c(configfile,
                      "queries_loc" = queries_loc)
    }
    if (exists("turbobat_loc")) {
      configfile <- c(configfile,
                      "turbobat_loc" = turbobat_loc)
    }
    if (exists("jdbc_access_driver_loc")) {
      configfile <- c(configfile,
                      "jdbc_access_driver_loc" = jdbc_access_driver_loc)
    }
    if (exists("jdbc_access_driverclass")) {
      configfile <- c(configfile,
                      "jdbc_access_driverclass" = jdbc_access_driverclass)
    }
    if (exists("access_db_loc")) {
      configfile <- c(configfile,
                      "access_db_loc" = access_db_loc)
    }
    if (exists("t3plus_user")) {
      configfile <- c(configfile,
                      "t3plus_user" = t3plus_user)
    }
    if (exists("t3plus_password")) {
      configfile <- c(configfile,
                      "t3plus_password" = t3plus_password)
    }
    if (exists("t3plus_dbname")) {
      configfile <- c(configfile,
                      "t3plus_dbname" = t3plus_dbname)
    }
    if (exists("t3plus_host")) {
      configfile <- c(configfile,
                      "t3plus_host" = t3plus_host)
    }
    if (exists("t3plus_port")) {
      configfile <- c(configfile,
                      "t3plus_port" = t3plus_port)
    }
    if (exists("observe_user")) {
      configfile <- c(configfile,
                      "observe_user" = observe_user)
    }
    if (exists("observe_password")) {
      configfile <- c(configfile,
                      "observe_password" = observe_password)
    }
    if (exists("observe_dbname")) {
      configfile <- c(configfile,
                      "observe_dbname" = observe_dbname)
    }
    if (exists("observe_host")) {
      configfile <- c(configfile,
                      "observe_host" = observe_host)
    }
    if (exists("observe_port")) {
      configfile <- c(configfile,
                      "observe_port" = observe_port)
    }
    if (exists("balbaya_user")) {
      configfile <- c(configfile,
                      "balbaya_user" = balbaya_user)
    }
    if (exists("balbaya_password")) {
      configfile <- c(configfile,
                      "balbaya_password" = balbaya_password)
    }
    if (exists("balbaya_dbname")) {
      configfile <- c(configfile,
                      "balbaya_dbname" = balbaya_dbname)
    }
    if (exists("balbaya_host")) {
      configfile <- c(configfile,
                      "balbaya_host" = balbaya_host)
    }
    if (exists("balbaya_port")) {
      configfile <- c(configfile,
                      "balbaya_port" = balbaya_port)
    }
    if (exists("sardara_user")) {
      configfile <- c(configfile,
                      "sardara_user" = sardara_user)
    }
    if (exists("sardara_password")) {
      configfile <- c(configfile,
                      "sardara_password" = sardara_password)
    }
    if (exists("sardara_dbname")) {
      configfile <- c(configfile,
                      "sardara_dbname" = sardara_dbname)
    }
    if (exists("sardara_host")) {
      configfile <- c(configfile,
                      "sardara_host" = sardara_host)
    }
    if (exists("sardara_port")) {
      configfile <- c(configfile,
                      "sardara_port" = sardara_port)
    }
    return(configfile)
  }
}
