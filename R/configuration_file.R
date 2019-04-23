#' @name configuration_file
#' @title Configuration file
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @description Apply several options to R from a configuration file (able to create one if necessary).
#' @param configtype Vector of character. For details see below the specification section.
#' @references \url{https://github.com/OB7-IRD/toolbox}
#' @return The function returns a list of configuration objects. If none configuration file has been provided at the beginning, you can export your own configuration file (.csv format).
#' @section Specification:
#' You can specify two kinds of configurations:
#' \itemize{
#' \item{"base" for a base configuration without a database's connection}
#' \item{"db" for an advanced configuration with a database's connection}
#' }
#' @examples
#' #If you want to use basic configuration without a database's connection
#' configuration_file("base")
#'
#' #If you want to use advanced configuration with a database's connection
#' configuration_file("db")
#' @export
configuration_file <- function(configtype) {
  if (configtype %in% c("base", "db")) {
    cat("Do you want to use parameters of an existing csv configuration file ?", "\n")
    cat("Be careful! separator of csv file need to be ';'", "\n")
    cat("(yes, no)", "\n")
    main_answer <- readLines(n = 1)
    while (!(main_answer %in% c("yes", "no"))) {
      cat("Be careful! Your answer is not correct", "\n")
      cat("Do you want to use parameters of an existing csv configuration file ?", "\n")
      cat("Be careful! separator of csv file need to be ';'", "\n")
      cat("(yes, no)", "\n")
      main_answer <- readLines(n = 1)
    }
    if (main_answer == "yes") {
      # Use configuration file ----
      # Create vector of configuration error ----
      error_list <- vector()
      # Import configuration file ----
      cat("Please select your configuration file", "\n")
      tmp <- read.table(file = file.choose(),
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
      if (configtype == "db") {
        # Location of queries directory ----
        if (length(as.character(tmp[which(tmp[, 2] == "location_of_queries_directory"), 3])) != 0) {
          queries_loc <- as.character(tmp[which(tmp[, 2] == "location_of_queries_directory"), 3])
        }
        # Define memory allowed for java ----
        if (length(as.character(tmp[which(tmp[, 2] == "memory_allowed_for_java"), 3])) == 0) {
          error_list <- c(error_list, "memory_allowed_for_java", "\n")
        } else {
          options(java.parameters = paste("-Xmx",
                                          as.character(tmp[which(tmp[, 2] == "memory_allowed_for_java"), 3]),
                                          sep = ""))
        }
        # Location of java folder ----
        if (length(as.character(tmp[which(tmp[, 2] == "location_of_java_folder"), 3])) == 0) {
          error_list <- c(error_list,
                          "location_of_java_folder",
                          "\n")
        } else {
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
      }
      # Error list identification ----
      if (length(error_list) != 0) {
        stop("Be careful! Your configuration file is incomplete", "\n", "Please check this or these missing arguments :", "\n", error_list, call. = FALSE)
      }
      # Extract configuration file informations ----
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
      return(configfile)
    } else {
      # Without a configuration file ----
      tmpnew <- as.data.frame(matrix(nrow = 0, ncol = 3))
      colnames(tmpnew) <- c("section",
                            "sub_section",
                            "value")
      # Location of working directory ----
      cat("Selection of working directory", "\n")
      tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_for_r_environnement"
      tmpnew[dim(tmpnew)[1], 2] <- "location_of_working_directory"
      tmpnew[dim(tmpnew)[1], 3] <- choose.dir(default = getwd(),
                                              caption = "Select working directory")
      work_path <- tmpnew[dim(tmpnew)[1], 3]
      # Location of output directory ----
      cat("Do you need an output directory ?", "\n")
      cat("(yes, no)", "\n")
      output_answer <- readLines(n = 1)
      while (!(output_answer %in% c("yes", "no"))) {
        cat("Be careful! Your answer is not correct", "\n")
        cat("Do you need an output directory ?", "\n")
        cat("(yes, no)", "\n")
        output_answer <- readLines(n = 1)
      }
      if (output_answer == "yes") {
        cat("Selection of output directory", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_for_r_environnement"
        tmpnew[dim(tmpnew)[1], 2] <- "location_of_output_directory"
        tmpnew[dim(tmpnew)[1], 3] <- choose.dir(default = work_path,
                                                caption = "Select output directory")
        output_loc <- tmpnew[dim(tmpnew)[1], 3]
      }
      # Location of functions directory ----
      cat("Do you need to use functions from a directory ?", "\n")
      cat("(yes, no)", "\n")
      functions_answer <- readLines(n = 1)
      while (!(functions_answer %in% c("yes", "no"))) {
        cat("Be careful! Your answer is not correct", "\n")
        cat("Do you need to use functions from a directory ?", "\n")
        cat("(yes, no)", "\n")
        functions_answer <- readLines(n = 1)
      }
      if (functions_answer == "yes") {
        cat("Selection of functions directory", "\n")
        tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_for_r_environnement"
        tmpnew[dim(tmpnew)[1], 2] <- "location_of_functions_directory"
        tmpnew[dim(tmpnew)[1], 3] <- choose.dir(default = work_path,
                                                caption = "Select functions directory")
        functions_loc <- tmpnew[dim(tmpnew)[1], 3]
      }
      # Location of turbobat file ----
      cat("Do you need to use the turbobat file ?", "\n")
      cat("(yes, no)", "\n")
      turbobat_answer <- readLines(n = 1)
      while (!(turbobat_answer %in% c("yes", "no"))) {
        cat("Be careful! Your answer is not correct", "\n")
        cat("Do you need to use the turbobat file ?", "\n")
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
      if (configtype == "db") {
        # Location of queries directory ----
        cat("Do you use sql file(s) for your queries on the database ?", "\n")
        cat("(yes, no)", "\n")
        queries_answer <- readLines(n = 1)
        while (!(queries_answer %in% c("yes", "no"))) {
          cat("Be careful! Your answer is not correct", "\n")
          cat("Do you use sql file(s) for your queries on the database ?", "\n")
          cat("(yes, no)", "\n")
          queries_answer <- readLines(n = 1)
        }
        if (queries_answer == "yes") {
          cat("Selection of queries directory", "\n")
          tmpnew[dim(tmpnew)[1] + 1, 1] <- "parameters_for_r_environnement"
          tmpnew[dim(tmpnew)[1], 2] <- "location_of_queries_directory"
          tmpnew[dim(tmpnew)[1], 3] <- choose.dir(default = work_path,
                                                  caption = "Select functions directory")
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
        tmpnew[dim(tmpnew)[1], 3] <- choose.dir(caption = "Select java folder")
        Sys.setenv(JAVA_HOME = as.character(tmpnew[dim(tmpnew)[1], 3]))
        # Connexion with Access database ----
        cat("Do you want to establish a connexion with an Access database ?", "\n")
        cat("(yes, no)", "\n")
        access_answer <- readLines(n = 1)
        while (!(access_answer %in% c("yes", "no"))) {
          cat("Be careful! Your answer is not correct", "\n")
          cat("Do you want to establish a connexion with an Access database ?", "\n")
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
        cat("Do you want to establish a connexion with the T3+ database ?", "\n")
        cat("(yes, no)", "\n")
        t3plus_answer <- readLines(n = 1)
        while (!(t3plus_answer %in% c("yes", "no"))) {
          cat("Be careful! Your answer is not correct", "\n")
          cat("Do you want to establish a connexion with the T3+ database ?", "\n")
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
        cat("Do you want to establish a connexion with the observe database ?", "\n")
        cat("(yes, no)", "\n")
        observe_answer <- readLines(n = 1)
        while (!(observe_answer %in% c("yes", "no"))) {
          cat("Be careful! Your answer is not correct", "\n")
          cat("Do you want to establish a connexion with the observe database ?", "\n")
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
        cat("Do you want to establish a connexion with the balbaya database ?", "\n")
        cat("(yes, no)", "\n")
        balbaya_answer <- readLines(n = 1)
        while (!(balbaya_answer %in% c("yes", "no"))) {
          cat("Be careful! Your answer is not correct", "\n")
          cat("Do you want to establish a connexion with the balbaya database ?", "\n")
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
      }
      # Extract configuration file informations ----
      cat("Do you want to export the configuration file ?", "\n")
      cat("Be careful! Existing file in the folder will delete.", "\n")
      cat("(yes, no)", "\n")
      final_answer <- readLines(n = 1)
      while (!(final_answer %in% c("yes", "no"))) {
        cat("Be careful! The answer is not correct", "\n")
        cat("Do you want to export the configuration file ?", "\n")
        cat("Be careful! Existing file in the folder will delete.", "\n")
        cat("(yes, no)", "\n")
        final_answer <- readLines(n = 1)
      }
      if (final_answer == "yes") {
        write.table(tmpnew,
                    file = paste(choose.dir(default = work_path,
                                            caption = "Select configuration file folder"),
                                 "\\ConfigFile.csv",
                                 sep = ""),
                    sep = ";",
                    row.names = FALSE,
                    col.names = TRUE,
                    append = FALSE)
      }
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
      return(configfile)
    }
  } else {
    # Function error ----
    cat("You have to specify in function argument what type of configuration you need", "\n")
    cat("'base' for basic data manipulation without database communication", "\n")
    cat("'DB' for data manipulation with database communication", "\n")
    stop("Please specify correct argument")
  }
}
