#' @name configuration_file
#' @title Configuration file creation and management
#' @description Create or apply a configuration file (YAML extension).
#' @param path_file {\link[base]{character}} expected. Path to a existent configuration file. The file need a .yml extension. By default the parameter is NULL and the function will create a new configuration file.
#' @param silent {\link[base]{logical}} expected. Display or not information when you run an existing configuration file. By default FALSE.
#' @return Return a R list with several information configuration and, if you want, a YAML file.
#' @examples
#' # If you want to create a new configuration file
#' \dontrun{
#' configuration_file()}
#' # If you want to use an existing configuration file
#' \dontrun{
#' configuration_file(path_file = "path_of_your_own_configuration_file")}
#' @importFrom yaml write_yaml yaml.load_file
#' @importFrom utils choose.dir
#' @export
configuration_file <- function(path_file = NULL,
                               silent = FALSE) {
  if (is.null(x = path_file)) {
    configuration_file <- list()
    # setup working directory ----
    if (interactive()
        && .Platform$OS.type == "windows") {
      wd_path <- utils::choose.dir(default = getwd(),
                            caption = "Choose your working directory")
    } else {
      cat("Write or paste below your working directory\n")
      wd_path <- readLines(n = 1)
    }
    configuration_file <- append(configuration_file,
                                 list(wd_path = wd_path))
    # setup output directory ----
    cat("Do you need an output directory?\n(yes, no)\n")
    output_directory_answer <- readLines(n = 1)
    while (!(output_directory_answer %in% c("yes", "no"))) {
      cat("Answer not correct\n",
          "Do you need an output directory?\n",
          "(yes, no)\n")
      output_directory_answer <- readLines(n = 1)
    }
    if (output_directory_answer == "yes") {
      if (interactive()
          && .Platform$OS.type == "windows") {
        output_path <- utils::choose.dir(default = getwd(),
                              caption = "Choose your output directory")
      } else {
        cat("Write or paste below your output directory\n")
        output_path <- readLines(n = 1)
      }
      configuration_file <- append(configuration_file,
                                   list(output_path = output_path))
    }
    # setup java memory availability ----
    cat("Do you need to modifiy memory allowed for java? (by default 512mo)\n",
        "Warning, this step must be performed prior to loading any packages\n",
        "(yes, no)\n")
    java_memory_answer <- readLines(n = 1)
    while (!(java_memory_answer %in% c("yes", "no"))) {
      cat("Answer not correct\n",
          "Do you need to modifiy memory allowed for java? (by default 512mo)\n",
          "Warning, this step must be performed prior to loading any packages\n",
          "(yes, no)\n")
      java_memory_answer <- readLines(n = 1)
    }
    if (java_memory_answer == "yes") {
      cat("Enter new memory value allowed for java\n",
          "Numerical value expeceted following by unit, g (for gigabyte) or m (for megabyte)\n",
          "e.g. 6g or 1024m\n")
      java_memory_available <- readLines(n = 1)
      options(java.parameters = paste0("-Xmx",
                                       java_memory_available))
      configuration_file <- append(configuration_file,
                                   list(java_memory_available = java_memory_available))
    }
    # database(s) setup ----
    cat("Do you need to establish a connexion with a database?\n",
        "(yes, no)\n")
    database_answer <- readLines(n = 1)
    while (!(database_answer %in% c("yes", "no"))) {
      cat("Answer not correct\n",
          "Do you need to establish a connexion with a database?\n",
          "(yes, no)\n")
      database_answer <- readLines(n = 1)
    }
    if (database_answer == "yes") {
      databases_pre_configuration <- c("sardara_vmot5",
                                       "balbaya_vmot5",
                                       "t3_prod_vmot7",
                                       "t3_process_vmot7",
                                       "access_avdth")
      databases_configuration <- list()
      while (database_answer == "yes") {
        cat("Pre-configuration for the following databases are available (enter new if you want to make your own configuration)\n",
            paste(databases_pre_configuration,
                  collapse = " - "),
            "\n")
        database_configuration_answer <- readLines(n = 1)
        while (!(database_configuration_answer %in% c(databases_pre_configuration,
                                                      "new"))) {
          cat("Answer not correct\n",
              "Pre-configuration for the following databases are available (enter new if you want to make your own configuration)\n",
              paste(databases_pre_configuration,
                    collapse = " - "),
              "\n")
          database_configuration_answer <- readLines(n = 1)
        }
        if (database_configuration_answer %in% c("sardara_vmot5",
                                                 "balbaya_vmot5",
                                                 "t3_prod_vmot7",
                                                 "t3_process_vmot7",
                                                 "new")) {
          cat(paste0("Enter login database\n"))
          login <- readLines(n = 1)
          cat(paste0("Enter password database\n"))
          password <- readLines(n = 1)
          if (database_configuration_answer == "sardara_vmot5") {
            dbname <- "sardara"
            host <- "vmot5-proto.ird.fr"
            port <- 5432
          } else if (database_configuration_answer == "balbaya_vmot5") {
            dbname <- "balbaya"
            host <- "vmot5-proto.ird.fr"
            port <- 5432
          } else if (database_configuration_answer == "t3_prod_vmot7") {
            dbname <- "t3_prod"
            host <- "vmot7-proto.ird.fr"
            port <- 5432
          } else if (database_configuration_answer == "t3_process_vmot7") {
            dbname <- "t3_29_process"
            host <- "vmot7-proto.ird.fr"
            port <- 5432
          } else if (database_configuration_answer == "new") {
            cat("Enter database name\n")
            dbname <- readLines(n = 1)
            cat("Enter database host\n")
            host <- readLines(n = 1)
            cat("Enter database port\n")
            port <- readLines(n = 1)
          }
          current_databases_configuration <- list(list(login = login,
                                                       password = password,
                                                       dbname = dbname,
                                                       host = host,
                                                       port = port))
        } else if (database_configuration_answer == "access_avdth") {
          cat("Select location of your access database\n")
          database_path <- file.choose()
          current_databases_configuration <- list(list(database_path = database_path))
        }
        if (database_configuration_answer == "new") {
          cat("Enter database configuration name\n")
          database_configuration_name <- readLines(n = 1)
          if (length(databases_configuration) != 0) {
            while (database_configuration_name %in% names(databases_configuration)) {
              cat("Name already exist among database(s) configured\n",
                  "Enter a valide database configuration name\n")
              database_configuration_name <- readLines(n = 1)
            }
          }
        } else {
          if (length(databases_configuration) != 0) {
            check_database_configuration_name <- regexpr(pattern = database_configuration_answer,
                                                         text = names(databases_configuration)) == 1
            if (any(check_database_configuration_name)) {
              cat("Type of database configuration exist among database(s) configured\n",
                  "Do you want to use a specific name or use an auto-increment?\n",
                  "(specific, auto)\n")
              check_database_configuration_name_answer <- readLines(n = 1)
              while (!(check_database_configuration_name_answer %in% c("specific", "auto"))) {
                cat("Answer not correct\n",
                    "Type of database configuration exist among database(s) configured\n",
                    "Do you want to use a specific name or use an auto-increment?\n",
                    "(specific, auto)\n")
                check_database_configuration_name_answer <- readLines(n = 1)
              }
              if (check_database_configuration_name_answer == "specific") {
                cat("Enter database configuration name\n")
                database_configuration_name <- readLines(n = 1)
                while (database_configuration_name %in% names(databases_configuration)) {
                  cat("Name already exist among database(s) configured\n",
                      "Enter a valide database configuration name\n")
                  database_configuration_name <- readLines(n = 1)
                }
              } else {
                database_configuration_name <- paste(database_configuration_answer,
                                                     sum(check_database_configuration_name) + 1,
                                                     sep = "_")
              }
            } else {
              database_configuration_name <- database_configuration_answer
            }
          } else {
            database_configuration_name <- database_configuration_answer
          }
        }
        names(current_databases_configuration) <- database_configuration_name
        databases_configuration <- append(x = databases_configuration,
                                          values = current_databases_configuration)
        cat("Do you need to establish a connexion with another database?\n",
            "(yes, no)\n")
        database_answer <- readLines(n = 1)
        while (!(database_answer %in% c("yes", "no"))) {
          cat("Answer not correct\n",
              "Do you need to establish a connexion with another database?\n",
              "(yes, no)\n")
          database_answer <- readLines(n = 1)
        }
      }
      configuration_file <- append(configuration_file,
                                   list(databases_configuration = databases_configuration))
    }
    # configuration file export ----
    cat("R object \"configuration_file\" available in the global environmenent\n")
    cat("Do you want to export the object in a YAML format?\n",
        "(yes, no)\n")
    export_yaml_answer <- readLines(n = 1)
    while (!(export_yaml_answer %in% c("yes", "no"))) {
      cat("Answer not correct\n",
          "Do you want to export the object in a YAML format?\n",
          "(yes, no)\n")
      export_yaml_answer <- readLines(n = 1)
    }
    if (export_yaml_answer == "yes") {
      yaml::write_yaml(x = configuration_file,
                       file = paste0(format(Sys.time(), "%Y%m%d_%H%M%S_"),
                                     "configuration_file.yml"))
      cat("Configuration file exported in the current working directory\n")
    }
  } else {
    # import existing configuration file ----
    if (! is.logical(silent)) {
      stop("invalid \"silent\" argument")
    }
    tryCatch(
      expr = {
        configuration_file <- yaml::yaml.load_file(input = path_file)
        if (silent == FALSE) {
          cat("Configuration file correctly imported\n",
              "Check data inside before use\n")
        }
      },
      error = function(a) {
        message("Error - invalid \"path_file\" argument")
      },
      warning = function(b) {
        message("Warning - This is not normal, check data of the configuration file")
      }
    )
    if ("java_memory_available" %in% names(configuration_file)) {
      options(java.parameters = paste0("-Xmx",
                                       configuration_file[["java_memory_available"]]))
    }
  }
  return(configuration_file)
}
