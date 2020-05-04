#' @param data (data frame) A data frame with data inside.
#' @param manipulation_process (character) A string matching one of the accepted processes. Available processes are "cwp_to_lat_lon" or "lat_lon_to_cwp".
#' @param cwp_resolution (character) A string matching one of the accepted resolution values. Accepted resolutions values are "10min_x_10min", "20min_x_20min", "30min_x_30min", "30min_x_1deg", "1deg_x_1deg", "5deg_x_5deg", "10deg_x_10deg", "20deg_x_20deg" and "30deg_x_30deg".
#' @param path_extern_referential_grid (character) A string path to a RData file which contains a referential grid. Inside the RData, the R object has to be a "SpatialPolygonsDataFrame" class and have a name like "grid_cwp_cwp_resolution" (where "cwp_resolution" is the same value the "cwp_resolution" argument).
lat_lon_cwp_manipulation = function(manipulation_process = "cwp_to_lat_lon",
                                    data_longitude = NULL,
                                    data_latitude = NULL,
                                    data_cwp = NULL,
                                    input_degree_format = NULL,
                                    output_degree_format = NULL,
                                    cwp_resolution = "1deg_x_1deg",
                                    path_extern_referential_grid = NULL) {
  # cwp_resolution argument checking ----
  if (class(cwp_resolution) != "character"
      || length(cwp_resolution) != 1
      || ! cwp_resolution %in% c("10min_x_10min", "20min_x_20min", "30min_x_30min", "30min_x_1deg", "1deg_x_1deg", "5deg_x_5deg", "10deg_x_10deg", "20deg_x_20deg", "30deg_x_30deg")) {
    stop("Invalid \"cwp_resolution\" argument, class character with one value inside expected.\n",
         "Values accepted are \"10min_x_10min\", \"20min_x_20min\", \"30min_x_30min\", \"30min_x_1deg\", \"1deg_x_1deg\", \"5deg_x_5deg\", \"10deg_x_10deg\", \"20deg_x_20deg\" and \"30deg_x_30deg\".\n")
  }
  # import reference grid ----
  if (is.null(path_extern_referential_grid)) {
    if (file.exists(system.file("grids_cwp",
                                paste0("grid_cwp_",
                                       cwp_resolution,
                                       ".RData"),
                                package = "furdeb"))) {
      load(file = system.file("grids_cwp",
                              paste0("grid_cwp_",
                                     cwp_resolution,
                                     ".RData"),
                              package = "furdeb"),
           envir = tmp_envir <- new.env())
    } else {
      stop("No reference grid available for the resolution ",
           cwp_resolution,
           ", take a look to the function create_cwp_grid (?furdeb::create_cwp_grid).\n")
    }
  } else {
    if (class(path_extern_referential_grid) != "character"
        || length(path_extern_referential_grid) != 1
        || tools::file_ext(path_extern_referential_grid) != "RData") {
      stop("invalid \"path_extern_referential_grid\" argument, class character with one value inside and a \"RData\" extension expected.\n")
    } else {
      if (file.exists(path_extern_referential_grid)) {
        load(file = path_extern_referential_grid,
             envir = tmp_envir <- new.env())
      } else {
        stop("invalid \"path_extern_referential_grid\" argument, no file available at the location.\n")
      }
    }
  }
  if (exists(x = paste0("grid_cwp_",
                        cwp_resolution),
             envir = tmp_envir)) {

    reference_grid <- get(x = paste0("grid_cwp_",
                                     cwp_resolution),
                          envir = tmp_envir)
    if (class(reference_grid) != "SpatialPolygonsDataFrame") {
      stop("invalid referential grid, the R object has to be a \"SpatialPolygonsDataFrame\" class.\n")
    } else {
      reference_grid_data <- reference_grid@data
    }
  } else {
    stop("invalid referential grid, no R object named \"",
         paste0("grid_cwp_",
                cwp_resolution),
         "\" available in the R environment provided.\n")
  }
  # manipulation_process argument checking ----
  if (class(manipulation_process) != "character"
      || length(manipulation_process) != 1
      || ! manipulation_process %in% c("cwp_to_lat_lon", "lat_lon_to_cwp")) {
    stop("Invalid \"manipulation_process\" argument, class character with one value inside expected.\n",
         "Values accepted are \"cwp_to_lat_lon\" or \"lat_lon_to_cwp\".\n")
  }
  # processes begin here ----
  if (manipulation_process == "cwp_to_lat_lon") {
    if (is.null(data_cwp)
        || class(data_cwp) != "character") {
      stop("Invalid \"data_cwp\" argument, class character expected.\n")
    } else {
      current_data_cwp <- unique(data_cwp)
      if (unique(sapply(X = seq_len(length.out = length(current_data_cwp)),
                        FUN = function(a) {
                          nchar(current_data_cwp[a])
                        })) != 7) {
        stop("Invalid cwp in \"data_cwp\" argument, values with 7 numbers (in format character) expected.\n")
      } else {
        longitude_data <- as.character()
        latitude_data <- as.character()
        for (b in seq_len(length.out = length(current_data_cwp))) {
          if (! current_data_cwp[b] %in% reference_grid_data[, "cwp_code"]) {
            warning("cwp not present in the reference grid, data avoided (check for \"na\" in the output).\n")
            tmp_longitude_data <- NA
            tmp_latitude_data <- NA
          } else {
            tmp_longitude_data <- reference_grid_data[reference_grid_data$cwp_code == current_data_cwp[b], "longitude"]
            tmp_latitude_data <- reference_grid_data[reference_grid_data$cwp_code == current_data_cwp[b], "latitude"]
          }
          longitude_data <- append(longitude_data,
                                   tmp_longitude_data)
          latitude_data <- append(latitude_data,
                                  tmp_latitude_data)
        }
        if (is.null(output_degree_format)
            || class(output_degree_format) != "character"
            || length(output_degree_format) != 1
            || ! output_degree_format %in% c("degree_minute_seconde", "decimal_degree")) {
          stop("Invalid \"output_degree_format\" argument, class character with one value inside expected.\n",
               "Values accepted are \"degree_minute_seconde\" and \"decimal_degree\".\n")
        } else {
          if (output_degree_format == "degree_minute_seconde") {
            longitude_data <- sapply(X = seq_len(length.out = length(longitude_data)),
                                     FUN = function(b) {
                                       current_longitude <- as.numeric(longitude_data[b])
                                       sign_current_longitude <- ifelse(test = sign(x = current_longitude) %in% c(0, +1),
                                                                        yes = "",
                                                                        no = "-")
                                       current_longitude_degree <- abs(x = trunc(x = current_longitude))
                                       current_longitude_minute <- trunc((abs(x = current_longitude) - current_longitude_degree) * 60)
                                       current_longitude_seconde <- (abs(x = current_longitude) - current_longitude_degree - current_longitude_minute / 60) * 3600
                                       current_longitude_degree_minute_seconde <- paste0(sign_current_longitude,
                                                                                         current_longitude_degree,
                                                                                         "°",
                                                                                         current_longitude_minute,
                                                                                         "'",
                                                                                         current_longitude_seconde,
                                                                                         "\"")
                                     })
            latitude_data <- sapply(X = seq_len(length.out = length(latitude_data)),
                                    FUN = function(c) {
                                      current_latitude <- as.numeric(latitude_data[c])
                                      sign_current_latitude <- ifelse(test = sign(x = current_latitude) %in% c(0, +1),
                                                                      yes = "",
                                                                      no = "-")
                                      current_latitude_degree <- abs(x = trunc(x = current_latitude))
                                      current_latitude_minute <- trunc((abs(x = current_latitude) - current_latitude_degree) * 60)
                                      current_latitude_seconde <- (abs(x = current_latitude) - current_latitude_degree - current_latitude_minute / 60) * 3600
                                      current_latitude_degree_minute_seconde <- paste0(sign_current_latitude,
                                                                                       current_latitude_degree,
                                                                                       "°",
                                                                                       current_latitude_minute,
                                                                                       "'",
                                                                                       current_latitude_seconde,
                                                                                       "\"")
                                    })
            data_final <- data.frame("cwp" = current_data_cwp,
                                     "longitude_degree_minute_seconde" = longitude_data,
                                     "latitude_degree_minute_seconde" = latitude_data,
                                     stringsAsFactors = FALSE)
          } else if (output_degree_format == "decimal_degree") {
            data_final <- data.frame("cwp" = current_data_cwp,
                                     "longitude_decimal_degree" = longitude_data,
                                     "latitude_decimal_degree" = latitude_data,
                                     stringsAsFactors = FALSE)
          } else {
            stop("Invalid \"output_degree_format\" argument, function not developped for this parameter yet.\n",
                 "Values accepted are \"degree_minute_seconde\" and \"decimal_degree\".\n")
          }
        }
      }
    }
  } else if (manipulation_process == "lat_lon_to_cwp") {
    if (is.null(data_longitude)
        || class(data_longitude) != "character") {
      stop("Invalid \"data_longitude\" argument, class character expected.\n")
    }
    if (is.null(data_latitude)
        || class(data_latitude) != "character") {
      stop("Invalid \"data_latitude\" argument, class character expected.\n")
    }
    if (length(data_longitude) != length(data_latitude)) {
      stop("Invalid \"data_longitude\" and \"data_latitude\" arguments, same length argument expected.\n")
    }
    if (class(input_degree_format) != "character"
        || length(input_degree_format) != 1
        || ! input_degree_format %in% c("degree_minute_seconde", "decimal_degree")) {
      stop("Invalid \"input_degree_format\" argument, class character with one value inside expected.\n",
           "Values accepted are \"degree_minute_seconde\" and \"decimal_degree\".\n")
    }
    if (input_degree_format == "decimal_degree") {
      longitude_data <- data_longitude
      latitude_data <- data_latitude
    } else {
      longitude_data <- sapply(X = seq_len(length.out = length(data_longitude)),
                               FUN = function(d) {
                                 current_longitude <- data_longitude[d]
                                 sign_current_longitude <- ifelse(test = sign(x = as.integer(regmatches(current_longitude, regexpr("[^°]*",
                                                                                                                                   current_longitude)))) %in% c(0, +1),
                                                                  yes = "",
                                                                  no = "-")
                                 current_longitude_degree <- regmatches(current_longitude, regexpr("[^°]*",
                                                                                                   current_longitude))
                                 current_longitude_minute <- gsub(pattern = ".*°(.+)'.*", replacement = "\\1", x = current_longitude)
                                 current_longitude_seconde <- gsub(pattern = ".*'(.+)\".*", replacement = "\\1", x = current_longitude)
                                 current_longitude_decimal_degree <- abs(as.numeric(current_longitude_degree)) + as.numeric(current_longitude_minute) / 60 + as.numeric(current_longitude_seconde) / 3600
                                 current_longitude_decimal_degree <- paste0(sign_current_longitude,
                                                                            current_longitude_decimal_degree)
                               })
    }
    current_longitude_latitude <- unique(data.frame(longitude_decimal_degree = as.numeric(longitude_data),
                                                    latitude_decimal_degree = as.numeric(latitude_data)))
    sp::coordinates(current_longitude_latitude) <- c("longitude_decimal_degree",
                                                     "latitude_decimal_degree")
    llp4s <- "+init=epsg:4326"
    llcrs <- sp::CRS(llp4s)
    sp::proj4string(current_longitude_latitude) <- llcrs
    overcurrent_longitude_latitude <- sp::over(x = current_longitude_latitude,
                                               y = reference_grid)
    current_longitude_latitude <- cbind(current_longitude_latitude@coords,
                                        overcurrent_longitude_latitude)
    data_final <- data.frame("cwp" = current_longitude_latitude$cwp_code,
                             "longitude_decimal_degree" = as.character(current_longitude_latitude$longitude_decimal_degree),
                             "latitude_decimal_degree" = as.character(current_longitude_latitude$latitude_decimal_degree),
                             stringsAsFactors = FALSE)

  }
  return(data_final)
}
