#' @name lat_lon_cwp_manipulation
#' @title Conversion function for latitude, longitude and cwp item
#' @description Conversion function for latitude, longitude and cwp item.
#' @param manipulation_process {\link[base]{character}} expected. A string matching one of the accepted processes. Available processes are "cwp_to_lat_lon" or "lat_lon_to_cwp".
#' @param data_longitude {\link[base]{character}} expected. Mandatory argument for "lat_lon_to_cwp" process. A string vector with longitude value(s). Formats allowed is decimal degree (with point separator) or in degree minute seconde format (xxdxx'xx'').
#' @param data_latitude {\link[base]{character}} expected. Mandatory argument for "lat_lon_to_cwp" process. A string vector with latitude value(s). Formats allowed is decimal degree (with point separator) or in degree minute seconde format (xxdxx'xx'').
#' @param data_cwp {\link[base]{character}} expected. Mandatory argument for "cwp_to_lat_lon" process. A string vector with cwp value(s). Mandatory argument for "cwp_to_lat_lon" process.
#' @param referential_grid_file_path {\link[base]{character}} expected. File path of the referential grid shape. File with .RData extension file expected.
#' @param input_cwp_format {\link[base]{character}} expected. Mandatory argument for "cwp_to_lat_lon" process. Cwp construction process related to the "centroid" or the "corner" of the square.
#' @param output_degree_cwp_parameter {\link[base]{character}} expected. Mandatory for "cwp_to_lat_lon" process. A string matching one of the accepted cwp parameter. Available parameters are "centroid" (for output coordinates in relation to cwp centroid) and "corner" (for output coordinates in relation to cwp corner).
#' @param output_degree_format {\link[base]{character}} expected. Mandatory for "cwp_to_lat_lon" process. A string matching one of the accepted output degree format. Available formats are "degree_minute_seconde" and "decimal_degree".
#' @param input_degree_format {\link[base]{character}} expected. Mandatory for "lat_lon_to_cwp" process. A string matching one of the accepted output degree format. Available formats are "degree_minute_seconde" and "decimal_degree".
#' @param epsg_code {\link[base]{integer}} expected. Mandatory for "lat_lon_to_cwp" process. An integer (on 4 digits) of one EPSG spatial reference systems related to latitude and longitude coordinates provide. Check this web site for more informations: https://www.spatialreference.org. By default 4326.
#' @param output_cwp_format {\link[base]{character}} expected. Output format of cwp. So far, you can choose between theses formats: "centroid_7" (based on the square's centroid with a cwp on 7 characters) or "corner_7" (based on the square's corner with a cwp on 7 characters)
#' @return Return a data.frame.
#' @importFrom dplyr last tibble inner_join rowwise mutate ungroup select rename case_when left_join
#' @importFrom sf st_coordinates st_centroid st_as_sf st_join st_intersects st_drop_geometry
#' @importFrom stringr str_extract
#' @export
lat_lon_cwp_manipulation <- function(manipulation_process,
                                     data_longitude = NULL,
                                     data_latitude = NULL,
                                     data_cwp = NULL,
                                     referential_grid_file_path,
                                     input_cwp_format = NULL,
                                     output_degree_cwp_parameter = NULL,
                                     output_degree_format = NULL,
                                     input_degree_format = NULL,
                                     epsg_code = as.integer(4326),
                                     output_cwp_format = NULL) {
  # setup
  suppressMessages(sf::sf_use_s2(use_s2 = FALSE))
  # local binding global variables ----
  geometry <- X <- Y <- latitude <- longitude <- latitude_degree <- latitude_minute <- sign_latitude <- latitude_seconde <- longitude_degree <- longitude_minute <- sign_longitude <- longitude_seconde <- cwp <- latitude_degree_minute_seconde <- longitude_degree_minute_seconde <- data_id <- NULL
  # global arguments verifications ----
  if (codama::r_type_checking(r_object = manipulation_process,
                              type = "character",
                              length = 1L,
                              allowed_values = c("lat_lon_to_cwp",
                                                 "cwp_to_lat_lon"),
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = manipulation_process,
                                   type = "character",
                                   length = 1L,
                                   allowed_values = c("lat_lon_to_cwp",
                                                      "cwp_to_lat_lon"),
                                   output = "message"))
  }
  if (codama::file_path_checking(file_path = referential_grid_file_path,
                                 extension = c("RData"),
                                 output = "logical") != TRUE) {
    return(codama::file_path_checking(file_path = referential_grid_file_path,
                                      extension = c("RData"),
                                      output = "message"))
  }
  # import reference grid ----
  reference_grid <- get(x = load(file = referential_grid_file_path))
  if (paste(class(reference_grid),
            collapse = " ") != "sf tbl_df tbl data.frame") {
    stop("invalid fao shapefile, R object of class sf expected.\n")
  }
  cwp_resolution <- unique(x = reference_grid$GRIDTYPE)
  if (length(x = cwp_resolution) != 1) {
    stop("invalid \"reference_grid\" argument, multiple cwp resolutions inside the referential.\n")
  }
  # processes begin here ----
  if (manipulation_process == "cwp_to_lat_lon") {
    if (is.null(data_cwp)
        || class(data_cwp) != "character"
        || length(x = unique(sapply(X = data_cwp,
                                    FUN = nchar))) != 1) {
      stop("Invalid \"data_cwp\" argument.\n")
    } else {
      cwp_length <- unique(sapply(X = data_cwp,
                                  FUN = nchar))
      data_cwp <- dplyr::tibble(cwp = data_cwp)
      data_cwp_unique <- unique(x = data_cwp)
      if (codama::r_type_checking(r_object = input_cwp_format,
                                  type = "character",
                                  length = 1L,
                                  allowed_values = c("centroid",
                                                     "corner"),
                                  output = "logical") != TRUE) {
        return(codama::r_type_checking(r_object = input_cwp_format,
                                       type = "character",
                                       length = 1L,
                                       allowed_values = c("centroid",
                                                          "corner"),
                                       output = "message"))
      }
      if (codama::r_type_checking(r_object = output_degree_cwp_parameter,
                                  type = "character",
                                  length = 1L,
                                  allowed_values = c("centroid",
                                                     "corner"),
                                  output = "logical") != TRUE) {
        return(codama::r_type_checking(r_object = output_degree_cwp_parameter,
                                       type = "character",
                                       length = 1L,
                                       allowed_values = c("centroid",
                                                          "corner"),
                                       output = "message"))
      }
      if (codama::r_type_checking(r_object = output_degree_format,
                                  type = "character",
                                  length = 1L,
                                  allowed_values = c("decimal_degree",
                                                     "degree_minute_seconde"),
                                  output = "logical") != TRUE) {
        return(codama::r_type_checking(r_object = output_degree_format,
                                       type = "character",
                                       length = 1L,
                                       allowed_values = c("decimal_degree",
                                                          "degree_minute_seconde"),
                                       output = "message"))
      }
      if (cwp_length %in% c(7)) {
        if (input_cwp_format == "centroid") {
          data_cwp_unique_final <- dplyr::inner_join(x = data_cwp_unique,
                                                     y = reference_grid[, c("geometry",
                                                                            "CWP_CODE")],
                                                     by = c("cwp" = "CWP_CODE"))
        } else if (input_cwp_format == "corner") {
          data_cwp_unique_final <- dplyr::inner_join(x = data_cwp_unique,
                                                     y = reference_grid[, c("geometry",
                                                                            "CWP_CODE")],
                                                     by = c("cwp" = "CWP_CODE"))
        }
        if (nrow(x = data_cwp_unique_final) != 0) {
          if (output_degree_cwp_parameter == "centroid") {
            data_cwp_unique_final <- suppressWarnings(data_cwp_unique_final %>%
                                                        dplyr::rowwise() %>%
                                                        dplyr::mutate(latitude = sprintf(fmt = "%02.f",
                                                                                         as.integer(x = data.frame(sf::st_coordinates(x = sf::st_centroid(x = geometry)))$Y)),
                                                                      longitude = sprintf(fmt = "%03.f",
                                                                                          as.integer(x = data.frame(sf::st_coordinates(x = sf::st_centroid(x = geometry)))$X))) %>%
                                                        dplyr::ungroup() %>%
                                                        dplyr::select(-geometry))
          } else if (output_degree_cwp_parameter == "corner") {
            data_cwp_unique_final <- suppressWarnings(data_cwp_unique_final %>%
                                                        dplyr::rowwise() %>%
                                                        dplyr::mutate(latitude = sprintf(fmt = "%02.f",
                                                                                         as.integer(x = data.frame(sf::st_coordinates(x = geometry)) %>%
                                                                                                      mutate(X = round(x = abs(x = X),
                                                                                                                       digits = 2),
                                                                                                             Y = round(x = abs(x = Y),
                                                                                                                       digits = 2)) %>%
                                                                                                      dplyr::filter(X == min(X)) %>%
                                                                                                      dplyr::filter(Y == min(Y)))$Y),
                                                                      longitude = sprintf(fmt = "%03.f",
                                                                                          as.integer(x = data.frame(sf::st_coordinates(x = geometry)) %>%
                                                                                                       mutate(X = round(x = abs(x = X),
                                                                                                                        digits = 2),
                                                                                                              Y = round(x = abs(x = Y),
                                                                                                                        digits = 2)) %>%
                                                                                                       dplyr::filter(X == min(X)) %>%
                                                                                                       dplyr::filter(Y == min(Y)))$X)) %>%
                                                        dplyr::ungroup() %>%
                                                        dplyr::select(-geometry))
          }
          if (output_degree_format == "decimal_degree") {
            data_cwp_unique_final <- dplyr::rename(.data = data_cwp_unique_final,
                                                   latitude_decimal_degree = latitude,
                                                   longitude_decimal_degree = longitude)
          } else if (output_degree_format == "degree_minute_seconde") {
            data_cwp_unique_final <- data_cwp_unique_final %>%
              dplyr::rowwise() %>%
              dplyr::mutate(latitude = as.numeric(x = latitude),
                            longitude = as.numeric(x = longitude),
                            sign_latitude = dplyr::case_when(
                              sign(x = latitude) %in% c(0, +1) ~ "",
                              TRUE ~ "-"
                            ),
                            latitude_degree = abs(x = trunc(x = latitude)),
                            latitude_minute = trunc((abs(x = latitude) - latitude_degree) * 60),
                            latitude_seconde = (abs(x = latitude) - latitude_degree - latitude_minute / 60) * 3600,
                            latitude_degree_minute_seconde = paste0(sign_latitude,
                                                                    latitude_degree,
                                                                    "d",
                                                                    latitude_minute,
                                                                    "'",
                                                                    round(x = latitude_seconde),
                                                                    "''"),
                            sign_longitude = dplyr::case_when(
                              sign(x = longitude) %in% c(0, +1) ~ "",
                              TRUE ~ "-"
                            ),
                            longitude_degree = abs(x = trunc(x = longitude)),
                            longitude_minute = trunc((abs(x = longitude) - longitude_degree) * 60),
                            longitude_seconde = (abs(x = longitude) - longitude_degree - longitude_minute / 60) * 3600,
                            longitude_degree_minute_seconde = paste0(sign_longitude,
                                                                     longitude_degree,
                                                                     "d",
                                                                     longitude_minute,
                                                                     "'",
                                                                     round(x = longitude_seconde),
                                                                     "''")) %>%
              dplyr::ungroup()%>%
              dplyr::select(cwp,
                            latitude_degree_minute_seconde,
                            longitude_degree_minute_seconde)
          }
        } else {
          data_cwp_unique_final <- dplyr::mutate(.data = data_cwp_unique_final,
                                                 latitude = NA,
                                                 longitude = NA) %>%
            dplyr::select(-geometry)
        }
        data_final <- dplyr::left_join(x = data_cwp,
                                       y = data_cwp_unique_final,
                                       by = "cwp")
      } else {
        stop("function not developed yet for cwp of length ",
             cwp_length,
             ".\n")
      }
    }
  } else if (manipulation_process == "lat_lon_to_cwp") {
    if (is.null(x = data_longitude)
        || class(data_longitude) != "character") {
      stop("invalid \"data_longitude\" argument, class character expected.\n")
    }
    if (is.null(data_latitude)
        || class(data_latitude) != "character") {
      stop("invalid \"data_latitude\" argument, class character expected.\n")
    }
    if (length(data_longitude) != length(data_latitude)) {
      stop("invalid \"data_longitude\" and \"data_latitude\" arguments, same length argument expected.\n")
    }
    if (codama::r_type_checking(r_object = input_degree_format,
                                type = "character",
                                length = 1L,
                                allowed_values = c("degree_minute_seconde",
                                                   "decimal_degree"),
                                output = "logical") != TRUE) {
      return(codama::r_type_checking(r_object = input_degree_format,
                                     type = "character",
                                     length = 1L,
                                     allowed_values = c("degree_minute_seconde",
                                                        "decimal_degree"),
                                     output = "message"))
    }
    if (codama::r_type_checking(r_object = output_cwp_format,
                                type = "character",
                                length = 1L,
                                allowed_values = c("centroid_7",
                                                   "corner_7"),
                                output = "logical") != TRUE) {
      return(codama::r_type_checking(r_object = output_cwp_format,
                                     type = "character",
                                     length = 1L,
                                     allowed_values = c("centroid_7",
                                                        "corner_7"),
                                     output = "message"))
    }
    if (input_degree_format == "decimal_degree") {
      longitude_data <- data_longitude
      latitude_data <- data_latitude
    } else {
      longitude_data <- sapply(X = seq_len(length.out = length(data_longitude)),
                               FUN = function(d) {
                                 current_longitude <- data_longitude[d]
                                 sign_current_longitude <- ifelse(test = sign(x = as.integer(regmatches(current_longitude,
                                                                                                        regexpr("[^d]*",
                                                                                                                current_longitude)))) %in% c(0, +1),
                                                                  yes = "",
                                                                  no = "-")
                                 current_longitude_degree <- regmatches(current_longitude,
                                                                        regexpr("[^d]*",
                                                                                current_longitude))
                                 current_longitude_minute <- stringr::str_extract(string = current_longitude,
                                                                                  pattern = "(?<=d)[[:digit:]]{1,2}")
                                 current_longitude_seconde <- gsub(pattern = ".*'(.+)''.*",
                                                                   replacement = "\\1",
                                                                   x = current_longitude)
                                 current_longitude_decimal_degree <- abs(as.numeric(current_longitude_degree)) + as.numeric(current_longitude_minute) / 60 + as.numeric(current_longitude_seconde) / 3600
                                 current_longitude_decimal_degree <- paste0(sign_current_longitude,
                                                                            current_longitude_decimal_degree)
                               })
      latitude_data <- sapply(X = seq_len(length.out = length(data_latitude)),
                              FUN = function(e) {
                                current_latitude <- data_latitude[e]
                                sign_current_latitude <- ifelse(test = sign(x = as.integer(regmatches(current_latitude,
                                                                                                      regexpr("[^d]*",
                                                                                                              current_latitude)))) %in% c(0, +1),
                                                                yes = "",
                                                                no = "-")
                                current_latitude_degree <- regmatches(current_latitude,
                                                                      regexpr("[^d]*",
                                                                              current_latitude))
                                current_latitude_minute <- stringr::str_extract(string = current_latitude,
                                                                                pattern = "(?<=d)[[:digit:]]{1,2}")
                                current_latitude_seconde <- gsub(pattern = ".*'(.+)\".*",
                                                                 replacement = "\\1",
                                                                 x = current_latitude)
                                current_latitude_decimal_degree <- abs(as.numeric(current_latitude_degree)) + as.numeric(current_latitude_minute) / 60 + as.numeric(current_latitude_seconde) / 3600
                                current_latitude_decimal_degree <- paste0(sign_current_latitude,
                                                                          current_latitude_decimal_degree)
                              })
    }
    data_latitude_longitude <- dplyr::tibble(longitude_decimal_degree = as.numeric(longitude_data),
                                             latitude_decimal_degree = as.numeric(latitude_data))
    data_latitude_longitude_unique <- unique(x = data_latitude_longitude)
    data_latitude_longitude_unique$data_id <- seq_len(length.out = nrow(data_latitude_longitude_unique))
    if (class(x = epsg_code) != "integer"
        || length(x = epsg_code) != 1
        || nchar(x = epsg_code) != 4) {
      stop("invalid \"epsg_code\" argument, class integer expected with one unique value inside.\n")
    }
    longitude_latitude_sf <- sf::st_as_sf(x = data_latitude_longitude_unique,
                                          coords = c("longitude_decimal_degree",
                                                     "latitude_decimal_degree"),
                                          crs = epsg_code)
    sf_join_longitude_latitude <- sf::st_join(x = longitude_latitude_sf,
                                              y = reference_grid,
                                              join = sf::st_intersects,
                                              left = TRUE)
    join_longitude_latitude <- sf::st_drop_geometry(sf_join_longitude_latitude)
    if (output_cwp_format == "centroid_7") {
      join_longitude_latitude <- join_longitude_latitude[, c("data_id",
                                                             "CWP_CODE")]
    } else if (output_cwp_format == "corner_7") {
      join_longitude_latitude <- join_longitude_latitude[, c("data_id",
                                                             "CWP_CODE")]
    }
    data_latitude_longitude_unique <- dplyr::inner_join(x = data_latitude_longitude_unique,
                                                        y = join_longitude_latitude,
                                                        by = "data_id")
    data_final <- dplyr::tibble(dplyr::left_join(x = data_latitude_longitude,
                                                 y = data_latitude_longitude_unique,
                                                 by = c("longitude_decimal_degree",
                                                        "latitude_decimal_degree"))) %>%
      dplyr::select(-data_id)
  }
  return(data_final)
}
