#' @name latitude_longitude_cwp_manipulation
#' @title Conversion function for latitude, longitude and cwp item
#' @description Conversion function for latitude, longitude and cwp item. For more information on the cwp format check the online document on https://www.fao.org/cwp-on-fishery-statistics/handbook/general-concepts/main-water-areas/en/#c737533.
#' @param manipulation_process {\link[base]{character}} expected. A string matching one of the accepted processes. Available processes are "cwp_to_latitude_longitude" or "latitude_longitude_to_cwp".
#' @param data_longitude {\link[base]{character}} expected. By default NULL. Mandatory argument for "latitude_longitude_to_cwp" process. A string vector with longitude value(s). Formats allowed is decimal degree (with point separator) or in degree minute seconde format (xxdxx'xx''). If values are duplicated, the function simplify the process automatically (no need to remove them before).
#' @param data_latitude {\link[base]{character}} expected. By default NULL. Mandatory argument for "latitude_longitude_to_cwp" process. A string vector with latitude value(s). Formats allowed is decimal degree (with point separator) or in degree minute seconde format (xxdxx'xx''). If values are duplicated, the function simplify the process automatically (no need to remove them before).
#' @param data_cwp {\link[base]{character}} expected. By default NULL. Mandatory argument for "cwp_to_latitude_longitude" process. A string vector with cwp value(s).
#' @param referential_grid_file_path {\link[base]{character}} expected. File path of the referential grid shape. File with .Rdata extension file expected.
#' @param output_degree_format {\link[base]{character}} expected. By default NULL. Mandatory for "cwp_to_latitude_longitude" process. A string matching one of the accepted output degree format. Available formats are "degree_minute_seconde" and "decimal_degree".
#' @param output_degree_parameter {\link[base]{character}} expected. By default NULL. Mandatory for "cwp_to_latitude_longitude" process. You can choose between "centroid" or "corner" for display coordinates of the square according to the centroid or the corner of it.
#' @param input_degree_format {\link[base]{character}} expected. By default NULL. Mandatory for "latitude_longitude_to_cwp" process. A string matching one of the accepted output degree format. Available formats are "degree_minute_seconde" and "decimal_degree".
#' @param epsg_code {\link[base]{integer}} expected. By default 4326. Mandatory for "latitude_longitude_to_cwp" process. An integer (on 4 digits) of one EPSG spatial reference systems related to latitude and longitude coordinates provide. Check this web site for more informations: https://www.spatialreference.org. By default 4326.
#' @return Return a tibble.
#' @export
latitude_longitude_cwp_manipulation <- function(manipulation_process,
                                                data_longitude = NULL,
                                                data_latitude = NULL,
                                                data_cwp = NULL,
                                                referential_grid_file_path,
                                                output_degree_format = NULL,
                                                output_degree_parameter = NULL,
                                                input_degree_format = NULL,
                                                epsg_code = as.integer(4326)) {
  warning(format(x = Sys.time(),
                 "%Y-%m-%d %H:%M:%S"),
          " - Strange behaviour has been identified regarding some positions manipulation. Investigations are ongoing, use the function with careful. Take a look to the GitHub ticket for the last update. \n(https://github.com/OB7-IRD/furdeb/issues/11).")
  # 1 - Setup ----
  suppressMessages(sf::sf_use_s2(use_s2 = FALSE))
  # 2 - Local binding global variables ----
  geometry <- NULL
  X <- NULL
  Y <- NULL
  latitude <- NULL
  longitude <- NULL
  latitude_degree <- NULL
  latitude_minute <- NULL
  sign_latitude <- NULL
  latitude_seconde <- NULL
  longitude_degree <- NULL
  longitude_minute <- NULL
  sign_longitude <- NULL
  longitude_seconde <- NULL
  cwp <- NULL
  latitude_degree_minute_seconde <- NULL
  latitude_decimal_degree <- NULL
  longitude_degree_minute_seconde <- NULL
  longitude_decimal_degree <- NULL
  count <- NULL
  data_id <- NULL
  CWP_B <- NULL
  X_COORD <- NULL
  Y_COORD <- NULL
  CWP_CODE <- NULL
  # 3 - Global arguments verifications ----
  codama::r_type_checking(r_object = manipulation_process,
                          type = "character",
                          length = 1L,
                          allowed_value = c("latitude_longitude_to_cwp",
                                            "cwp_to_latitude_longitude"))
  codama::file_path_checking(file_path = referential_grid_file_path,
                             extension = c("Rdata",
                                           "RData"))
  reference_grid <- get(x = load(file = referential_grid_file_path))
  if (paste(class(reference_grid),
            collapse = " ") != "sf tbl_df tbl data.frame") {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Invalid fao shapefile, R object of class sf expected.")
  }
  codama::r_type_checking(r_object = names(reference_grid),
                          type = "character",
                          allowed_value = c("CWP_CODE",
                                            "GRIDTYPE",
                                            "QUADRANT",
                                            "X_Y_TYPE",
                                            "X_COORD",
                                            "Y_COORD",
                                            "CWP_A",
                                            "CWP_B",
                                            "CWP_C",
                                            "CWP_D",
                                            "CWP_E",
                                            "SURFACE",
                                            "ON_SEA",
                                            "ON_LAND",
                                            "ON_LAND_P",
                                            "ON_SEA_P",
                                            "geometry"))
  cwp_resolution <- unique(x = reference_grid$GRIDTYPE)
  if (length(x = cwp_resolution) != 1) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         "- Invalid \"reference_grid\" argument, multiple cwp resolutions inside the referential.")
  }
  if (length(x = unique(sapply(X = reference_grid$CWP_CODE,
                               FUN = nchar))) != 1) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         "- Invalid reference grid, CWP of multiple length inside the referential.")
  }
  # 4 - Process ----
  if (manipulation_process == "cwp_to_latitude_longitude") {
    codama::r_type_checking(r_object = data_cwp,
                            type = "character")
    cwp_length <- unique(sapply(X = data_cwp,
                                FUN = nchar))
    data_cwp <- dplyr::tibble(cwp = data_cwp)
    data_cwp_unique <- unique(x = data_cwp)
    if (cwp_length != unique(sapply(X = reference_grid$CWP_CODE,
                                    FUN = nchar))) {
      stop(format(x = Sys.time(),
                  "%Y-%m-%d %H:%M:%S"),
           "- Dispencrecy between \"input_cwp_format\" argument and referential grid.")
    }
    codama::r_type_checking(r_object = output_degree_format,
                            type = "character",
                            length = 1L,
                            allowed_value = c("decimal_degree",
                                              "degree_minute_seconde"))
    codama::r_type_checking(r_object = output_degree_parameter,
                            type = "character",
                            length = 1L,
                            allowed_value = c("centroid",
                                              "corner"))
    if (cwp_length %in% c(7)) {
      data_cwp_unique_final <- dplyr::inner_join(x = data_cwp_unique,
                                                 y = reference_grid[, c("geometry",
                                                                        "CWP_CODE")],
                                                 by = c("cwp" = "CWP_CODE"))
      if (nrow(x = data_cwp_unique_final) != 0) {
        if (output_degree_parameter == "centroid") {
          data_cwp_unique_final <- suppressWarnings(data_cwp_unique_final %>%
                                                      dplyr::rowwise() %>%
                                                      dplyr::mutate(latitude = as.numeric(x = data.frame(sf::st_coordinates(x = sf::st_centroid(x = geometry)))$Y),
                                                                    longitude = as.numeric(x = data.frame(sf::st_coordinates(x = sf::st_centroid(x = geometry)))$X)) %>%
                                                      dplyr::ungroup() %>%
                                                      dplyr::select(-geometry))
        } else if (output_degree_parameter == "corner") {
          data_cwp_unique_final <- suppressWarnings(data_cwp_unique_final %>%
                                                      dplyr::rowwise() %>%
                                                      dplyr::mutate(latitude = as.numeric(x = dplyr::distinct(data.frame(sf::st_coordinates(x = geometry)) %>%
                                                                                                                dplyr::mutate(X = round(x = abs(x = X),
                                                                                                                                        digits = 2),
                                                                                                                              Y = round(x = abs(x = Y),
                                                                                                                                        digits = 2)) %>%
                                                                                                                dplyr::filter(X == min(X)) %>%
                                                                                                                dplyr::filter(Y == min(Y))))[2],
                                                                    longitude = as.numeric(x = dplyr::distinct(data.frame(sf::st_coordinates(x = geometry)) %>%
                                                                                                                 dplyr::mutate(X = round(x = abs(x = X),
                                                                                                                                         digits = 2),
                                                                                                                               Y = round(x = abs(x = Y),
                                                                                                                                         digits = 2)) %>%
                                                                                                                 dplyr::filter(X == min(X)) %>%
                                                                                                                 dplyr::filter(Y == min(Y))))[1]) %>%
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
      stop(format(x = Sys.time(),
                  "%Y-%m-%d %H:%M:%S"),
           " - Error, function not developed yet for cwp of length ",
           cwp_length,
           ".")
    }
  } else if (manipulation_process == "latitude_longitude_to_cwp") {
    codama::r_type_checking(r_object = data_latitude,
                            type = "character")
    codama::r_type_checking(r_object = data_longitude,
                            type = "character")
    if (length(x = data_longitude) != length(x = data_latitude)) {
      stop(format(x = Sys.time(),
                  "%Y-%m-%d %H:%M:%S"),
           " - Invalid \"data_longitude\" and \"data_latitude\" arguments, same length argument expected.")
    }
    codama::r_type_checking(r_object = input_degree_format,
                            type = "character",
                            length = 1L,
                            allowed_value = c("degree_minute_seconde",
                                              "decimal_degree"))
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
    if (codama::r_type_checking(r_object = epsg_code,
                                type = "integer",
                                length = 1L,
                                output = "logical") != TRUE
        || nchar(x = epsg_code) != 4) {
      stop(format(x = Sys.time(),
                  "%Y-%m-%d %H:%M:%S"),
           " - Invalid \"epsg_code\" argument, class integer expected with one unique value inside.")
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
    join_longitude_latitude <- join_longitude_latitude[, c("data_id",
                                                           "CWP_CODE")]
    if (nrow(x = data_latitude_longitude_unique) != nrow(x = join_longitude_latitude)) {
      data_latitude_longitude_unique_duplicate <- dplyr::inner_join(x = data_latitude_longitude_unique,
                                                                    y = join_longitude_latitude,
                                                                    by = "data_id") %>%
        dplyr::group_by(longitude_decimal_degree,
                        latitude_decimal_degree) %>%
        dplyr::mutate(count = dplyr::n()) %>%
        dplyr::filter(count != 1)
      data_id_duplicate <- unique(data_latitude_longitude_unique_duplicate$data_id)
      for (current_data_id in data_id_duplicate) {
        current_sf_join_longitude_latitude <- dplyr::filter(.data = sf_join_longitude_latitude,
                                                            data_id == current_data_id)
        if (length(x = unique(x = current_sf_join_longitude_latitude$CWP_B)) != 1) {
          if (any(unique(x = current_sf_join_longitude_latitude$CWP_B) == 1)) {
            best_cwp <- dplyr::filter(.data = current_sf_join_longitude_latitude,
                                      CWP_B == 1)$CWP_CODE
          } else {
            best_cwp <- dplyr::filter(.data = current_sf_join_longitude_latitude,
                                      CWP_B %in% c(2, 4))$CWP_CODE
          }
        } else {
          if (unique(x = current_sf_join_longitude_latitude$CWP_B) == 1) {
            best_cwp <- dplyr::filter(.data = current_sf_join_longitude_latitude,
                                      X_COORD == min(unique(x = current_sf_join_longitude_latitude$X_COORD))
                                      & Y_COORD == min(unique(x = current_sf_join_longitude_latitude$Y_COORD)))$CWP_CODE
          } else if (unique(x = current_sf_join_longitude_latitude$CWP_B) == 2) {
            best_cwp <- dplyr::filter(.data = current_sf_join_longitude_latitude,
                                      X_COORD == min(unique(x = current_sf_join_longitude_latitude$X_COORD))
                                      & Y_COORD == max(unique(x = current_sf_join_longitude_latitude$Y_COORD)))$CWP_CODE
          } else if (unique(x = current_sf_join_longitude_latitude$CWP_B) == 3) {
            best_cwp <- dplyr::filter(.data = current_sf_join_longitude_latitude,
                                      X_COORD == max(unique(x = current_sf_join_longitude_latitude$X_COORD))
                                      & Y_COORD == max(unique(x = current_sf_join_longitude_latitude$Y_COORD)))$CWP_CODE
          } else if (unique(x = current_sf_join_longitude_latitude$CWP_B) == 4) {
            best_cwp <- dplyr::filter(.data = current_sf_join_longitude_latitude,
                                      X_COORD == max(unique(x = current_sf_join_longitude_latitude$X_COORD))
                                      & Y_COORD == min(unique(x = current_sf_join_longitude_latitude$Y_COORD)))$CWP_CODE
          }
        }
        join_longitude_latitude <- dplyr::filter(.data = join_longitude_latitude,
                                                 ! (data_id == current_data_id
                                                    & CWP_CODE != best_cwp))
      }
    }
    data_latitude_longitude_unique <- dplyr::inner_join(x = data_latitude_longitude_unique,
                                                        y = join_longitude_latitude,
                                                        by = "data_id")
    data_final <- dplyr::tibble(dplyr::left_join(x = data_latitude_longitude,
                                                 y = data_latitude_longitude_unique,
                                                 by = c("longitude_decimal_degree",
                                                        "latitude_decimal_degree"))) %>%
      dplyr::select(-data_id) %>%
      dplyr::rename("cwp" = "CWP_CODE")
  }
  return(data_final)
}
