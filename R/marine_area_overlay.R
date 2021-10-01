#' @name marine_area_overlay
#' @title Consistent spatial marine area overlay (related to fao or eez area)
#' @description Consistent spatial marine area overlay (related to fao or eez area) for points, grids and polygons.
#' @param data {\link[base]{data.frame}} expected. R dataframe, with at least two columns with longitude and latitude values. Be careful! Your longitude and latitude data have to be in the WGS84 projection and coordinates in decimal degrees.
#' @param overlay_expected {\link[base]{character}} expected. Type of overlay output. You can choose between "fao_area", "eez_area" or "fao_eez_area".
#' @param longitude_name {\link[base]{character}} expected. Longitude column name in your data.
#' @param latitude_name {\link[base]{character}} expected. Latitude column name in your data.
#' @param fao_area_file_path {\link[base]{character}} expected. File path of the fao area shape. You can provide a .shp or a .RData file.
#' @param fao_overlay_level {\link[base]{character}} expected. Level of fao accuarcy that you want for classified your data. By default, major fao fishing area are selected. Check the section details below.
#' @param auto_selection_fao {\link[base]{logical}} expected. Add a new column in the output with the most detailed overlay level available.
#' @param eez_area_file_path {\link[base]{character}} expected. File path of the eez area shape. You can provide a .shp or a .RData file.
#' @param for_fdi_use {\link[base]{logical}} expected. Add a new column in the output with the FDI variable "eez_indicator".
#' @param silent {\link[base]{logical}} expected. Display or not warning information regarding projection of your spatial coordinates. By default FALSE.
#' @return The function return your input data frame with one or several columns (regarding arguments' specification) which contains area classification.
#' @details
#' For the argument "fao_overlay_level", you can choose between 5 modalities (descending size classification):
#' \itemize{
#'  \item{ocean: }{ocean area}
#'  \item{major: }{major fao fishing area}
#'  \item{subarea: }{sub fao fishing area}
#'  \item{division: }{division fao fishing area}
#'  \item{subdivision: }{sub-division fao fishing area}
#'  \item{subunit: }{sub-unit fao fishing area}
#' }
#' Specificity for fao fishing area parameters: all the items above your specification (thus contain it at higher levels) will be added in the output. For example, if you select "subarea", you will also have the information about the major area concerning.
#' If you want more information visit http://www.fao.org/fishery/area/search/en
#' @export
#' @importFrom dplyr last inner_join rename
#' @importFrom sf read_sf st_is_valid st_make_valid st_as_sf st_join st_intersects st_drop_geometry
marine_area_overlay <- function(data,
                                overlay_expected,
                                longitude_name,
                                latitude_name,
                                fao_area_file_path,
                                fao_overlay_level = "major",
                                auto_selection_fao = FALSE,
                                eez_area_file_path,
                                for_fdi_use = NULL,
                                silent = FALSE) {
  # arguments verifications ----
  if (missing(data)
      && ! is.data.frame(data)) {
    stop("invalid \"data\" argument\n")
  }
  overlay_expected <- match.arg(arg = overlay_expected,
                                choices = c("fao_area",
                                            "eez_area",
                                            "fao_eez_area"))
  if (missing(longitude_name)
      & ! is.character(longitude_name)) {
    stop("invalid \"longitude_name\" argument\n")
  }
  if (missing(latitude_name)
      && ! is.character(latitude_name)) {
    stop("invalid \"latitude_name\" argument\n")
  }
  if (overlay_expected %in% c("fao_area",
                              "fao_eez_area")) {
    if (missing(fao_area_file_path)
        && ! is.character(fao_area_file_path)) {
      stop("invalid \"fao_area_file_path\" argument\n")
    }
    fao_overlay_level <- match.arg(arg = fao_overlay_level,
                                   choices = c("ocean",
                                               "major",
                                               "subarea",
                                               "division",
                                               "subdivision",
                                               "subunit"))
    if (! is.logical(auto_selection_fao)) {
      stop("invalid \"auto_selection_fao\" argument")
    }
  } else if (overlay_expected == "eez_area") {
    if (missing(eez_area_file_path)
        && ! is.character(eez_area_file_path)) {
      stop("invalid \"eez_area_file_path\" argument\n")
    }
  }
  if (! is.null(for_fdi_use)
      && ! is.logical(for_fdi_use)) {
    stop("invalid \"for_fdi_use\" argument")
  }
  if (! is.logical(silent)) {
    stop("invalid \"silent\" argument")
  }
  if (silent == FALSE) {
    cat("Be careful!\n",
        "You're spatial coordinates have to be in WGS84 projection\n",
        "Be patient! The function could be long\n")
  }
  # shapes imports ----
  # fao area
  if (overlay_expected %in% c("fao_area",
                              "fao_eez_area")
      | (overlay_expected == "eez_area"
         & (! is.null(for_fdi_use)
            && for_fdi_use == TRUE))) {
    fao_area_file_path_extension <- dplyr::last(x = unlist(strsplit(fao_area_file_path,
                                                                    '[.]')))
    if (fao_area_file_path_extension == "shp") {
      fao_area <- sf::read_sf(fao_area_file_path)
      if (! all(sf::st_is_valid(fao_area))) {
        fao_area <- sf::st_make_valid(fao_area)
      }
    } else if (fao_area_file_path_extension == "RData") {
      fao_area <- get(x = load(file = fao_area_file_path))
      if (paste(class(fao_area),
                collapse = " ") != "sf tbl_df tbl data.frame") {
        stop("invalid fao shapefile, R object of class sf\n")
      }
    } else {
      stop("invalid \"fao_area_file_path\" argument, shp or RData extensions expected\n")
    }
  }
  # eez area
  if (overlay_expected %in% c("eez_area",
                              "fao_eez_area")) {
    eez_area_file_path_extension <- dplyr::last(x = unlist(strsplit(eez_area_file_path,
                                                                    '[.]')))
    if (eez_area_file_path_extension == "shp") {
      eez_area <- sf::read_sf(eez_area_file_path)
      if (! all(sf::st_is_valid(eez_area))) {
        eez_area <- sf::st_make_valid(eez_area)
      }
    } else if (eez_area_file_path_extension == "RData") {
      eez_area <- get(x = load(file = eez_area_file_path))
      if (paste(class(eez_area),
                collapse = " ") != "sf tbl_df tbl data.frame") {
        stop("invalid eez shapefile, R object of class sf\n")
      }
    } else {
      stop("invalid \"eez_area_file_path\" argument, shp or RData extensions expected\n")
    }
  }
  # data design ----
  data_unique <- unique(data)
  data_unique$data_id <- seq_len(length.out = nrow(data_unique))
  data_sf <- sf::st_as_sf(x = unique(data_unique),
                          coords = c(longitude_name,
                                     latitude_name),
                          crs = 4326)
  if (overlay_expected %in% c("fao_area",
                              "fao_eez_area")) {
    # fao spatial overlay ----
    if (fao_overlay_level == "ocean") {
      accuracy <- "OCEAN"
      names(accuracy) <- "MAJOR"
    } else if (fao_overlay_level == "major") {
      accuracy <- "F_AREA"
      names(accuracy) <- "MAJOR"
    } else if (fao_overlay_level == "subarea") {
      accuracy <- c("F_AREA",
                    "F_SUBAREA")
      names(accuracy) <- c("MAJOR",
                           "SUBAREA")
    } else if (fao_overlay_level == "division") {
      accuracy <- c("F_AREA",
                    "F_SUBAREA",
                    "F_DIVISION")
      names(accuracy) <- c("MAJOR",
                           "SUBAREA",
                           "DIVISION")
    } else if (fao_overlay_level == "subdivision") {
      accuracy <- c("F_AREA",
                    "F_SUBAREA",
                    "F_DIVISION",
                    "F_SUBDIVIS")
      names(accuracy) <- c("MAJOR",
                           "SUBAREA",
                           "DIVISION",
                           "SUBDIVISION")
    } else if (fao_overlay_level == "subunit") {
      accuracy <- c("F_AREA",
                    "F_SUBAREA",
                    "F_DIVISION",
                    "F_SUBDIVIS",
                    "F_SUBUNIT")
      names(accuracy) <- c("MAJOR",
                           "SUBAREA",
                           "DIVISION",
                           "SUBDIVISION",
                           "SUBUNIT")
    }
    for (level in seq_len(length.out = length(accuracy))) {
      fao_area_sub <- fao_area[fao_area$F_LEVEL == names(accuracy)[level], ]

      sf_join_data_fao_area_sub <- sf::st_join(x = data_sf,
                                               y = fao_area_sub,
                                               join = sf::st_intersects,
                                               left = TRUE)
      join_data_fao_area_sub <- sf::st_drop_geometry(sf_join_data_fao_area_sub)
      join_data_fao_area_sub <- join_data_fao_area_sub[, c("data_id",
                                                           accuracy[level])]

      data_unique <- dplyr::inner_join(x = data_unique,
                                       y = join_data_fao_area_sub,
                                       by = "data_id")

    }
    names(data_unique)[4:ncol(data_unique)] <- sapply(tolower(x = names(accuracy)),
                                                      paste0,
                                                      "_fao")
    # best fao area selection ----
    if (auto_selection_fao == TRUE) {
      accuracy_position_ori = ncol(data_unique)
      for (data_unique_id in seq_len(length.out = nrow(data_unique))) {
        accuracy_position <- accuracy_position_ori
        while(accuracy_position >= 4) {
          if (! is.na(data_unique[data_unique_id,
                                  accuracy_position])) {
            data_unique[data_unique_id,
                        "best_fao_area"] <- data_unique[data_unique_id,
                                                        accuracy_position]
            accuracy_position <- 0
          } else {
            accuracy_position <- accuracy_position - 1
          }
        }
      }
    }
  }
  # eez spatial overlay ----
  if (overlay_expected %in% c("eez_area",
                              "fao_eez_area")) {
    sf_join_data_eez_area <- sf::st_join(x = data_sf,
                                         y = eez_area,
                                         join = sf::st_intersects,
                                         left = TRUE)
    join_data_eez_area_sub <- sf::st_drop_geometry(sf_join_data_eez_area) %>%
      select(data_id,
             IHO_SEA,
             EEZ,
             ISO_TER1)
    names(join_data_eez_area_sub) <- tolower(names(join_data_eez_area_sub))
    data_unique <- dplyr::inner_join(x = data_unique,
                                     y = join_data_eez_area_sub,
                                     by = "data_id")
    if (! is.null(for_fdi_use)
        && for_fdi_use == TRUE) {
      eez_indicator_referential <- read.csv2(file = system.file("eez_indicator_referential.csv",
                                                                package = "furdeb"))
      eu_countries <- read.csv2(file = system.file("eu_countries.csv",
                                                   package = "furdeb"))
      if (! "division_fao" %in% names(data_unique)) {
        fao_area_sub <- fao_area[fao_area$F_LEVEL == "DIVISION",]
        sf_join_data_fao_area_sub <- sf::st_join(x = data_sf,
                                                 y = fao_area_sub,
                                                 join = sf::st_intersects,
                                                 left = TRUE)
        join_data_fao_area_sub <- sf::st_drop_geometry(sf_join_data_fao_area_sub)
        join_data_fao_area_sub <- join_data_fao_area_sub[, c("data_id",
                                                             "F_DIVISION")] %>%
          dplyr::rename("division_fao" = "F_DIVISION")

        data_unique <- dplyr::inner_join(x = data_unique,
                                         y = join_data_fao_area_sub,
                                         by = "data_id")
      }
      for (data_unique_id in seq_len(length.out = nrow(data_unique))) {
        if (is.na(data_unique[data_unique_id, "division_fao"])) {
          data_unique[data_unique_id, "eez_indicator"] <- NA
        } else {
          if (data_unique[data_unique_id, "division_fao"] %in% eez_indicator_referential$sub_region) {
            if (data_unique[data_unique_id, "iso_ter1"] %in% eu_countries$country_code) {
              data_unique[data_unique_id, "eez_indicator"] <- "EU"
            } else if (is.na(data_unique[data_unique_id, "eez"])) {
              data_unique[data_unique_id, "eez_indicator"] <- "RFMO"
            } else {
              data_unique[data_unique_id, "eez_indicator"] <- "COAST"
            }
          } else {
            data_unique[data_unique_id, "eez_indicator"] <- NA
          }
        }
      }
    }
    data_unique <- select(.data = data_unique,
                          -iho_sea,
                          -iso_ter1)
  }
  # merge with data ----
  data <- dplyr::inner_join(data,
                            data_unique,
                            by = c(latitude_name,
                                   longitude_name)) %>%
    select(-data_id)
  return(data)
}
