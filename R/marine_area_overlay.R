#' @name marine_area_overlay
#' @title Consistent spatial marine area overlay
#' @description Consistent spatial marine area overlay for points, grids and polygons (related to fao, eez or ices areas).
#' @param data {\link[base]{data.frame}} expected. R dataframe, with at least two columns with longitude and latitude values. Be careful! Your longitude and latitude data have to be in the WGS84 projection and coordinates in decimal degrees.
#' @param overlay_expected {\link[base]{character}} expected. Type of overlay output. You can choose between "fao_area", "eez_area", "fao_eez_area", "ices_area" or "all".
#' @param longitude_name {\link[base]{character}} expected. Longitude column name in your data.
#' @param latitude_name {\link[base]{character}} expected. Latitude column name in your data.
#' @param fao_area_file_path {\link[base]{character}} expected. File path of the fao area shape. You can provide a .shp or a .Rdata/.RData file.
#' @param fao_overlay_level {\link[base]{character}} expected. Level of fao accuarcy that you want for classified your data. By default, major fao fishing area are selected. Check the section details below.
#' @param auto_selection_fao {\link[base]{logical}} expected. Add a new column in the output with the most detailed overlay level available.
#' @param eez_area_file_path {\link[base]{character}} expected. File path of the eez area shape. You can provide a .shp or a .Rdata/.RData file.
#' @param for_fdi_use {\link[base]{logical}} expected. Add a new column in the output with the FDI variable "eez_indicator".
#' @param ices_area_file_path {\link[base]{character}} expected. File path of the ices area shape. You can provide a .shp or a .Rdata/.RData file.
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
marine_area_overlay <- function(data,
                                overlay_expected,
                                longitude_name,
                                latitude_name,
                                fao_area_file_path = NULL,
                                fao_overlay_level = NULL,
                                auto_selection_fao = FALSE,
                                eez_area_file_path = NULL,
                                for_fdi_use = NULL,
                                ices_area_file_path = NULL,
                                silent = FALSE) {
  # 0 - Local binding global variables ----
  data_id <- NULL
  IHO_SEA <- NULL
  EEZ <- NULL
  ISO_TER1 <- NULL
  iho_sea <- NULL
  iso_ter1 <- NULL
  ICESNAME <- NULL
  sub_region <- NULL
  # 1 - Arguments verifications ----
  codama::r_type_checking(r_object = data,
                          type = "data.frame")
  codama::r_type_checking(r_object = overlay_expected,
                          type = "character",
                          length = 1L,
                          allowed_value = c("fao_area",
                                            "eez_area",
                                            "ices_area",
                                            "fao_eez_area",
                                            "all"))
  codama::r_type_checking(r_object = longitude_name,
                          type = "character",
                          length = 1L)
  codama::r_type_checking(r_object = latitude_name,
                          type = "character",
                          length = 1L)
  if (! is.null(x = fao_area_file_path)) {
    codama::file_path_checking(file_path =  fao_area_file_path,
                               extension = c("Rdata",
                                             "RData",
                                             "shp"))
  }
  codama::r_type_checking(r_object = fao_overlay_level,
                          type = "character",
                          length = 1L,
                          allowed_value = c("ocean",
                                            "major",
                                            "subarea",
                                            "division",
                                            "subdivision",
                                            "subunit"))
  codama::r_type_checking(r_object = auto_selection_fao,
                          type = "logical",
                          length = 1L)
  if (! is.null(x = fao_area_file_path)) {
    codama::file_path_checking(file_path =  fao_area_file_path,
                               extension = c("Rdata",
                                             "RData",
                                             "shp"))
  }
  codama::r_type_checking(r_object = for_fdi_use,
                          type = "logical",
                          length = 1L)
  if (! is.null(x = ices_area_file_path)) {
    codama::file_path_checking(file_path =  ices_area_file_path,
                               extension = c("Rdata",
                                             "RData",
                                             "shp"))
  }
  codama::r_type_checking(r_object = silent,
                          type = "logical",
                          length = 1L)
  if (silent == FALSE) {
    message("Be careful!. ",
            "You're spatial coordinates have to be in WGS84 projection. ",
            "Be patient! The function could be long.")
  }
  # shapes imports ----
  # fao area
  if (overlay_expected %in% c("fao_area",
                              "fao_eez_area",
                              "all")
      || (overlay_expected == "eez_area"
          && (! is.null(for_fdi_use)
              && for_fdi_use == TRUE))) {
    fao_area_file_path_extension <- dplyr::last(x = unlist(strsplit(fao_area_file_path,
                                                                    "[.]")))
    if (fao_area_file_path_extension == "shp") {
      fao_area <- sf::read_sf(fao_area_file_path)
      if (! all(sf::st_is_valid(fao_area))) {
        fao_area <- sf::st_make_valid(fao_area)
      }
    } else if (fao_area_file_path_extension == "Rdata") {
      fao_area <- get(x = load(file = fao_area_file_path))
      if (paste(class(fao_area),
                collapse = " ") != "sf tbl_df tbl data.frame") {
        stop(format(x = Sys.time(),
                    "%Y-%m-%d %H:%M:%S"),
             " - Invalid fao shapefile, R object of class sf.")
      }
    } else {
      stop(format(x = Sys.time(),
                  "%Y-%m-%d %H:%M:%S"),
           " - Invalid \"fao_area_file_path\" argument, shp or Rdata extensions expected.")
    }
  }
  # eez area
  if (overlay_expected %in% c("eez_area",
                              "fao_eez_area",
                              "all")) {
    eez_area_file_path_extension <- dplyr::last(x = unlist(strsplit(eez_area_file_path,
                                                                    "[.]")))
    if (eez_area_file_path_extension == "shp") {
      eez_area <- sf::read_sf(eez_area_file_path)
      if (! all(sf::st_is_valid(eez_area))) {
        eez_area <- sf::st_make_valid(eez_area)
      }
    } else if (eez_area_file_path_extension %in% c("Rdata",
                                                   "RData")) {
      eez_area <- get(x = load(file = eez_area_file_path))
      if (paste(class(eez_area),
                collapse = " ") != "sf tbl_df tbl data.frame") {
        stop(format(x = Sys.time(),
                    "%Y-%m-%d %H:%M:%S"),
             " - Invalid eez shapefile, R object of class sf.")
      }
    } else {
      stop(format(x = Sys.time(),
                  "%Y-%m-%d %H:%M:%S"),
           " - Invalid \"eez_area_file_path\" argument, shp or Rdata extensions expected.")
    }
  }
  # ices area
  if (overlay_expected %in% c("ices_area",
                              "all")) {
    ices_area_file_path_extension <- dplyr::last(x = unlist(strsplit(ices_area_file_path,
                                                                     "[.]")))
    if (ices_area_file_path_extension == "shp") {
      ices_area <- sf::read_sf(ices_area_file_path)
      if (! all(sf::st_is_valid(ices_area))) {
        ices_area <- sf::st_make_valid(ices_area)
      }
    } else if (ices_area_file_path_extension == "Rdata") {
      ices_area <- get(x = load(file = ices_area_file_path))
      if (paste(class(ices_area),
                collapse = " ") != "sf tbl_df tbl data.frame") {
        stop(format(x = Sys.time(),
                    "%Y-%m-%d %H:%M:%S"),
             " - Invalid ices shapefile, R object of class sf.")
      }
    } else {
      stop(format(x = Sys.time(),
                  "%Y-%m-%d %H:%M:%S"),
           " - Invalid \"ices_area_file_path\" argument, shp or Rdata extensions expected.")
    }
  }
  # data design ----
  data_unique <- unique(data[, c(longitude_name,
                                 latitude_name)])
  data_unique$data_id <- seq_len(length.out = nrow(data_unique))
  data_sf <- sf::st_as_sf(x = data_unique,
                          coords = c(longitude_name,
                                     latitude_name),
                          crs = 4326)
  if (overlay_expected %in% c("fao_area",
                              "fao_eez_area",
                              "all")) {
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
      if (nrow(x = data_sf) != nrow(x = sf_join_data_fao_area_sub)) {
        warning(format(x = Sys.time(),
                       format = "%Y-%m-%d %H:%M:%S"),
                " - At least one position is associated with more than one fao area. Arbitrary selection of the first zone.",
                sep = "")
        sf_join_data_fao_area_sub = sf_join_data_fao_area_sub[! duplicated(x = sf_join_data_fao_area_sub$data_id),  ]
      }
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
      accuracy_position_ori <- ncol(x = data_unique)
      for (data_unique_id in seq_len(length.out = nrow(data_unique))) {
        accuracy_position <- accuracy_position_ori
        while (accuracy_position >= 4) {
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
                              "fao_eez_area",
                              "all")) {
    # setup sf
    value_s2_sf <- sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(use_s2 = FALSE))
    sf_join_data_eez_area <- sf::st_join(x = data_sf,
                                         y = eez_area,
                                         join = sf::st_intersects,
                                         left = TRUE)
    if (length(unique(x = sf_join_data_eez_area$data_id)) != nrow(x = sf_join_data_eez_area)) {
      warning(format(x = Sys.time(),
                     format = "%Y-%m-%d %H:%M:%S"),
              " - At least one position is associated with more than one eez area. Output eez area will be NA.",
              sep = "")
      data_id_duplicated <- unique(x = sf_join_data_eez_area[duplicated(sf_join_data_eez_area$data_id), ]$data_id)
      sf_join_data_eez_area <- sf_join_data_eez_area[! duplicated(x = sf_join_data_eez_area$data_id), ]
      for (data_id in data_id_duplicated) {
        sf_join_data_eez_area[sf_join_data_eez_area$data_id == data_id, "EEZ"] <- NA
        sf_join_data_eez_area[sf_join_data_eez_area$data_id == data_id, "ISO_TER1"] <- NA
      }
    }
    join_data_eez_area_sub <- sf::st_drop_geometry(sf_join_data_eez_area) %>%
      dplyr::select(data_id,
                    IHO_SEA,
                    EEZ,
                    ISO_TER1)
    names(join_data_eez_area_sub) <- tolower(names(join_data_eez_area_sub))
    data_unique <- dplyr::inner_join(x = data_unique,
                                     y = join_data_eez_area_sub,
                                     by = "data_id")
    if (! is.null(for_fdi_use)
        && for_fdi_use == TRUE) {
      eez_indicator_referential <- utils::read.csv2(file = system.file("eez_indicator_referential.csv",
                                                                       package = "furdeb"))
      eu_countries <- utils::read.csv2(file = system.file("eu_countries.csv",
                                                          package = "furdeb"))
      if (! "division_fao" %in% names(data_unique)) {
        fao_area_sub <- fao_area[fao_area$F_LEVEL == "DIVISION", ]
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
        if (data_unique[data_unique_id, "division_fao"] %in% eez_indicator_referential$sub_region
            || data_unique[data_unique_id, "subarea_fao"] %in% eez_indicator_referential$sub_region) {
          current_eez_indicator_referential <- dplyr::filter(.data = eez_indicator_referential,
                                                             sub_region %in% c(data_unique[data_unique_id, "division_fao"],
                                                                               data_unique[data_unique_id, "subarea_fao"]))
          if (nrow(x = current_eez_indicator_referential) != 1) {
            if (data_unique[data_unique_id, "iso_ter1"] %in% eu_countries$country_code
                && "EU" %in% current_eez_indicator_referential$eez_indicator
                || (is.na(x = data_unique[data_unique_id, "iso_ter1"])
                    && all(current_eez_indicator_referential$eez_indicator %in% c("EU",
                                                                                  "UK")))) {
              data_unique[data_unique_id, "eez_indicator"] <- "EU"
            } else if (data_unique[data_unique_id, "iso_ter1"] %in% "GBR"
                       && "UK" %in% current_eez_indicator_referential$eez_indicator) {
              data_unique[data_unique_id, "eez_indicator"] <- "UK"
            } else if ((is.na(data_unique[data_unique_id, "eez"]))
                       && "RFMO" %in% current_eez_indicator_referential$eez_indicator) {
              data_unique[data_unique_id, "eez_indicator"] <- "RFMO"
            } else if ("COAST" %in% current_eez_indicator_referential$eez_indicator) {
              data_unique[data_unique_id, "eez_indicator"] <- "COAST"
            } else {
              data_unique[data_unique_id, "eez_indicator"] <- "NA"
            }
          } else {
            data_unique[data_unique_id, "eez_indicator"] <- current_eez_indicator_referential$eez_indicator
          }
        } else {
          data_unique[data_unique_id, "eez_indicator"] <- "NA"
        }
      }
    }
    data_unique <- dplyr::select(.data = data_unique,
                                 -iho_sea) %>%
      dplyr::rename(eez_country = iso_ter1)
    # setup
    suppressMessages(sf::sf_use_s2(use_s2 = value_s2_sf))
  }
  # ices spatial overlay ----
  if (overlay_expected %in% c("ices_area",
                              "all")) {
    sf_join_data_ices_area <- sf::st_join(x = data_sf,
                                          y = ices_area,
                                          join = sf::st_intersects,
                                          left = TRUE)
    join_data_ices_area_sub <- sf::st_drop_geometry(sf_join_data_ices_area) %>%
      dplyr::select(data_id,
                    ICESNAME) %>%
      dplyr::rename("ices_area" = "ICESNAME")
    data_unique <- dplyr::inner_join(x = data_unique,
                                     y = join_data_ices_area_sub,
                                     by = "data_id")
  }
  # merge with data ----
  data <- dplyr::inner_join(data,
                            data_unique,
                            by = c(latitude_name,
                                   longitude_name)) %>%
    dplyr::select(-data_id)
  return(data)
}
