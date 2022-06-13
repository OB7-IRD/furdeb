#' @name fao_reference_grid_update
#' @title Specific update of the fao reference grid
#' @description Function for specific update of the fao reference grid.
#' @param reference_grid {\link[sf]{sf}} expected. A R object of class "sf" with data related to an official FAO cwp grid.
#' @export
#' @importFrom dplyr rowwise mutate case_when ungroup
#' @importFrom sf st_coordinates st_centroid
fao_reference_grid_update <- function(reference_grid) {
  # local binding global variables ----
  geometry <- X <- Y <- NULL
  # function ----
  sf::sf_use_s2(use_s2 = FALSE)
  reference_grid_udpdate <- reference_grid %>%
    dplyr::rowwise() %>%
    dplyr::mutate(cwp_centroid_11 = paste0(dplyr::case_when(
      GRIDTYPE == "1deg_x_1deg" ~ "5",
      GRIDTYPE == "5deg_x_5deg" ~ "6",
      GRIDTYPE == "10deg_x_10deg" ~ "7",
      GRIDTYPE == "20deg_x_20deg" ~ "8",
      GRIDTYPE == "30deg_x_30deg" ~ "9",
      TRUE ~ "error"
    ),
    dplyr::case_when(
      QUADRANT == "NE" ~ "1",
      QUADRANT == "SE" ~ "2",
      QUADRANT == "SW" ~ "3",
      QUADRANT == "NW" ~ "4",
      TRUE ~ "error",
    ),
    sub(pattern = "[.]",
        replacement = "",
        x = sprintf(fmt = "%05.2f",
                    abs(x = (data.frame(sf::st_coordinates(x = sf::st_centroid(x = geometry))))$Y))),
    sub(pattern = "[.]",
        replacement = "",
        x = sprintf(fmt = "%05.2f",
                    abs(x = (data.frame(sf::st_coordinates(x = sf::st_centroid(x = geometry))))$X)))),
    cwp_corner_11 = paste0(dplyr::case_when(
      GRIDTYPE == "1deg_x_1deg" ~ "5",
      GRIDTYPE == "5deg_x_5deg" ~ "6",
      GRIDTYPE == "10deg_x_10deg" ~ "7",
      GRIDTYPE == "20deg_x_20deg" ~ "8",
      GRIDTYPE == "30deg_x_30deg" ~ "9",
      TRUE ~ "error"
    ),
    dplyr::case_when(
      QUADRANT == "NE" ~ "1",
      QUADRANT == "SE" ~ "2",
      QUADRANT == "SW" ~ "3",
      QUADRANT == "NW" ~ "4",
      TRUE ~ "error",
    ),
    sub(pattern = "[.]",
        replacement = "",
        x = sprintf(fmt = "%05.2f",
                    (unique(x = data.frame(sf::st_coordinates(x = geometry))) %>%
                       mutate(X = abs(x = X),
                              Y = abs(x = Y)) %>%
                       dplyr::filter(X == min(X)) %>%
                       dplyr::filter(Y == min(Y)))$Y)),
    sub(pattern = "[.]",
        replacement = "",
        x = sprintf(fmt = "%06.2f",
                    (unique(x = data.frame(sf::st_coordinates(x = geometry))) %>%
                       mutate(X = abs(x = X),
                              Y = abs(x = Y)) %>%
                       dplyr::filter(X == min(X)) %>%
                       dplyr::filter(Y == min(Y)))$X)))) %>%
    dplyr::ungroup()
  return(reference_grid_udpdate)
}
