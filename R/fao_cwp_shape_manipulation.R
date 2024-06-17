#' @name fao_cwp_shape_manipulation
#' @title Optimisation function for FAO CWP shape file manipulation
#' @description Optimise FAO CWP shape file and save it into a RData format for increase the reading process.
#' @param referential_grid_file_path {\link[base]{character}} expected. File path of the FAO CWP shape file (shp extension expected).
#' @param output_directory_path {\link[base]{character}} expected. Directory path for the output.
#' @export
fao_cwp_shape_manipulation <- function(referential_grid_file_path,
                                       output_directory_path) {
  # 1 - global arguments verifications ----
  codama::file_path_checking(file_path = referential_grid_file_path,
                             extension = "shp")
  codama::r_type_checking(r_object = output_directory_path,
                          type = "character",
                          length = 1L)
  # 2 - Process ----
  message(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Start process for FAO CWP shape optimisation.",
          sep = "")
  sf::sf_use_s2(FALSE)
  reference_grid <- sf::read_sf(referential_grid_file_path)
  save(reference_grid,
       file = file.path(output_directory_path,
                        paste0("cwp-grid-map-",
                               unique(reference_grid$GRIDTYPE),
                               ".RData")))
  message(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Successful process for FAO CWP shape optimisation.",
          sep = "")
}
