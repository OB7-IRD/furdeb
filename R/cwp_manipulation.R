#' @param data (data frame) A data frame with data
#' @param cwp_resolution (character) A string matching one of the accepted resolution values. Accepted resolutions values are "10min_x_10min", "20min_x_20min", "30min_x_30min", "30min_x_1deg", "1deg_x_1deg", "5deg_x_5deg", "10deg_x_10deg", "20deg_x_20deg" and "30deg_x_30deg".
cwp_manipulation = function(data,
                            manipulation_process,
                            degree_format,
                            cwp_resolution = "1deg_x_1deg",
                            path_extern_referential_grid = NULL) {
  if (class(cwp_resolution) != "character"
      || length(cwp_resolution) != 1
      || ! cwp_resolution %in% c("10min_x_10min", "20min_x_20min", "30min_x_30min", "30min_x_1deg", "1deg_x_1deg", "5deg_x_5deg", "10deg_x_10deg", "20deg_x_20deg", "30deg_x_30deg")) {
    stop("Invalid \"cwp_resolution\" argument, class character with one value inside expected.\n",
         "Values accepted are \"10min_x_10min\", \"20min_x_20min\", \"30min_x_30min\", \"30min_x_1deg\", \"1deg_x_1deg\", \"5deg_x_5deg\", \"10deg_x_10deg\", \"20deg_x_20deg\" and \"30deg_x_30deg\".")
  }
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
                        cwp_resolution))) {
    reference_grid <- get(x = paste0("grid_cwp_",
                                     cwp_resolution),
                          envir = tmp_envir)
    reference_grid_data <- reference_grid@data
  } else {
    stop("invalid referential grid, no R object named \"",
         paste0("grid_cwp_",
                cwp_resolution),
         "\" available in the R environment provided.\n")
  }

}
