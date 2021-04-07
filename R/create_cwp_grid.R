#' @name create_cwp_grid
#' @title Creates a CWP (FAO Coordinating Working Party) grid spatial object
#' @description Creates a CWP grid spatial object.
#' @param resolution {\link[base]{character}} expected. A string matching one of the accepted resolution values. Accepted resolutions values are "10min_x_10min", "20min_x_20min", "30min_x_30min", "30min_x_1deg", "1deg_x_1deg", "5deg_x_5deg", "10deg_x_10deg", "20deg_x_20deg" and "30deg_x_30deg".
#' @param longitude_min {\link[base]{integer}} expected. Longitude minimum value of the output grid in decimal degree. By default -180.
#' @param latitude_min {\link[base]{integer}} expected. Latitude minimum value of the output grid in decimal degree. By default -90.
#' @param longitude_max {\link[base]{integer}} expected. Longitude maximum value of the output grid in decimal degree. By default 180.
#' @param latitude_max {\link[base]{integer}} expected. Latitude maximum value of the output grid in decimal degree. By default 90.
#' @param parallel {\link[base]{logical}} expected. Run in parallel (if you have at least two processor cores). By default FALSE.
#' @param ... Others parallel options
#' @return Create an object of class "SpatialPolygonsDataFrame" named grid_cwp_resolution (where resolution is the function argument).
#' @references
#' CWP Handbook - http://www.fao.org/fishery/cwp/handbook/G/en
#' From a function developed by Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}.
#' @export
#' @importFrom parallel mclapply
#' @importFrom sp CRS proj4string spChFIDs SpatialPoints spTransform SpatialPolygons
#' @importFrom raster raster extent ncell
#' @importFrom rgeos gArea
create_cwp_grid <- function(resolution = "1deg_x_1deg",
                            longitude_min = -180,
                            latitude_min = -90,
                            longitude_max = 180,
                            latitude_max = 90,
                            parallel = FALSE,
                            ...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " - Start process for CWP grid (",
      gsub(pattern = "_",
           replacement = " ",
           x = resolution),
      ") creation.\n",
      "[minimum longitude: ",
      longitude_min,
      ", maximum longitude: ",
      longitude_max,
      ", minimum latitude: ",
      latitude_min,
      ", maximum latitude: ",
      latitude_max,
      "]\n",
      sep = "")
  # parallel argument checking
  if (class(parallel) != "logical") {
    stop("invalid \"parallel\" argument, class logical expected.\n")
  } else {
    apply_handler <- if (parallel == TRUE) {
      parallel::mclapply
    } else {
      lapply
    }
  }
  # latitude and longitude arguments checking
  if (class(longitude_min) != "numeric"
      || length(longitude_min) != 1
      || longitude_min < -180
      || longitude_min > 180) {
    stop("invalid \"longitude_min\" argument, class numeric with value between -180 and 180 expected.\n")
  }
  if (class(latitude_min) != "numeric"
             || length(latitude_min) != 1
             || latitude_min < -90
             || latitude_min > 90) {
    stop("invalid \"latitude_min\" argument, class numeric with value between -90 and 90 expected.\n")
  }
  if (class(longitude_max) != "numeric"
             || length(longitude_max) != 1
             || longitude_max < -180
             || longitude_max > 180) {
    stop("invalid \"longitude_max\" argument, class numeric with value between -180 and 180 expected.\n")
  }
  if (class(latitude_max) != "numeric"
             || length(latitude_max) != 1
             || latitude_max < -90
             || latitude_max > 90) {
    stop("invalid \"latitude_max\" argument, class numeric with value between -90 and 90 expected.\n")
  }
  # resolution argument checking
  # reference resolutions
  grids <- data.frame(
    # referred as code A in the CWP Handbook
    size = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    # in decimal degree
    lat = c(1/6, 1/3, 0.5, 0.5, 1, 5, 10, 20, 30),
    lon = c(1/6, 1/3, 0.5, 1, 1, 5, 10, 20, 30),
    resolution = c("10min_x_10min", "20min_x_20min","30min_x_30min", "30min_x_1deg", "1deg_x_1deg", "5deg_x_5deg", "10deg_x_10deg", "20deg_x_20deg", "30deg_x_30deg"))
  if (class(resolution) != "character"
      || length(resolution) != 1
      || ! resolution %in% c("10min_x_10min", "20min_x_20min","30min_x_30min", "30min_x_1deg", "1deg_x_1deg", "5deg_x_5deg", "10deg_x_10deg", "20deg_x_20deg", "30deg_x_30deg")) {
    stop("invalid \"resolution\" argument, class character with one value accepted. Values accepted are ",
         paste(paste0("'",
                      grids$resolution, "'"),
               collapse = ", "))
  } else {
    grid <- grids[grids$resolution == resolution,]
  }
  # create grid
  eckp4s <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  # +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
  llp4s <- "+init=epsg:4326"
  llcrs <- sp::CRS(llp4s)
  r <- raster::raster(raster::extent(matrix(c(longitude_min,
                                              latitude_min,
                                              longitude_max,
                                              latitude_max),
                                            nrow = 2)),
                      nrow = length(seq(latitude_min,
                                        latitude_max,
                                        grid$lat)) - 1,
                      ncol = length(seq(longitude_min,
                                        longitude_max,
                                        grid$lon)) - 1,
                      crs = llp4s)
  r[] <- seq_len(length.out = raster::ncell(r))
  sp <- as(r, "SpatialPolygonsDataFrame")
  sp::proj4string(sp) <- llcrs
  # densify adding vertices each minute
  sp <- furdeb::add_vertices(sp = sp,
                             each = 1 / 60,
                             parallel = parallel)
  # attributes (including grid coding)
  idx <- 0
  attrs <- do.call("rbind",
                   apply_handler(sp@polygons,
                                 function(poly) {
                                   labpt <- slot(poly, "labpt")
                                   quadrant <- paste0(ifelse(test = labpt[2] < 0,
                                                             yes = "S",
                                                             no = "N"),
                                                      ifelse(labpt[1] < 0,
                                                             yes = "W",
                                                             no = "E"))
                                   quadrant_id <- switch(quadrant,
                                                         "NE" = 1L,
                                                         "SE" = 2L,
                                                         "SW" = 3L,
                                                         "NW" = 4L)
                                   corner_lon <- ifelse(test = nchar(min(abs(sp::bbox(poly)[1L, ]))) == 2,
                                                        yes = paste0("0", min(abs(sp::bbox(poly)[1L, ]))),
                                                        no = ifelse(test = nchar(min(abs(sp::bbox(poly)[1L, ]))) == 1,
                                                                    yes = paste0("00", min(abs(sp::bbox(poly)[1L, ]))),
                                                                    no = min(abs(sp::bbox(poly)[1L, ]))))
                                   corner_lat <- ifelse(test = nchar(x = min(abs(sp::bbox(poly)[2L, ]))) == 1,
                                                        yes = paste0("0", min(abs(sp::bbox(poly)[2L, ]))),
                                                        no = min(abs(sp::bbox(poly)[2L, ])))
                                   gridcode <- paste0(grid$size,
                                                      quadrant_id,
                                                      corner_lat,
                                                      corner_lon)
                                   cwp.idx <- NA
                                   if (grid$size < 5) {
                                     m.bbox <- sp::bbox(poly)
                                     m <- as.integer(floor(m.bbox))
                                     if (m[4] == m[2]) {
                                       m[4] <- m[4] + 1
                                     }
                                     if (m[3] == m[1]) {
                                       m[3] <- m[3] + 1
                                     }
                                     mr <- raster::raster(raster::extent(matrix(m,
                                                                                nrow = 2)),
                                                          nrow = length(seq(m[2], m[4], grid$lat)) - 1,
                                                          ncol = length(seq(m[1], m[3], grid$lon)) - 1,
                                                          crs = NA)
                                     mr[] <- seq_len(length.out = raster::ncell(mr))
                                     mr.sp <- as(mr, "SpatialPolygonsDataFrame")
                                     mr.seq <- as.integer(sapply(mr.sp@polygons,
                                                                 slot,
                                                                 "ID"))
                                     mr.seq <- switch(quadrant,
                                                      "SE" = as.character(mr.seq),
                                                      "NW" = as.character(rev(mr.seq)),
                                                      "NE" = as.character(unlist(rev(split(mr.seq,
                                                                                           ceiling(seq_along(mr.seq) * grid$lon))))),
                                                      "SW" = as.character(unlist(rev(split(rev(mr.seq),
                                                                                           ceiling(seq_along(rev(mr.seq)) * grid$lon))))))
                                     sp::spChFIDs(mr.sp) <- mr.seq
                                     pt <- sp::SpatialPoints(coords = matrix(data = labpt,
                                                                             nrow = 1,
                                                                             ncol = 2))
                                     mr.sp.idx <- as.integer(sp::over(pt,
                                                                      mr.sp))
                                     cwp.idx <- as.integer(slot(mr.sp[mr.sp.idx, ]@polygons[[1]],
                                                                "ID"))
                                     gridcode <- paste0(gridcode,
                                                        cwp.idx)
                                   }
                                   df <- data.frame(gridtype = grid$resolution,
                                                    quadrant = quadrant,
                                                    longitude = labpt[1],
                                                    latitude = labpt[2],
                                                    cwp_a = grid$size,
                                                    cwp_b = quadrant_id,
                                                    cwp_c = corner_lat,
                                                    cwp_d = corner_lon,
                                                    cwp_e = cwp.idx,
                                                    cwp_code = gridcode,
                                                    surface = rgeos::gArea(sp::spTransform(sp::SpatialPolygons(Srl = list(poly),
                                                                                                               proj4string = llcrs),
                                                                                           eckp4s)))
                                   return(df)
                                 }, ...))
  sp@data <- attrs
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      " - Successful process for CWP grid (",
      gsub(pattern = "_",
           replacement = " ",
           x = resolution),
      ") creation.\n",
      "[minimum longitude: ",
      longitude_min,
      ", maximum longitude: ",
      longitude_max,
      ", minimum latitude: ",
      latitude_min,
      ", maximum latitude: ",
      latitude_max,
      "]\n",
      sep = "")
  assign(x = paste0("grid_cwp_",
                    resolution),
         value = sp,
         envir = .GlobalEnv)
}
