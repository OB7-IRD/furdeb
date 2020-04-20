#' @name add_vertices
#' @title Densify a spatial polygon by adding vertices
#' @description Densify a spatial polygon by adding vertices.
#' @param sp (SpatialPolygonsDataFrame) An object of class "SpatialPolygonsDataFrame".
#' @param each (numeric or integer) The step value to use to create vertices. By default 0.1.
#' @param parallel (logical) Run in parallel (if you have at least two processor cores). By default FALSE.
#' @param ... Others parallel options
#' @return An object of class "SpatialPolygonsDataFrame".
#' @references
#' From a function developed by Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}.
#' @export
#' @importFrom parallel mclapply
add_vertices <- function(sp,
                         each = as.numeric(0.1),
                         parallel = FALSE,
                         ...) {
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
  # sp argument checking
  if (class(sp) != "SpatialPolygonsDataFrame") {
    stop("invalid \"sp\" argument, class \"SpatialPolygonsDataFrame\" expected.\n")
  }
  # each argument checking
  if (! class(each) %in% c("numeric")
      || length(each) != 1
      || each <= 0) {
    stop("invalid \"each\" argument, class numeric with one value superior to zero expected.\n")
  }
  sp@polygons <- apply_handler(sp@polygons,
                               function(p) {
                                 coords <- slot(p,"Polygons")[[1]]@coords
                                 newcoords <- do.call("rbind",lapply(1:(nrow(coords)-1),
                                                                     function(i) {
                                                                       i_coords <- coords[i:(i+1), ]
                                                                       out_coords <- data.frame(
                                                                         x = seq(from = i_coords[1L, 1L],
                                                                                 to = i_coords[2L, 1L],
                                                                                 by = ifelse(i_coords[1L, 1L] <= i_coords[2L,1L],
                                                                                             each,
                                                                                             -each)),
                                                                         y = seq(from = i_coords[1L, 2L],
                                                                                 to = i_coords[2L, 2L],
                                                                                 by = ifelse(i_coords[1L, 2L] <= i_coords[2L, 2L],
                                                                                             each,
                                                                                             -each)))
                                                                       if (i < (nrow(coords) - 1)) {
                                                                         out_coords <- out_coords[, -nrow(out_coords)]
                                                                       }
                                                                       out_coords <- as.matrix(out_coords)
                                                                       return(out_coords)
                                                                     }))
                                 slot(p,"Polygons")[[1]]@coords <- newcoords
                                 return(p)
                               },
                               ...)
  return(sp)
}
