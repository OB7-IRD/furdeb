# import testing data ----
load(file = system.file("test_data",
                        "lat_lon_cwp_manipulation_data_correct.RData",
                        package = "furdeb"))

# lat_lon_to_cwp_1 ----
# input_degree_format = "decimal_degree"
# cwp_resolution = "1deg_x_1deg"
test_that("lat_lon_to_cwp_1", {
  expect_equal(object = lat_lon_cwp_manipulation(manipulation_process = "lat_lon_to_cwp",
                                                 data_longitude = data_correct[[1]],
                                                 data_latitude = data_correct[[2]],
                                                 input_degree_format = "decimal_degree",
                                                 cwp_resolution = "1deg_x_1deg")$cwp,
               expected = c("5445179", "5125150", "5368080", "5200025"))
})

# cwp_to_lat_lon_1 ----
# input_degree_format = "decimal_degree"
# output_degree_cwp_parameter = "centroid"
# cwp_resolution = "1deg_x_1deg"
test_that("cwp_to_lat_lon_1", {
  expect_equal(object = lat_lon_cwp_manipulation(manipulation_process = "cwp_to_lat_lon",
                                                 data_cwp = data_correct[[3]],
                                                 output_degree_format = "decimal_degree",
                                                 output_degree_cwp_parameter = "centroid",
                                                 cwp_resolution = "1deg_x_1deg")$longitude_decimal_degree_centroid,
               expected = c("-179.5", "150.5", "-80.5", "25.5"))
  expect_equal(object = lat_lon_cwp_manipulation(manipulation_process = "cwp_to_lat_lon",
                                                 data_cwp = data_correct[[3]],
                                                 output_degree_format = "decimal_degree",
                                                 output_degree_cwp_parameter = "centroid",
                                                 cwp_resolution = "1deg_x_1deg")$latitude_decimal_degree_centroid,
               expected = c("45.5", "25.5", "-68.5", "-0.5"))
})

# cwp_to_lat_lon_2 ----
# input_degree_format = "decimal_degree"
# output_degree_cwp_parameter = "corner"
# cwp_resolution = "1deg_x_1deg"
test_that("cwp_to_lat_lon_2", {
  expect_equal(object = lat_lon_cwp_manipulation(manipulation_process = "cwp_to_lat_lon",
                                                 data_cwp = data_correct[[3]],
                                                 output_degree_format = "decimal_degree",
                                                 output_degree_cwp_parameter = "corner",
                                                 cwp_resolution = "1deg_x_1deg")$longitude_decimal_degree_corner,
               expected = c("-179", "150", "-80", "25"))
  expect_equal(object = lat_lon_cwp_manipulation(manipulation_process = "cwp_to_lat_lon",
                                                 data_cwp = data_correct[[3]],
                                                 output_degree_format = "decimal_degree",
                                                 output_degree_cwp_parameter = "corner",
                                                 cwp_resolution = "1deg_x_1deg")$latitude_decimal_degree_corner,
               expected = c("45", "25", "-68", "0"))
})
