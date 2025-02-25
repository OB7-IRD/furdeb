# furdeb 1.0.1- 2025.01.21

## Added
* Add connection to two or more databases in [`data_extraction()`](https://ob7-ird.github.io/furdeb/reference/data_extraction.html) for SQL.

## Changed
* Use package [RPostgres](https://CRAN.R-project.org/package=RPostgres) instead of [RPostgreSQL](https://CRAN.R-project.org/package=RPostgreSQL) in [`postgresql_db_connection()`](https://ob7-ird.github.io/furdeb/reference/postgresql_dbconnection.html) function to fix connection issues.
* Update of time_allocation_activity_code_ref.csv (reference table for allocating fishing time and time at sea according to activity code)

## Changed

# furdeb 1.0.0 - 2024.11.29

## Added
* time_allocation_activity_code_ref.csv : reference table for allocating fishing time and time at sea according to activity code

## Changed
* Update vignettes
* Update control functions 
* Update function data extraction
* Update marine_area_overlay.R

# furdeb 0.0.0.9000 - 2023.10.12

## Added
* First clean design development version
