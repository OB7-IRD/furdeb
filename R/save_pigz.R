#' @name save_pigz
#' @title Save R Object using pigz compression
#' @description Save writes an external representation of R objects to the specified file using pigz parallel implementation compression (http://www.zlib.net/pigz/).
#' @param ... Names of the objects to be saved (as symbols or character strings).
#' @param list {\link[base]{character}} expected. A vector containing the names of objects to be saved.
#' @param output_file_path {\link[base]{character}} expected. Path name of the file where data will be saved.
#' @param cores_utilisation {\link[base]{character}} or {\link[base]{integer}} expected. Percentage of cores to use to compress (value inferior or equal to 1 expected). Use "auto" for automatic management (number of cores minus 1).
#' @param compression_level {\link[base]{integer}} expected. Compression level, 0 for no compression to 9 (maximum).
#' @param precheck {\link[base]{logical}} expected. Should the existence of the objects be checked before starting to.
#' @param envir Environment to search for objects to be saved.
#' @param eval_promises {\link[base]{logical}} expected. Should objects which are promises be forced before saving?
#' @details
#' Under windows OS utilisation, download through this link (https://sourceforge.net/projects/pigz-for-windows/) the pigz executable file and place it in the System32 directory. The executable file is also available in the package directory (use the function system.file("pigz.zip", package = "furdeb") for located it).
#' @importFrom parallel detectCores
#' @export
save_pigz <- function(...,
                      list = character(),
                      output_file_path,
                      cores_utilisation = "auto",
                      compression_level = 1L,
                      precheck = TRUE,
                      envir = parent.frame(),
                      eval_promises = TRUE) {
  # 1 - Arguments verification ----
  if (.Platform$OS.type != "windows") {
    return("function not developed yet for other systems than windows\ntake a look here for move forward https://github.com/barkasn/fastSave\n")
  }
  # cores_utilisation argument checking
  if ((codama::r_type_checking(r_object = cores_utilisation,
                               type = "character",
                               length = 1L,
                               allowed_value = "auto",
                               output = "logical")
       || (codama::r_type_checking(r_object = cores_utilisation,
                                   type = "integer",
                                   length = 1L,
                                   output = "logical")
           && (cores_utilisation > 0
               & cores_utilisation <= 1))) != TRUE) {
    return(format(x = Sys.time(),
                  "%Y-%m-%d %H:%M:%S"),
           " - Error, invalid \"cores_utilisation\" argument.\n")
  }
  # output_file_path argument checking
  if (codama::r_type_checking(r_object = output_file_path,
                              type = "character",
                              length = 1L,
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = output_file_path,
                                   type = "character",
                                   length = 1L,
                                   output = "message"))
  }
  # compression_level argument checking
  if (codama::r_type_checking(r_object = compression_level,
                              type = "integer",
                              length = 1L,
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = compression_level,
                                   type = "integer",
                                   length = 1L,
                                   output = "message"))
  }
  # 2 - Global process ----
  if (cores_utilisation == "auto") {
    cores_number <- parallel::detectCores() - 1
  } else {
    cores_number <- round(parallel::detectCores() * cores_utilisation)
  }
  names <- as.character(x = substitute(list(...)))[-1L]
  if (missing(x = list)
      && length(x = names) == 0) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Error, nothing specified to be saved.\n")
  }
  list <- c(list,
            names)
  if (precheck == TRUE) {
    ok <- vapply(list,
                 exists,
                 NA,
                 envir = envir)
    if (! all(ok)) {
      n <- sum(!ok)
      stop(sprintf(ngettext(n,
                            "object %s not found",
                            "objects %s not found"),
                   paste(sQuote(list[!ok]),
                         collapse = ", ")),
           domain = NA)
    }
  }
  on.exit(close(con = con))
  con <- pipe(paste0("pigz ",
                     paste0("-",
                            compression_level),
                     paste0(" --processes ",
                            cores_number),
                     " > ",
                     output_file_path))
  save(...,
       list = list,
       file = con,
       envir = envir,
       eval.promises = eval_promises,
       precheck = precheck)
}


