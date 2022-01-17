#' @name save_pigz
#' @title Save R Object using pigz compression
#' @description Save writes an external representation of R objects to the specified file using pigz parallel implementation compression (http://www.zlib.net/pigz/).
#' @param ... Names of the objects to be saved (as symbols or character strings).
#' @param list {\link[base]{character}} expected. A vector containing the names of objects to be saved.
#' @param output_file_path {\link[base]{character}} expected. Path name of the file where data will be saved.
#' @param cores_utilisation {\link[base]{character}} or {\link[base]{numeric}} expected. Percentage of cores to use to compress (value inferior or equal to 1 expected). Use "auto" for automatic management (number of cores minus 1).
#' @param precheck {\link[base]{logical}} expected. Should the existence of the objects be checked before starting to.
#' @param envir Environment to search for objects to be saved.
#' @param eval_promises {\link[base]{logical}} expected. Should objects which are promises be forced before saving?
#' @details
#' Under windows OS utilisation, download through this link (https://sourceforge.net/projects/pigz-for-windows/) the pigz executable file and place it in the System32 directory. The executable file is also available in the package directory (use the function system.file("pigz.zip", package = "furdeb") for located it).
#' @export
#' @importFrom parallel detectCores
save_pigz <- function(...,
                      list = character(),
                      output_file_path,
                      cores_utilisation = "auto",
                      precheck = TRUE,
                      envir = parent.frame(),
                      eval_promises = TRUE) {
  if (.Platform$OS.type != "windows") {
    stop("function not developed yet for other systems than windows\ntake a look here for move forward https://github.com/barkasn/fastSave\n")
  }
  # arguments verifications ----
  if (cores_utilisation == "auto") {
    cores_number <- parallel::detectCores() - 1
  } else if (is.numeric(x = cores_utilisation)
             && (cores_utilisation > 0
                 & cores_utilisation <= 1)) {
    cores_number <- round(parallel::detectCores() * cores_utilisation)
  } else {
    stop("invalid \"cores_utilisation\" argument\n")
  }
  if (missing(output_file_path)
      && (paste0(class(x = output_file_path),
                 collapse = "_") != "character"
          && length(x = output_file_path) != 1)) {
    stop("invalid \"output_file_path\" argument\n")
  }
  # process ----
  names <- as.character(x = substitute(list(...)))[-1L]
  if (missing(x = list)
      && length(x = names) == 0) {
    stop("nothing specified to be saved\n")
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
                     paste0("--processes ",
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


