#' @name list_logical_merge
#' @title Logical merging for list
#' @description Process for merging two lists according to the name of them.
#' @param first_list {\link[base]{list}} expected. First list to merge.
#' @param second_list {\link[base]{list}} expected. Second list to merge.
#' @importFrom codama r_type_checking
#' @export
list_logical_merge <- function(first_list,
                               second_list) {
  # 1 - Arguments verification ----
  # first_list argument checking
  if (codama::r_type_checking(r_object = first_list,
                              type = "list",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = first_list,
                                   type = "list",
                                   output = "message"))
  }
  # second_list argument checking
  if (codama::r_type_checking(r_object = second_list,
                              type = "list",
                              output = "logical") != TRUE) {
    return(codama::r_type_checking(r_object = second_list,
                                   type = "list",
                                   output = "message"))
  }
  # 2 - Global process ----
  final_list <- first_list
  if (length(x = which(x = ! names(x = second_list) %in% names(x = final_list))) != 0) {
    current_names_not_commun <- names(second_list)[which(x = ! names(x = second_list) %in% names(x = final_list))]
    for (current_name_not_commun in current_names_not_commun) {
      final_list[[current_name_not_commun]] <- second_list[[current_name_not_commun]]
    }
  }
  if (length(x = which(x = names(x = second_list) %in% names(x = final_list))) != 0) {
    current_names_commun <- names(second_list)[which(x = names(x = second_list) %in% names(x = final_list))]
    for (current_name_commun in current_names_commun) {
      final_list[[current_name_commun]] <- append(x = final_list[[current_name_commun]],
                                                  values = second_list[[current_name_commun]])
    }
  }
  return(final_list)
}
