#' Printing a Table
#'
#' Prints a finished table
#' @param list_obj the name of the tbl_start object previously initialized.
#' @keywords tangram.pipe
#' @export

tbl_print <- function(
  list_obj
){
  if ("Print" %in% names(list_obj)){
    print(list_obj[['Print']])
  }
}