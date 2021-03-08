#' Printing a Table
#'
#' Prints a finished table
#' @param list_obj the name of the tbl_start object previously initialized.
#' @keywords tangram.pipe
#' @method print tangram.pipe
#' @export

print.tangram.pipe <- function(
  list_obj
){
  idx <- which(vapply(list_obj, function(x){"out" %in% class(x)}, TRUE))
  if (length(idx) > 1){
    idx <- idx[1]
  }
  print(list_obj[[idx]])
}