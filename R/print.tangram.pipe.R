#' Printing a Table
#'
#' Prints a finished table
#' @param list_obj the name of the tbl_start object previously initialized.
#' @keywords tangram.pipe
#' @export

print.tangram.pipe <- function(
  list_obj
){
  idx <- which(lapply(list_obj, class)=="tangram.pipe")
  if (length(idx) > 1){
    idx <- idx[1]
  }
  class(list_obj[[idx]]) <- "data.frame"
  return(list_obj[[idx]])
}