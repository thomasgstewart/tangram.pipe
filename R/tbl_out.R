#' Output Table
#'
#' Produces a finalized table
#' @param list_obj the name of the tbl_start object previously initialized.
#' @keywords tangram.pipe
#' @export

tbl_out <- function(
  list_obj
){
  sections <- length(list_obj) - 6
  out_tbl <- list_obj[[7]]
  for (i in 1:sections){
    if (all(colnames(out_tbl) == colnames(list_obj[[i + 6]]))){
      out_tbl <- rbind(out_tbl, list_obj[[i + 6]])
    }
  }
  out_tbl
}