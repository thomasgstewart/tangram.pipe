#' Empty Row
#'
#' Produces a empty dividing row in the table.  May have a row header.
#' @param list_obj the name of the tbl_start object previously initialized.
#' @param num_col the number of columns to include.
#' @param header a header to include for the empty row.
#' @keywords tangram.pipe
#' @export

empty_row <- function(
  list_obj
  , num_col=NULL
  , header=NULL
){
  if (is.null(num_col)){
    num_col <- list_obj[['num_col']]
  }
  erow <- c(rep("",num_col))

  list_obj[[length(list_obj) + 1]] <- erow
  if (!is.null(header)){
    list_obj[[length(list_obj)]][1] <- header
  }
  return(list_obj)
}