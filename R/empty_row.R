#' Empty Row
#'
#' Produces a empty dividing row in the table.  May have a row header.
#' @param list_obj the name of the tbl_start object previously initialized.
#' @param header a header to include for the empty row.
#' @keywords tangram.pipe
#' @export

empty_row <- function(
  list_obj
  , header=NULL
){
  if (!is.null(header)){
    list_obj[[length(list_obj) + 1]] <- rep("", dim(list_obj[[length(list_obj)]])[2])
    list_obj[[length(list_obj)]][1] <- header
  } else {
    list_obj[[length(list_obj) + 1]] <- ""
  }
  return(list_obj)
}