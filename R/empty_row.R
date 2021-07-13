#' Empty Row
#'
#' Produces a empty dividing row in the table.  May have a row header.
#' @param list_obj the name of the tbl_start object previously initialized.
#' @param header a header to include for the empty row.
#' @return If a header is included, a list object is returned with a one-element dataframe containing the header as the most recent entry to `list_obj`. Otherwise, a list is returned containing a blank character as the last element of `list_obj`.
#' @keywords tangram.pipe
#' @export

empty_row <- function(
  list_obj
  , header=NULL
){
  if (!is.null(header)){
    list_obj[[length(list_obj) + 1]] <- data.frame(Variable=header)
  } else {
    list_obj[[length(list_obj) + 1]] <- ""
  }
  return(list_obj)
}