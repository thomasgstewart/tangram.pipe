#' Table Initialization
#'
#' Initializes the table by specifying the desired elements and data components.
#' @param data the dataset to be used in the table.
#' @param col_var the variable to be used in the table columns. NULL if single summary column desired.
#' @param missing logical: if TRUE, missing data is considered; FALSE only uses complete cases.
#' @param overall logical: if TRUE, an overall column is included.
#' @param comparison logical: if TRUE, a comparison test is conducted between columns.
#' @return A list containing separate entries holding information provided in the function's arguments, as well as a calculated number of column categories to include for the initialized table.
#' @import dplyr
#' @keywords tangram.pipe
#' @examples 
#' x <- tbl_start(iris, "Species", missing=TRUE, overall=TRUE, comparison=TRUE)
#' @export

tbl_start <- function(
    data
  , col_var
  , missing = FALSE
  , overall = TRUE
  , comparison = FALSE
){
  class(data) <- "data.frame"
  if (!is.null(col_var)){
    num_col <- data[col_var] %>%
      filter(!is.na(data[col_var])) %>%
      unique() %>%
      nrow()
  } else {
    num_col <- 1
  }
  out <- list(data=data, col_var=col_var, missing=missing, overall=overall, comparison=comparison, num_col=num_col)
}
