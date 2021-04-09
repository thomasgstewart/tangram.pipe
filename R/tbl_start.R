#' Table Initialization
#'
#' Initializes the table by specifying the desired elements and data components.
#' @param data the dataset to be used in the table.
#' @param col_var the variable to be used in the table columns.
#' @param missing logical: if TRUE, missing data is considered; FALSE only uses complete cases.
#' @param overall logical: if TRUE, an overall column is included.
#' @param comparison logical: if TRUE, a comparison test is conducted between columns.
#' @import dplyr
#' @keywords tangram.pipe
#' @examples 
#' x <- tbl_start(iris, "Species", missing=TRUE, overall=TRUE, comparison=TRUE)
#' @export

tbl_start <- function(
    data
  , col_var
  , missing
  , overall
  , comparison
){
  num_col <- data[col_var] %>%
    filter(!is.na(data[col_var])) %>%
    unique() %>%
    nrow()
  #nrow(unique(filter(data[col_var], !is.na(data[col_var]))))
  pipetab_out <- list(data=data, col_var=col_var, missing=missing, overall=overall, comparison=comparison, num_col=num_col)
}
#add calculation with number of columns so other functions can use info
