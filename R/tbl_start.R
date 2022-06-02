#' Table Initialization
#'
#' Initializes a `tangram.pipe` table by specifying the desired elements and data components.
#' @param data The dataset to be used in the table.
#' @param col_var The variable to be used in the table columns. NULL if single summary column desired.
#' @param missing logical: if TRUE, missing data is considered; FALSE only uses complete cases.
#' @param overall logical: if TRUE, an overall column is included.
#' @param comparison logical: if TRUE, a comparison test is conducted between columns.
#' @param digits The default number of digits to use in the table. By default, the package will use 2 significant digits.
#' @param default_num_summary The default summary function to use for numerical rows. By default, the package will use `num_default()`, but the user can also choose `num_minmax`, `num_medianiqr`, `num_mean_sd`, or write a custom function to use for the rows.
#' @param default_cat_summary The default summary function to use for categorical rows. By default, the package will use `cat_default()`, but the user can also choose `cat_pct` or write a custom function to use for the rows.
#' @param default_binary_summary The default summary function to use for binary rows. By default, the package will use `binary_default()`, but the user can also choose `binary_pct` or write a custom function to use for the rows.
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
  , digits = 2
  , default_num_summary = num_default
  , default_cat_summary = cat_default
  , default_binary_summary = binary_default
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
  out <- list(data = data, 
              col_var = col_var, 
              missing = missing, 
              overall = overall, 
              comparison = comparison, 
              num_col = num_col,
              digits = digits,
              default_num_summary = default_num_summary,
              default_cat_summary = default_cat_summary,
              default_binary_summary = default_binary_summary)
}
