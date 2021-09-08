#' Numeric Row
#'
#' Adds in a numeric row to the table.
#' @param list_obj the name of the `tbl_start` object previously initialized.
#' @param row_var the name of the variable to be used in the rows.
#' @param col_var the variable to be used in the table columns. Default is from initialized `tbl_start` object.
#' @param newdata enter new dataset name if different from that initialized in `tbl_start`.
#' @param rowlabel the label for the table row name, if different from `row_var`.
#' @param summary summary function for the data, if different from the one supplied in `tbl_start`.
#' @param missing logical: if TRUE, missing data is considered; FALSE only uses complete cases.
#' @param overall logical: if TRUE, an overall column is included.
#' @param comparison the name of the comparison test to use, if different from that initialized in `tbl_start`.
#' @param digits significant digits to use.
#' @return A list with the numeric row's table information added as a new element to `list_obj`.
#' @import dplyr
#' @keywords tangram.pipe
#' @examples 
#' x <- tbl_start(iris, "Species", missing=TRUE, overall=TRUE, comparison=TRUE) %>%
#'   num_row("Sepal.Length", rowlabel="Sepal Length")
#' @export

num_row <- function(
    list_obj
  , row_var
  , col_var=NULL
  , newdata=FALSE
  , rowlabel=NULL
  , summary=NULL
  , missing=NULL
  , overall=NULL
  , comparison=NULL
  , digits=2
){
  # Determine if row parameters override initialized defaults
  if (is.null(summary)){
    summary <- list_obj[["default_num_summary"]]
  }
  if (is.null(missing)){
    missing <- list_obj[["missing"]]
  }
  if (is.null(overall)){
    overall <- list_obj[["overall"]]
  }
  if (is.null(comparison)){
    comparison <- list_obj[["comparison"]]
    if (comparison == TRUE){
      comparison <- num_diff
    }
  }

  # Formatting row information
  if (is.null(col_var)){
    col_var <- list_obj[["col_var"]]
    num_col <- list_obj[['num_col']]
  } else {
    if (class(newdata) == 'logical'){
      num_col <- list_obj[['data']][col_var] %>%
        filter(!is.na(list_obj[['data']][col_var])) %>%
        unique() %>%
        nrow()
    }
  }
  if (class(newdata) == 'logical'){
    data <- list_obj[['data']][,c(row_var, col_var)]
  } else {
    data <- newdata[,c(row_var, col_var)]
    if (!is.null(col_var)){
      num_col <- data[col_var] %>%
        filter(!is.na(data[col_var])) %>%
        unique() %>%
        nrow()
    } else {
      num_col <- 1
    }
  }
  if (is.null(rowlabel)){
    if (is.null(dim(data))) {
      if  (!is.null(attr(data, "label"))){
        rowlabel <- attr(data, "label")
      } else {
        rowlabel <- row_var
      }
    } else {
      if (!is.null(attr(data[,1], "label"))) {
        rowlabel <- attr(data[,1], "label")
      } else {
        rowlabel <- row_var
      }
    }
  }
  
  if (!is.null(col_var)){
    data[,2] <- as.factor(data[,2])
  }

  #Default summary function will take mean (SD)
  num_out <- summary(data, rowlabel = rowlabel, missing = missing, digits = digits)
  if (overall == FALSE){
    num_out <- num_out[,-ncol(num_out)]
  }

  if (class(comparison) == "function" & num_col > 1){
    comp <- comparison(data, num_col, row_var, digits)
    for (i in 1:ncol(comp)){
      num_out$compare <- ""
      num_out$compare[1] <- comp[i]
      num_out$compare <- as.character(num_out$compare)
      colnames(num_out)[ncol(num_out)] <- colnames(comp)[i]
    }
    colnames(num_out)[ncol(num_out)] <- "Compare: All Groups"
  }

  list_obj[[length(list_obj) + 1]] <- num_out
  return(list_obj)
}





