#' Binary Row
#'
#' Adds in a binary row to the table.
#' @param list_obj the name of the tbl_start object previously initialized.
#' @param row_var the name of the variable to be used in the rows.
#' @param col_var the variable to be used in the table columns. Default is from initialized tbl_start object
#' @param newdata enter new dataset name if different from that initialized in tbl_start
#' @param rowlabels the label for the table row name, if different from row_var
#' @param summary summary function for the data. Default will compute proportion (N)
#' @param reference the name of the row category to use as the reference. Default will use alphabetical first category.
#' @param missing missing logical: if TRUE, missing data is considered; FALSE only uses complete cases.
#' @param overall logical: if TRUE, an overall column is included.
#' @param comparison the name of the comparison test to do, if different from that initialized in tbl_start.
#' @param digits significant digits to use.
#' @import dplyr
#' @keywords tangram.pipe
#' @examples 
#' x <- tbl_start(iris2, "Species", missing=TRUE, overall=TRUE, comparison=TRUE) %>%
#'   binary_row("color", rowlabels="Color")
#' @export

binary_row <- function(
  list_obj
  , row_var
  , col_var=NULL
  , newdata=FALSE
  , rowlabels=NULL
  , summary=binary_default
  , reference=NULL
  , missing=NULL
  , overall=NULL
  , comparison=NULL  #Null or function
  , digits=2
){
  if (is.null(missing)){
    missing <- list_obj[["missing"]]
  }
  if (is.null(overall)){
    overall <- list_obj[["overall"]]
  }
  if (is.null(comparison)){
    comparison <- list_obj[["comparison"]] #Hierarchy of if-else (1. check if comparison not NULL, 2. T/F)
    if (comparison == TRUE){
      comparison <- binary_diff
    }
  }

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
    num_col <- data[col_var] %>%
      filter(!is.na(data[col_var])) %>%
      unique() %>%
      nrow()
  }
  if (is.null(rowlabels)){
    rowlabels <- row_var
  }
  
  data[,2] <- as.factor(data[,2])

  #Default summary function will take % (N)

  #Calculations
  if (is.null(reference)){
    reference = sort(unique(data[,1]))[1]
  }
  binary_out <- summary(data, reference, rowlabels, missing, digits)
  if (overall == FALSE){
    binary_out <- binary_out[,-ncol(binary_out)]
  }

  if (class(comparison) == "function" & num_col > 1){
    comp <- comparison(data, num_col, reference, digits)
    for (i in 1:ncol(comp)){
      binary_out$compare <- ""
      binary_out$compare[1] <- comp[i]
      binary_out$compare <- as.character(binary_out$compare)
      colnames(binary_out)[ncol(binary_out)] <- colnames(comp)[i]
    }
    colnames(binary_out)[ncol(binary_out)] <- "Compare: All Groups"
  }

  list_obj[[length(list_obj) + 1]] <- binary_out
  return(list_obj)
}
