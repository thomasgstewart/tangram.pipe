#' Categorical Row
#'
#' Adds in a categorical row to the table.
#' @param list_obj the name of the tbl_start object previously initialized.
#' @param row_var the name of the variable to be used in the rows.
#' @param col_var the variable to be used in the table columns. Default is from initialized tbl_start object
#' @param newdata enter new dataset name if different from that initialized in tbl_start
#' @param rowlabels the label for the table row name, if different from row_var
#' @param summary summary function for the data. Default will compute proportion (N)
#' @param missing missing logical: if TRUE, missing data is considered; FALSE only uses complete cases.
#' @param overall logical: if TRUE, an overall column is included.
#' @param comparison the name of the comparison test to do, if different from that initialized in tbl_start.
#' @param digits significant digits to use.
#' @import dplyr
#' @keywords tangram.pipe
#' @examples 
#' x <- tbl_start(iris2, "Species", missing=TRUE, overall=TRUE, comparison=TRUE) %>%
#'   cat_row("Stem.Size", rowlabels="Stem Size")
#' @export

cat_row <- function(
    list_obj
  , row_var
  , col_var=NULL
  , newdata=FALSE
  , rowlabels=NULL
  , summary=cat_default
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
      comparison <- cat_comp_default
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
    data <- list_obj[['data']][,c(row_var, col_var)] #list_obj %>% le('data') %>% select(row_var, col_var)
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
  cat_out <- summary(data, rowlabels, missing, digits)
  if (overall == FALSE){
    cat_out <- cat_out[,-ncol(cat_out)]
  }

  if (class(comparison) == "function" & num_col > 1){
    comp <- comparison(data, digits)
    for (i in 1:ncol(comp)){
      cat_out$compare <- ""
      cat_out$compare[1] <- comp[i]
      cat_out$compare <- as.character(cat_out$compare)
      colnames(cat_out)[ncol(cat_out)] <- colnames(comp)[i]
    }
    colnames(cat_out)[ncol(cat_out)] <- "Compare: All Groups"
  }


  list_obj[[length(list_obj) + 1]] <- cat_out
  return(list_obj)
}
