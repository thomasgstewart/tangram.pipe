#' Categorical Row
#'
#' Adds in a categorical row to a `tangram.pipe` table.
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
#' @param ordering If `ascending`, will sort by overall ascending order; if `descending`, will sort by overall descending order. Default is no row sorting.
#' @param sortcol Column to sort row on. Requires `ordering` to be `ascending` or `descending`. By default, will sort based on overall statistics.
#' @param indent number of spaces to indent category names.
#' @return A list with the categorical row's table information added as a new element to `list_obj`.
#' @seealso Possible summary functions for categorical data:\link[tangram.pipe]{cat_default}, \link[tangram.pipe]{cat_pct}, \link[tangram.pipe]{cat_count}, \link[tangram.pipe]{cat_jama}
#' @seealso Other related row-building functions: \link[tangram.pipe]{num_row}, \link[tangram.pipe]{binary_row}, \link[tangram.pipe]{n_row}, \link[tangram.pipe]{empty_row}
#' @seealso Starting a `tangram.pipe` table: \link[tangram.pipe]{tbl_start}
#' @import dplyr
#' @keywords tangram.pipe
#' @examples 
#' iris$Stem.Size <- sample(c("Small", "Medium", "Medium", "Large"), size=150, replace=TRUE)
#' x <- tbl_start(iris, "Species", missing=TRUE, overall=TRUE, comparison=TRUE) %>%
#'   cat_row("Stem.Size", rowlabel="Stem Size")
#' @export

cat_row <- function(
    list_obj
  , row_var
  , col_var=NULL
  , newdata=FALSE
  , rowlabel=NULL
  , summary=NULL
  , missing=NULL
  , overall=NULL
  , comparison=NULL  #Null or function
  , digits=2
  , ordering="none"
  , sortcol=NULL
  , indent=5
){
  # Determine if row parameters override initialized defaults
  if (is.null(summary)){
    summary <- list_obj[["default_cat_summary"]]
  }
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

  # Formatting row information
  if (is.null(col_var)){
    col_var <- list_obj[["col_var"]]
    num_col <- list_obj[['num_col']]
  } else {
    if (inherits(newdata, 'logical')){
      num_col <- list_obj[['data']][col_var] %>%
        filter(!is.na(list_obj[['data']][col_var])) %>%
        unique() %>%
        nrow()
    }
  }
  if (inherits(newdata, 'logical')){
    data <- list_obj[['data']][,c(row_var, col_var)] #list_obj %>% le('data') %>% select(row_var, col_var)
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
  
  #Default summary function will take % (N)

  #Calculations
  cat_out <- summary(data, 
                     rowlabel = rowlabel, 
                     missing = missing, 
                     digits = digits, 
                     ordering = ordering, 
                     sortcol = sortcol)
  if (overall == FALSE){
    cat_out <- cat_out[,-ncol(cat_out)]
  }

  if (inherits(comparison, "function") & num_col > 1){
    comp <- comparison(data, digits)
    for (i in 1:ncol(comp)){
      cat_out$compare <- ""
      cat_out$compare[1] <- comp[i]
      cat_out$compare <- as.character(cat_out$compare)
      colnames(cat_out)[ncol(cat_out)] <- colnames(comp)[i]
    }
    colnames(cat_out)[ncol(cat_out)] <- "Compare: All Groups"
  }
  
  idt <- paste(rep(" ", indent), collapse="")
  cat_out[,1] <- ifelse(cat_out[,2]=="" & cat_out[,1] != "", paste0(idt, cat_out[,1]), cat_out[,1])

  list_obj[[length(list_obj) + 1]] <- cat_out
  return(list_obj)
}
