#' Row counter
#'
#' Counts the instances of each column variable of the dataframe to be used in
#' the table (if applicable), and gives an overall row count.
#' @param list_obj the name of the tbl_start object previously initialized.
#' @param col_var the variable to be used in the table columns. Default is from initialized tbl_start object.
#' @param newdata enter new dataset name if different from that initialized in tbl_start.
#' @param missing logical: if TRUE, missing data in the column variable is considered; FALSE only uses complete cases.
#' @param overall logical: if TRUE, an overall column is included.
#' @return A list with the row counts added as a new element to `list_obj`.
#' @import dplyr
#' @keywords tangram.pipe
#' @examples 
#' x <- tbl_start(iris, "Species", missing=TRUE, overall=TRUE, comparison=TRUE) %>%
#'   n_row()
#' @export

n_row <- function(
    list_obj
  , col_var=NULL
  , newdata=FALSE
  , missing=NULL
  , overall=NULL
){
  if (is.null(missing)){
    missing <- list_obj[["missing"]]
  }
  if (is.null(overall)){
    overall <- list_obj[["overall"]]
  }
  if (is.null(col_var)){
    col_var <- list_obj[['col_var']]
  }
  
  if (class(newdata) == 'logical'){
    data <- list_obj[['data']]
  } else {
    data <- newdata
  }
  
  if (!is.null(col_var)){
    data[,col_var] <- as.factor(data[,col_var])
    colN <- data[,col_var] %>%
      table() %>%
      matrix(nrow = 1) %>%
      as.data.frame()
    colnames(colN) <- levels(data[,col_var])
    n_out <- data.frame(Variable = "", Measure = "N", colN, check.names = FALSE)
    
    if (missing == TRUE){
      n_out <- cbind(n_out, Overall = length(data[,col_var]))
    } else 
    {
      n_out <- cbind(n_out, Overall = sum(complete.cases(data[,col_var])))
    }
  } else {
    n_out <- data.frame(Variable = "", Measure = "N", Overall = nrow(data), check.names = FALSE)
  }
  
  if (overall == FALSE){
    n_out <- n_out[,-ncol(n_out)]
  }
  
  n_out[1,] <- as.character(n_out[1,])
  list_obj[[length(list_obj) + 1]] <- n_out
  return(list_obj)
}
