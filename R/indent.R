#' Indentation
#' 
#' Used to put indentation for categorical row categories for HTML-style table
#' @param df The output data frame object to be printed in HTML form
#' @param spaces The number of spaces for the indentation
#' @export

indent <- function(df, spaces){
  if ("data.frame" %in% class(df)){
    class(df) <- "data.frame"
    idt <- paste(rep("&nbsp;", spaces), collapse="")
    df[,1] <- ifelse(df[,2]=="" & df[,1] != "", paste0(idt, df[,1]), df[,1])
    return(df)
  } else {
    warning("Error: 'indent' requires a data frame object")
  }
}