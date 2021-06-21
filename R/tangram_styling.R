#' Tangram Styling
#' 
#' Used to preprocess a tangram.pipe table for HTML formatting
#' @param df The output data frame object to be printed in HTML form.
#' @export

tangram_styling <- function(df){
  df[,1] <- gsub(" ", "&nbsp;", df[,1])
  df <- as.data.frame(apply(df, 2, function(x) {gsub("=  <", "&le;", x)}))
  return(df)
}