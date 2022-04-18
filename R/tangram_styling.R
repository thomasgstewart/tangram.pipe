#' Tangram Styling
#' 
#' Used to preprocess a `tangram.pipe` table for HTML formatting.
#' @param df The output data frame object to be printed in HTML form.
#' @return A dataframe containing HTML formatting code where applicable.
#' @import dplyr
#' @export

tangram_styling <- function(df){
  leading_white_space <- gsub("^([ ]*).*", "\\1", df[,1])
  leading_white_space <- gsub(" ", "&nbsp;", leading_white_space)
  df[,1] <- paste0(leading_white_space, trimws(df[,1],"left"))
  if (nrow(df) == 1) {
    temp <- as.list(apply(df, 2, function(x) {gsub("=  <", "&le;", x)}))
    df <- as.data.frame(temp)
    colnames(df) <- names(temp)
  } else {
    df <- as.data.frame(apply(df, 2, function(x) {gsub("=  <", "&le;", x)}))
  }
  return(df)
}
