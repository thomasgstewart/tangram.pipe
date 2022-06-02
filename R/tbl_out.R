#' Output Table
#'
#' Produces a finalized `tangram.pipe` table.
#' @param list_obj the name of the tbl_start object previously initialized.
#' @return A tangram.pipe class object with the finalized table as a dataframe added as the most recent element of `list_obj`.
#' @keywords tangram.pipe
#' @importFrom dplyr bind_rows
#' @examples 
#' iris$color <- sample(c("Blue", "Purple"), size=150, replace=TRUE)
#' iris$Stem.Size <- sample(c("Small", "Medium", "Medium", "Large"), size=150, replace=TRUE)
#' iris$Leaf.Color <- "Green"
#' x <- tbl_start(iris, "Species", missing=TRUE, overall=TRUE, comparison=TRUE) %>%
#'   num_row("Sepal.Length", rowlabel="Sepal Length") %>%
#'   empty_row() %>%
#'   num_row("Sepal.Width", rowlabel="Sepal Width") %>%
#'   empty_row() %>%
#'   num_row("Petal.Length", rowlabel="Petal Length") %>%
#'   empty_row() %>%
#'   num_row("Petal.Width", rowlabel="Petal Width") %>%
#'   empty_row() %>%
#'   cat_row("Stem.Size", rowlabel="Stem Size") %>%
#'   empty_row() %>%
#'   binary_row("color", rowlabel="Color") %>%
#'   tbl_out()
#' @export

tbl_out <- function(
  list_obj
){
  sections <- length(list_obj) - 10
  if (sections == 1){
    out_tbl <- list_obj[[11]]
  } else if (sections > 1) {
    out_tbl <- list_obj[[11]]
    for (i in 2:sections){
      if (all(suppressWarnings(colnames(out_tbl) == colnames(list_obj[[i + 10]])))){
        out_tbl <- rbind(out_tbl, list_obj[[i + 10]])
      } else {
        out_tbl <- bind_rows(out_tbl, list_obj[[i+10]])
        out_tbl[is.na(out_tbl)] <- ""
        out_tbl[is.null(out_tbl)] <- ""
      }
    }
  } else {
    out_tbl <- data.frame()
  }
  return(out_tbl)
}
