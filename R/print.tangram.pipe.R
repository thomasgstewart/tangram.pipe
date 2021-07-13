#' Printing a Table
#'
#' Prints a finished table created from tangram.pipe.
#' @param x the name of the tbl_start object previously initialized.
#' @param ... further arguments passed to or from other methods.
#' @return A dataframe object containing the information from the last element of a tangram.pipe class object created using `tbl_out()`.  This is the finalized table object.
#' @keywords tangram.pipe
#' @method print tangram.pipe
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
#'   tbl_out() %>%
#'   print()
#' @export

print.tangram.pipe <- function(
  x, ...
){
  idx <- which(vapply(x, function(x){"out" %in% class(x)}, TRUE))
  if (length(idx) > 1){
    idx <- idx[1]
  }
  print(x[[idx]])
}