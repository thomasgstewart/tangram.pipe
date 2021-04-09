#' Printing a Table
#'
#' Prints a finished table
#' @param x the name of the tbl_start object previously initialized.
#' @param ... further arguments passed to or from other methods
#' @keywords tangram.pipe
#' @method print tangram.pipe
#' @examples 
#' iris$color <- sample(c("Blue", "Purple"), size=150, replace=TRUE)
#' iris$Stem.Size <- sample(c("Small", "Medium", "Medium", "Large"), size=150, replace=TRUE)
#' iris$Leaf.Color <- "Green"
#' x <- tbl_start(iris, "Species", missing=TRUE, overall=TRUE, comparison=TRUE) %>%
#'   num_row("Sepal.Length", rowlabels="Sepal Length") %>%
#'   empty_row() %>%
#'   num_row("Sepal.Width", rowlabels="Sepal Width") %>%
#'   empty_row() %>%
#'   num_row("Petal.Length", rowlabels="Petal Length") %>%
#'   empty_row() %>%
#'   num_row("Petal.Width", rowlabels="Petal Width") %>%
#'   empty_row() %>%
#'   cat_row("Stem.Size", rowlabels="Stem Size") %>%
#'   empty_row() %>%
#'   binary_row("color", rowlabels="Color") %>%
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