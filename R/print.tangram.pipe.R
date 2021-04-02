#' Printing a Table
#'
#' Prints a finished table
#' @param list_obj the name of the tbl_start object previously initialized.
#' @keywords tangram.pipe
#' @method print tangram.pipe
#' @examples 
#' x <- tbl_start(iris2, "Species", missing=TRUE, overall=TRUE, comparison=TRUE) %>%
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
  list_obj
){
  idx <- which(vapply(list_obj, function(x){"out" %in% class(x)}, TRUE))
  if (length(idx) > 1){
    idx <- idx[1]
  }
  print(list_obj[[idx]])
}