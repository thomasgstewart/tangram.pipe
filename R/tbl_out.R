#' Output Table
#'
#' Produces a finalized table
#' @param list_obj the name of the tbl_start object previously initialized.
#' @keywords tangram.pipe
#' @importFrom dplyr bind_rows
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
#'   tbl_out()
#' @export

tbl_out <- function(
  list_obj
){
  sections <- length(list_obj) - 6
  if (sections == 1){
    out_tbl <- list_obj[[7]]
  } else if (sections > 1) {
    out_tbl <- list_obj[[7]]
    for (i in 2:sections){
      if (all(suppressWarnings(colnames(out_tbl) == colnames(list_obj[[i + 6]])))){
        out_tbl <- rbind(out_tbl, list_obj[[i + 6]])
      } else {
        out_tbl <- bind_rows(out_tbl, list_obj[[i+6]])
        out_tbl[is.na(out_tbl)] <- ""
        out_tbl[is.null(out_tbl)] <- ""
      }
    }
  } else {
    out_tbl <- data.frame()
  }
  list_obj[[length(list_obj) + 1]] <- out_tbl
  class(list_obj[[length(list_obj)]]) <- c("data.frame", "out")
  class(list_obj) <- "tangram.pipe"
  return(list_obj)
}