#' Summary for a Categorical Row
#' 
#' Summarizes a categorical row using counts and column proportions.
#' @param dt the name of the dataframe object.
#' @param rowlabels the label for the table row name, if different from row_var.
#' @param missing logical: if TRUE, missing data is considered; FALSE only uses complete cases.
#' @param digits significant digits to use.
#' @import dplyr
#' @keywords tangram.pipe
#' @export

cat_default <- function(dt, rowlabels, missing, digits){
  dt <- filter(dt, !is.na(dt[,2]))
  rnd <- paste0("%.", digits, "f")
  ct <- dt %>%
    table(useNA=ifelse(missing==TRUE, "ifany", "no")) %>%
    as.matrix() %>%
    cbind(Overall=dt %>%
            table(useNA=ifelse(missing==TRUE, "ifany", "no")) %>%
            rowSums())
  prop <- dt %>%
    table(useNA=ifelse(missing==TRUE, "ifany", "no")) %>%
    prop.table(margin=2) %>%
    as.matrix() %>%
    cbind(Overall=dt %>%
            table(useNA=ifelse(missing==TRUE, "ifany", "no")) %>%
            prop.table() %>%
            rowSums())

  out <- matrix(paste0(sprintf(rnd, prop), " (", ct, ")"), nrow=nrow(prop), dimnames=dimnames(prop)) %>%
    as.data.frame() #%>%
    #tibble::rownames_to_column(paste(row_var))
  out <- cbind(rownames(out), out)
  rownames(out) <- NULL
  row1 <- c(paste(rowlabels), rep("", ncol(out)-1))
  out <- rbind(row1, out)
  if (missing == TRUE){
    out[,1] <- gsub("NA.", "Missing", out[,1])
  }
  out <- cbind(out[,1], Measure="", out[,(2:ncol(out))])
  out$Measure[1] <- "Col. Prop. (N)"
  colnames(out)[1] <- "Variable"
  out
}
