#' Percentage summary for a Categorical Row
#' 
#' Summarizes a categorical row using counts and column percentages.
#' @param dt the name of the dataframe object.
#' @param rowlabel the label for the table row name, if different from row_var.
#' @param missing logical: if TRUE, missing data is considered; FALSE only uses complete cases.
#' @param digits significant digits to use.
#' @return A dataframe with summary statistics for a categorical variable.
#' @import dplyr
#' @keywords tangram.pipe
#' @export

cat_pct <- function(dt, rowlabel, missing, digits){
  rnd <- paste0("%.", digits, "f")
  nocols <- FALSE
  if (is.null(ncol(dt))){
    nocols <- TRUE
    dt <- data.frame(x = dt) %>% 
      mutate(y= 1:n() %% 2)
  }
  
  dt <- filter(dt, !is.na(dt[,2]))
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
  
  prop <- prop * 100
  
  cols <- unlist(dimnames(prop)[2])
  out <- matrix(paste0(sprintf(rnd, prop), "% (", ct, ")"), 
                nrow=nrow(prop), dimnames = list(NULL,cols)) %>%
    as.data.frame() 
  
  out <- cbind(dimnames(prop)[1], out)
  
  row1 <- c(paste(rowlabel), rep("", ncol(out)-1))
  out <- rbind(row1, out)
  
  if (missing == TRUE){
    out[is.na(out[,1]),1] <- "Missing"
  }
  out <- cbind(out[,1], Measure="", out[,(2:ncol(out))])
  out$Measure[1] <- "Col. Pct. (N)"
  colnames(out)[1] <- "Variable"
  if (nocols == TRUE){
    out <- out[,-c(3,4)]
  }
  out
}
