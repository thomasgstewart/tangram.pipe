#' JAMA-style summary for a Binary Row
#' 
#' Summarizes a binary row using column percentages and the total number in each cell divided by the column total. This is the style used by the Journal of the American Medical Association.
#' @param dt the name of the dataframe object.
#' @param ... Additional arguments supplied within the package row functions.
#' @return A dataframe with summary statistics for a binary variable.
#' @details This is an internal function of `tangram.pipe`. Additional arguments 
#' should be supplied for this function to work properly.
#' 
#' `reference` : the name of the row category to use as the reference. Default will use alphabetical first category
#' 
#' `ref.label` : choice of whether you want the reference label to be in the table. Default is `on` and includes reference label; `off` switches it off.
#' 
#' `rowlabel` : the label for the table row name, if different from row_var.
#' 
#' `compact` : if TRUE, data displayed in one row.
#' 
#' `missing` : if TRUE, missing data is considered; FALSE only uses complete cases.
#' 
#' `digits` : significant digits to use.
#' @seealso Possible summary functions for binary data:\link[tangram.pipe]{binary_default}, \link[tangram.pipe]{binary_pct}, \link[tangram.pipe]{binary_count}
#' @import dplyr
#' @keywords tangram.pipe
#' @export

binary_jama <- function(dt, ...){
  dots <- list(...)
  reference <- dots$reference
  ref.label <- dots$ref.label
  rowlabel <- dots$rowlabel
  compact <- dots$compact
  missing <- dots$missing
  digits <- dots$digits
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
  
  total.ct <- rep(colSums(ct), nrow(ct)) %>%
    matrix(ncol = nrow(ct)) %>%
    t()
  
  prop <- dt %>%
    table(useNA=ifelse(missing==TRUE, "ifany", "no")) %>%
    prop.table(margin=2) %>%
    as.matrix() %>%
    cbind(Overall=dt %>%
            table(useNA=ifelse(missing==TRUE, "ifany", "no")) %>%
            prop.table() %>%
            rowSums())
  
  prop <- prop * 100
  
  out <- matrix(paste0(sprintf(rnd, prop), " (", sprintf("%1.0f/%1.0f", ct, total.ct), ")")
                , nrow=nrow(prop), dimnames=dimnames(prop)) %>%
    as.data.frame()
  out <- cbind(dimnames(prop)[[1]], out)
  rownames(out) <- NULL
  out[is.na(out)] <- "NA."
  row1 <- c(paste(rowlabel), rep("", ncol(out)-1))
  out <- rbind(row1, out)
  
  if (missing == TRUE){
    out <- out %>% filter(out[,1]==reference | out[,1]=="NA." | out[,1]==rowlabel)
    out[,1] <- gsub("NA.", "Missing", out[,1])
  } else {
    out <- out %>% filter(out[,1]==reference | out[,1]==rowlabel)
  }
  out <- cbind(out[,1], Measure="", out[,(2:ncol(out))])
  if (compact == TRUE){
    out$Measure[2] <- "Pct. (n/N)"
    if (ref.label != "off") {out[2,1] <- paste0(out[1,1], ": ", out[2,1])} else {out[2,1] <- rowlabel}
    out <- out[-1,]
  } else {
    out$Measure[1] <- "Pct. (n/N)"
  }
  colnames(out)[1] <- "Variable"
  
  if (nocols == TRUE){
    out <- out[,-c(3,4)]
  }
  rownames(out) <- NULL
  out
}
