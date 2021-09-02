#' Median/IQR summary for a Numeric Row
#' 
#' Summarizes a numeric row using the median and interquartile range.
#' @param dt the name of the dataframe object.
#' @param rowlabel the label for the table row name, if different from row_var.
#' @param missing logical: if TRUE, missing data is considered; FALSE only uses complete cases.
#' @param digits significant digits to use.
#' @return A dataframe with summary statistics for a numeric variable.
#' @import dplyr
#' @importFrom  stats complete.cases
#' @importFrom stats aggregate
#' @importFrom stats sd
#' @importFrom stats median
#' @importFrom stats quantile
#' @keywords tangram.pipe
#' @export

num_medianiqr <- function(dt, rowlabel, missing, digits){
  rnd <- paste0("%.", digits, "f")
  
  nocols <- FALSE
  if (is.null(ncol(dt))){
    nocols <- TRUE
    dt <- data.frame(x = dt) %>% 
      mutate(y= 1:n() %% 2)
  }
  
  if (missing == TRUE){
    miss <- dt %>% filter(is.na(dt[,1]))
    miss <- miss[,2] %>% table() %>% as.data.frame() %>% t()
    miss <- as.numeric(miss[2,])
  }
  
  dt <- dt[complete.cases(dt),]
  med <- aggregate(dt[,1],list(dt[,2]),median)
  med[,2] <- sprintf(rnd, med[,2])
  med <- med %>% t() %>% as.data.frame()
  
  Q1 <- aggregate(dt[,1],list(dt[,2]),quantile, probs=.25)
  Q1[,2] <- sprintf(rnd, Q1[,2])
  Q1 <- Q1 %>% t() %>% as.data.frame()
  
  Q3 <- aggregate(dt[,1],list(dt[,2]),quantile, probs=.75)
  Q3[,2] <- sprintf(rnd, Q3[,2])
  Q3 <- Q3 %>% t() %>% as.data.frame()
  
  out <- med
  out["medianiqr",] <- paste0(med[2,], " (", Q1[2,], "\u2013", Q3[2,], ")")
  colnames(out) <- out[1,]
  out <- out["medianiqr",]
  
  out$Overall <- ""
  out$Overall[1] <- paste0(sprintf(rnd, median(dt[,1])),
                           " (",
                           sprintf(rnd, quantile(dt[,1], probs = 0.25)), 
                           "\u2013", 
                           sprintf(rnd, quantile(dt[,1], probs = 0.75)),
                           ")")
  out <- data.frame(Measure="Median (IQR)", out)
  rownames(out) <- NULL
  
  if (missing == TRUE){
    out <- cbind(Variable="",out)
    out[2,] <- ""
    out$Variable[1] <- rowlabel
    out$Measure[2] <- "Missing"
    for (i in 1:length(miss)){
      out[2,(2+i)] <- miss[i]
    }
    out$Overall[2] <- sum(miss)
  } else {
    out <- cbind(Variable="",out)
    out$Variable[1] <- rowlabel
  }
  
  if (nocols == TRUE){
    out <- out[,-c(3,4)]
  }
  return(out)
}
