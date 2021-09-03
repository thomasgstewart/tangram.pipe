#' Min-Max summary for a Numeric Row
#' 
#' Summarizes a numeric row using the minimum and maximum values.
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

num_minmax <- function(dt, rowlabel, missing, digits){
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
    miss <- if (dim(miss)[1] >= 2) as.numeric(miss[2,]) else 0
  }
  
  dt <- dt[complete.cases(dt),]
  MIN <- aggregate(dt[,1],list(dt[,2]),min)
  MIN[,2] <- sprintf(rnd, MIN[,2])
  MIN <- MIN %>% t() %>% as.data.frame()
  
  MAX <- aggregate(dt[,1],list(dt[,2]),max)
  MAX[,2] <- sprintf(rnd, MAX[,2])
  MAX <- MAX %>% t() %>% as.data.frame()
  
  out <- MIN
  out["minmax",] <- paste0(MIN[2,], "\u2013", MAX[2,])
  colnames(out) <- out[1,]
  out <- out["minmax",]
  
  out$Overall <- ""
  out$Overall[1] <- paste0(sprintf(rnd, min(dt[,1])), 
                           "\u2013", 
                           sprintf(rnd,max(dt[,1])))
  out <- data.frame(Measure="Min \u2013 Max", out)
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
