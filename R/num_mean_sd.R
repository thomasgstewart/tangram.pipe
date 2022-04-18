#' Mean/SD summary for a Numeric Row
#' 
#' Summarizes a numeric row using the mean and standard deviation.
#' @param dt the name of the dataframe object.
#' @param ... Additional arguments supplied within the package row functions.
#' @return A dataframe with summary statistics for a numeric variable.
#' @details This is an internal function of `tangram.pipe`. Additional arguments 
#' should be supplied for this function to work properly.
#' 
#' `rowlabel` : the label for the table row name, if different from row_var.
#' 
#' `missing` : if TRUE, missing data is considered; FALSE only uses complete cases.
#' 
#' `digits` : significant digits to use.
#' @seealso Additional prewritten summary functions for numeric data: \link[tangram.pipe]{num_default}, \link[tangram.pipe]{num_medianiqr}, \link[tangram.pipe]{num_minmax}, \link[tangram.pipe]{num_date}
#' @import dplyr
#' @importFrom  stats complete.cases
#' @importFrom stats aggregate
#' @importFrom stats sd
#' @importFrom stats median
#' @importFrom stats quantile
#' @keywords tangram.pipe
#' @export

num_mean_sd <- function(dt, ...){
  dots <- list(...)
  rowlabel <- dots$rowlabel
  missing <- dots$missing
  digits <- dots$digits
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
  avg <- aggregate(dt[,1],list(dt[,2]),mean)
  avg[,2] <- sprintf(rnd, avg[,2])
  avg <- avg %>% t() %>% as.data.frame()
  
  SD <- aggregate(dt[,1],list(dt[,2]),sd)
  SD[,2] <- sprintf(rnd, SD[,2])
  SD <- SD %>% t() %>% as.data.frame()
  
  out <- avg
  out["meanSD",] <- paste0(avg[2,], " (", SD[2,], ")")
  colnames(out) <- out[1,]
  out <- out["meanSD",]
  
  out$Overall <- ""
  out$Overall[1] <- paste0(sprintf(rnd, mean(dt[,1])),
                           " (",
                           sprintf(rnd, sd(dt[,1])), 
                           ")")
  out <- data.frame(Measure="Mean (Std. Dev.)", out, check.names = FALSE)
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
