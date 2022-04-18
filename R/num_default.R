#' Default summary for a Numeric Row
#' 
#' Summarizes a numeric row using the five-number summary, mean, and standard deviation.
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
#' @seealso Additional prewritten summary functions for numeric data: \link[tangram.pipe]{num_mean_sd}, \link[tangram.pipe]{num_medianiqr}, \link[tangram.pipe]{num_minmax}, \link[tangram.pipe]{num_date}
#' @import dplyr
#' @importFrom  stats complete.cases
#' @importFrom stats aggregate
#' @importFrom stats sd
#' @importFrom stats median
#' @importFrom stats quantile
#' @keywords tangram.pipe
#' @export

num_default <- function(dt, ...){
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
  out <- aggregate(dt[,1],list(dt[,2]),mean)
  out[,2] <- sprintf(rnd, out[,2])
  out <- out %>% t() %>% as.data.frame()
    
  SD <- aggregate(dt[,1],list(dt[,2]),sd)
  SD[,2] <- sprintf(rnd, SD[,2])
  SD <- SD %>% t() %>% as.data.frame()
    
  med <- aggregate(dt[,1],list(dt[,2]),median)
  med[,2] <- sprintf(rnd, med[,2])
  med <- med %>% t() %>% as.data.frame()
    
  Q1 <- aggregate(dt[,1],list(dt[,2]),quantile, probs=.25)
  Q1[,2] <- sprintf(rnd, Q1[,2])
  Q1 <- Q1 %>% t() %>% as.data.frame()
    
  Q3 <- aggregate(dt[,1],list(dt[,2]),quantile, probs=.75)
  Q3[,2] <- sprintf(rnd, Q3[,2])
  Q3 <- Q3 %>% t() %>% as.data.frame()
    
  MIN <- aggregate(dt[,1],list(dt[,2]),min)
  MIN[,2] <- sprintf(rnd, MIN[,2])
  MIN <- MIN %>% t() %>% as.data.frame()
    
  MAX <- aggregate(dt[,1],list(dt[,2]),max)
  MAX[,2] <- sprintf(rnd, MAX[,2])
  MAX <- MAX %>% t() %>% as.data.frame()
    
  out["min",] <- MIN[2,]
  out["Q1",] <- Q1[2,]
  out["median",] <- med[2,]
  out["Q3",] <- Q3[2,]
  out["max",] <- MAX[2,]
  out["mean",] <- out[2,]
  out["SD",] <- SD[2,]
  colnames(out) <- out[1,]
    
  out$Overall <- ""
  out$Overall[3] <- sprintf(rnd, min(dt[,1]))
  out$Overall[4] <- sprintf(rnd, as.numeric(quantile(dt[,1],.25)))
  out$Overall[5] <- sprintf(rnd, median(dt[,1]))
  out$Overall[6] <- sprintf(rnd, as.numeric(quantile(dt[,1],.75)))
  out$Overall[7] <- sprintf(rnd, max(dt[,1]))
  out$Overall[8] <- sprintf(rnd, mean(dt[,1]))
  out$Overall[9] <- sprintf(rnd, sd(dt[,1]))
  out <- out[(3:nrow(out)),]
  out <- data.frame(Measure=rownames(out), out, check.names = FALSE)
  rownames(out) <- NULL
  if (missing == TRUE){
    out <- cbind(Variable="",out)
    out[8,] <- ""
    out$Variable[1] <- rowlabel
    out$Measure[8] <- "Missing"
    for (i in 1:length(miss)){
      out[8,(2+i)] <- miss[i]
    }
    out$Overall[8] <- sum(miss)
  } else {
    out <- cbind(Variable="",out)
    out$Variable[1] <- rowlabel
  }

  if (nocols == TRUE){
    out <- out[,-c(3,4)]
  }
  out
}

