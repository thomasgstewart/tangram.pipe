#' Date summary for a Numeric Row
#' 
#' Summarizes a numeric row using the five-number summary for a date object.
#' @param dt the name of the dataframe object.
#' @param ... Additional arguments supplied within the package row functions.
#' @return A dataframe with summary statistics for a numeric variable.
#' @details This is an internal function of `tangram.pipe`. Additional arguments 
#' should be supplied for this function to work properly.
#' 
#' `rowlabel` : the label for the table row name, if different from row_var.
#' 
#' `missing` : if TRUE, missing data is considered; FALSE only uses complete cases.
#' @seealso Additional prewritten summary functions for numeric data: \link[tangram.pipe]{num_default}, \link[tangram.pipe]{num_mean_sd}, \link[tangram.pipe]{num_medianiqr}, \link[tangram.pipe]{num_minmax}
#' @import dplyr
#' @importFrom  stats complete.cases
#' @importFrom stats aggregate
#' @importFrom stats sd
#' @importFrom stats median
#' @importFrom stats quantile
#' @keywords tangram.pipe
#' @export

num_date <- function(dt, ...){
  dots <- list(...)
  rowlabel <- dots$rowlabel
  missing <- dots$missing
  nocols <- FALSE
  if (is.null(ncol(dt))) {
    nocols <- TRUE
    dt <- data.frame(x = dt) %>% mutate(y = 1:n()%%2)
  }
  if (missing == TRUE) {
    miss <- dt %>% filter(is.na(dt[, 1]))
    miss <- miss[, 2] %>% table() %>% as.data.frame() %>% 
      t()
    miss <- if (dim(miss)[1] >= 2) 
      as.numeric(miss[2, ])
    else 0
  }
  dt <- dt[complete.cases(dt), ]
  dt[,1] <- as.Date(dt[,1])
  out <- aggregate(dt[, 1], list(dt[, 2]), median) %>% 
    t() %>% 
    as.data.frame()
  Q1 <- aggregate(dt[, 1], list(dt[, 2]), quantile, probs = 0.25, type = 1) %>% 
    t() %>% 
    as.data.frame()
  Q3 <- aggregate(dt[, 1], list(dt[, 2]), quantile, probs = 0.75, type = 1) %>% 
    t() %>% 
    as.data.frame()
  MIN <- aggregate(dt[, 1], list(dt[, 2]), min) %>% t() %>% as.data.frame()
  MAX <- aggregate(dt[, 1], list(dt[, 2]), max) %>% t() %>% as.data.frame()
  out["min", ] <- MIN[2, ]
  out["Q1", ] <- Q1[2, ]
  out["median", ] <- out[2, ]
  out["Q3", ] <- Q3[2, ]
  out["max", ] <- MAX[2, ]
  
  colnames(out) <- out[1, ]
  out$Overall <- ""
  out$Overall[3] <- min(dt[, 1]) %>% as.character()
  out$Overall[4] <- quantile(dt[, 1], 0.25, type = 1) %>% as.character
  out$Overall[5] <- median(dt[, 1]) %>% as.character
  out$Overall[6] <- quantile(dt[, 1],0.75, type = 1) %>% as.character
  out$Overall[7] <- max(dt[, 1]) %>% as.character
  out <- out[(3:nrow(out)), ]
  out <- data.frame(Measure = rownames(out), out, check.names = FALSE)
  rownames(out) <- NULL
  if (missing == TRUE) {
    out <- cbind(Variable = "", out)
    out[6, ] <- ""
    out$Variable[1] <- rowlabel
    out$Measure[6] <- "Missing"
    for (i in 1:length(miss)) {
      out[6, (2 + i)] <- miss[i]
    }
    out$Overall[6] <- sum(miss)
  }
  else {
    out <- cbind(Variable = "", out)
    out$Variable[1] <- rowlabel
  }
  if (nocols == TRUE) {
    out <- out[, -c(3, 4)]
  }
  out
}
