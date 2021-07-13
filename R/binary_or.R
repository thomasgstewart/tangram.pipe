#' Binary Odds Ratio
#'
#' Calculates odds ratio across categories for binary data.
#' @param dt the name of the dataframe object.
#' @param num_col the number of categorical columns in the data.
#' @param reference the name of the reference row category to use.
#' @param digits significant digits to use.
#' @return A dataframe with computed odds ratios between pairs of columns for binary data, as well as an overall chi-squared test across all groups.
#' @importFrom stats glm
#' @importFrom stats relevel
#' @importFrom stats setNames
#' @keywords tangram.pipe
#' @export

binary_or <- function(dt, num_col, reference, digits){
  rnd <- paste0("%.", digits, "f")
  dt[,1] <- as.factor(dt[,1])
  dt[,2] <- as.factor(dt[,2])
  if (reference != levels(dt[,1])[1]){
    dt[,1] <- relevel(dt[,1], ref=reference)
  }
  model <- summary(glm(dt[,1]~dt[,2], data=dt, family="binomial"))
  OR <- list()
  name <- c()
  for (i in 2:num_col){
    logOR <- model$coefficients[i,1]
    logSE <- model$coefficients[i,2]
    pt_est <- sprintf(rnd, exp(logOR))
    LB <- sprintf(rnd, exp(logOR - 1.96*logSE))
    UB <- sprintf(rnd, exp(logOR + 1.96*logSE))
    OR[[i-1]] <- paste0(pt_est, " (", LB, ", ", UB, ")")
    name[i-1] <- paste0(levels(dt[,2])[1], " vs. ", levels(dt[,2])[i])
  }
  
  chisq <- suppressWarnings(chisq.test(table(dt)))
  N <- nrow(dt)
  stat <- ((N-1)/N)*chisq$statistic
  pval <- sprintf(rnd, 1-pchisq(stat, df=chisq$parameter))
  pval <- ifelse(as.numeric(pval)<.0005, "< 0.001", pval)

  out <- OR %>% 
    as.data.frame %>% 
    setNames(name) %>% 
    mutate(Total = paste("p = ", pval))
  
  out <- as.data.frame(cbind(Test="Odds Ratio", out))
  
  out
}

