cat_comp_default <- function(dt){
  chisq <- chisq.test(table(dt))
  N <- nrow(dt)
  stat <- round(((N-1)/N)*chisq$statistic,2)
  pvalue <- round(1-pchisq(stat, df=chisq$parameter),2)
  out <- paste0(stat, " (", pvalue, ")")
}