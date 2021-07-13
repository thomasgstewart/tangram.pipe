#' Chi-Squared Test for Categorical Variables
#'
#' Default comparison function for categorical data.
#' @param dt the name of the dataframe object.
#' @param digits significant digits to use.
#' @return A dataframe calculating relative entropy between column pairs, as well as an overall chi-squared test across all groups.
#' @importFrom stats chisq.test
#' @importFrom stats pchisq
#' @importFrom stats setNames
#' @export

cat_comp_default <- function(dt, digits){
  fmt <- paste0("%.", digits, "f")
  counts <- table(dt)
  props <- t(counts) / colSums(counts)
  ent <- list()
  name <- c()
  for (i in 2:length(levels(dt[,2]))){
    ent[i-1] <- sprintf(fmt, (log(props[1,] / props[i,]) * props[1,]) %>% sum) 
    name[i-1] <- paste0(levels(dt[,2])[1], " vs. ", levels(dt[,2])[i])
  }

  chisq <- suppressWarnings(chisq.test(table(dt)))
  N <- nrow(dt)
  stat <- ((N-1)/N)*chisq$statistic
  pval <- sprintf(fmt, 1-pchisq(stat, df=chisq$parameter))
  pval <- ifelse(as.numeric(pval)<.0005, "< 0.001", pval)
  
  out <- ent %>% 
    as.data.frame %>% 
    setNames(name) %>% 
    mutate(Total = paste("p = ", pval))

  out <- as.data.frame(cbind(Test="Relative Entropy", out))
  
  return(out)
}
