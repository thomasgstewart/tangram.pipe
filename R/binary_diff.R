#' Binary Difference in Proportions
#'
#' Default comparison function for binary data.
#' @param dt the name of the dataframe object.
#' @param num_col the number of categorical columns in the data.
#' @param reference the name of the reference row category to use.
#' @param digits significant digits to use.
#' @return A dataframe with difference in proportions test results between pairs of columns for binary data, as well as an overall chi-squared test across all groups. 
#' @import dplyr
#' @importFrom stats complete.cases
#' @importFrom stats prop.test
#' @importFrom stats setNames
#' @keywords tangram.pipe
#' @export

binary_diff <- function(dt, num_col, reference, digits){
  diffprop <- function(pt){ c(-diff(pt$estimate),  pt$conf.int) }
  rnd <- paste0("%4.", digits, "f (%4.", digits, "f, %4.", digits, "f)")
  fmt <- function(dm) {sprintf(rnd, dm[1], dm[2], dm[3])}
  
  dt2 <- split(dt,dt[,2])
  k <- num_col
  dt3 <- list()
  name <- c()
  for(i in 2:k){
    x1 <- sum(dt2[[1]][,1]==reference, na.rm=TRUE)
    x2 <- sum(dt2[[i]][,1]==reference, na.rm=TRUE)
    n1 <- nrow(dt2[[1]])
    n2 <- nrow(dt2[[i]])
    dt3[[i-1]] <- prop.test(x=c(x1,x2), n=c(n1,n2)) %>% 
      diffprop %>% 
      fmt
    
    name[i-1] <- paste0(names(dt2[1]), " vs. ", names(dt2[i]))
  }
  
  rnd2 <- paste0("%.", digits, "f")
  chisq <- suppressWarnings(chisq.test(table(dt)))
  N <- nrow(dt)
  stat <- ((N-1)/N)*chisq$statistic
  pval <- sprintf(rnd2, 1-pchisq(stat, df=chisq$parameter))
  pval <- ifelse(as.numeric(pval)<.0005, "< 0.001", pval)
  out <- dt3 %>% 
    as.data.frame %>% 
    setNames(name) %>% 
    mutate(Total = paste("p = ", pval))
  
  out <- as.data.frame(cbind(Test="Difference in Proportions", out))
  
  out
}
