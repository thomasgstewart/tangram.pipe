#' Binary Risk Ratio
#'
#' Calculates risk ratio across categories for binary data
#' @param dt the name of the dataframe object.
#' @param num_col num_col the number of categorical columns in the data.
#' @param reference the name of the reference row category to use.
#' @param digits significant digits to use.
#' @importFrom stats complete.cases
#' @keywords tangram.pipe
#' @export

binary_rr <- function(dt, num_col, reference, digits){
  dt <- dt[complete.cases(dt),]
  dt[,1] <- as.factor(dt[,1])
  dt[,2] <- as.factor(dt[,2])
  if (reference != levels(dt[,1])[1]){
    dt[,1] <- relevel(dt[,1], ref=reference)
  }
  rnd <- paste0("%4.", digits, "f (%4.", digits, "f, %4.", digits, "f)")
  RR_cal <- function(a,b,c,d){
    pt_est <- (A/(A+B))/(C/(C+D))
    logSE <- sqrt(1/A + 1/C + (1/(A+B)) + (1/(C+D)))
    c(pt_est, exp(log(pt_est)-1.96*logSE), exp(log(pt_est)+1.96*logSE))
  }
  fmt <- function(dm) {sprintf(rnd, dm[1], dm[2], dm[3])}
  
  RR <- list()
  name <- c()
  for (i in 2:num_col){
    A <- sum(dt[,1] == reference & dt[,2] == sort(unique(dt[,2]))[1])
    B <- sum(dt[,1] == reference & dt[,2] == sort(unique(dt[,2]))[i])
    C <- sum(dt[,1] != reference & dt[,2] == sort(unique(dt[,2]))[1])
    D <- sum(dt[,1] != reference & dt[,2] == sort(unique(dt[,2]))[i])
    
    RR[[i-1]] <- RR_cal(A,B,C,D) %>%
      fmt
    name[i-1] <- paste0(names(dt2[1]), " vs. ", names(dt2[i]))
  }
  rnd2 <- paste0("%.", digits, "f")
  chisq <- suppressWarnings(chisq.test(table(dt)))
  N <- nrow(dt)
  stat <- ((N-1)/N)*chisq$statistic
  pval <- sprintf(rnd2, 1-pchisq(stat, df=chisq$parameter))
  pval <- ifelse(as.numeric(pval)<.0005, "<.001", pval)
  out <- RR %>% 
    as.data.frame %>% 
    setNames(name) %>% 
    mutate(Total = paste("p = ", pval))
  out
}
