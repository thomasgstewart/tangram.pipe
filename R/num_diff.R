#' Numeric Difference in Means
#'
#' Default comparison function for numeric data.
#' @param dt the name of the dataframe object.
#' @param num_col the number of categorical columns in the data.
#' @param row_var the name of the row variable in the data.
#' @param digits significant digits to use.
#' @return A dataframe calculating the difference in means between column pairs, as well as an overall one-way ANOVA across all groups.
#' @import dplyr
#' @importFrom stats t.test
#' @importFrom stats anova
#' @importFrom stats lm
#' @importFrom stats setNames
#' @keywords tangram.pipe
#' @export

num_diff <- function(dt, num_col, row_var, digits){
  diffmeans <- function(tt){ c(-diff(tt$estimate),  tt$conf.int) }
  rnd <- paste0("%4.", digits, "f (%4.", digits, "f, %4.", digits, "f)")
  fmt <- function(dm) {sprintf(rnd, dm[1], dm[2], dm[3])}
  
  dt2 <- split(dt,dt[,2])
  k <- num_col
  dt3 <- list()
  name <- c()
  for(i in 2:k){
    if (is.na(sd(dt2[[1]][,1], na.rm=TRUE)) | is.na(sd(dt2[[i]][,1], na.rm=TRUE))){
      dt3[[i-1]] <- ""
    } else {
      dt3[[i-1]] <- t.test(dt2[[1]][,1], dt2[[i]][,1]) %>% 
        diffmeans %>% 
        fmt
    }
    
    name[i-1] <- paste0(names(dt2[1]), " vs. ", names(dt2[i]))
  }
  pval <- anova(lm(dt[,1]~dt[,2]))[[5]][1] %>% round(digits)
  pval <- ifelse(pval<.0005, "< 0.001", pval)
  out <- dt3 %>% 
    as.data.frame %>% 
    setNames(name) %>% 
    mutate(Total = paste("p = ", pval))
  
  out <- as.data.frame(cbind(Test="Difference in Means", out))
  
  out
}
