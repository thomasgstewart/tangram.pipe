binary_diff <- function(dt, num_col, reference, digits){
  rnd <- paste0("%.",digits,"f")
  dt <- dt[complete.cases(dt),]
  var1 <- sort(unique(dt[,2]))[1]
  var2 <- sort(unique(dt[,2]))[2]
  n1 <- sum(dt[,2]==var1)
  n2 <- sum(dt[,2]==var2)
  x1 <- dt %>%
    filter(dt[,2]==var1 & dt[,1]==reference) %>%
    nrow()
  x2 <- dt %>%
    filter(dt[,2]==var2 & dt[,1]==reference) %>%
    nrow()
  test <- prop.test(x=c(x1, x2), n=c(n1, n2))
  diff <- as.numeric(test$estimate[1]-test$estimate[2]) %>%
    round(digits)
  out <- paste0(diff, " (", sprintf(rnd,test$conf.int[1]), ", ", sprintf(rnd,test$conf.int[2]), ")")
  if (num_col > 2){
    for (i in 3:num_col){
      varI <- sort(unique(dt[,2]))[i]
      nI <- sum(dt[,2]==varI)
      xI <- dt %>%
        filter(dt[,2]==varI & dt[,1]==reference) %>%
        nrow()
      test <- prop.test(x=c(x1, xI), n=c(n1, nI))
      diff <- as.numeric(test$estimate[1]-test$estimate[2]) %>%
        round(digits)
      out2 <- paste0(diff, " (", sprintf(rnd,test$conf.int[1]), ", ", sprintf(rnd,test$conf.int[2]), ")")
      out <- data.frame(out, out2)
      colnames(out)[i-1] <- paste0("Compare: ", sort(unique(dt[,2]))[i])
    }
    colnames(out)[1] <- paste0("Compare: ", sort(unique(dt[,2]))[2])
  }
  out
}

