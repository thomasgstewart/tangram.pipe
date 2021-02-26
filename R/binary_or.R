binary_or <- function(dt, num_col, reference, digits){
  rnd <- paste0("%.", digits, "f")
  model <- summary(glm(dt[,1]~dt[,2], data=dt, family="binomial"))
  logOR <- model$coefficients[2,1]
  if (reference != sort(unique(dt[,1]))[1]){
    logOR <- -logOR
  }
  logSE <- model$coefficients[2,2]
  pt_est <- sprintf(rnd, exp(logOR))
  LB <- sprintf(rnd, exp(logOR - 1.96*logSE))
  UB <- sprintf(rnd, exp(logOR + 1.96*logSE))
  OR <- paste0(pt_est, " (", LB, ", ", UB, ")")
  if (num_col > 2){
    for (i in 3:num_col){
      logOR <- model$coefficients[i,1]
      if (reference != sort(unique(dt[,1]))[1]){
        logOR <- -logOR
      }
      logSE <- model$coefficients[i,2]
      pt_est <- sprintf(rnd, exp(logOR))
      LB <- sprintf(rnd, exp(logOR - 1.96*logSE))
      UB <- sprintf(rnd, exp(logOR + 1.96*logSE))
      OR2 <- paste0(pt_est, " (", LB, ", ", UB, ")")
      OR <- data.frame(OR, OR2)
      colnames(OR)[i-1] <- paste0("OR: ", sort(unique(dt[,2]))[i])
    }
    colnames(OR)[1] <- paste0("OR: ", sort(unique(dt[,2]))[2])
  }
  OR
}