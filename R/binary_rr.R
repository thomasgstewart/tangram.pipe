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
  rnd <- paste0("%.", digits, "f")
  A <- sum(dt[,1] == reference & dt[,2] == sort(unique(dt[,2]))[1])
  B <- sum(dt[,1] == reference & dt[,2] == sort(unique(dt[,2]))[2])
  C <- sum(dt[,1] != reference & dt[,2] == sort(unique(dt[,2]))[1])
  D <- sum(dt[,1] != reference & dt[,2] == sort(unique(dt[,2]))[2])
  pt_est <- (A/(A+B))/(C/(C+D))
  logSE <- sqrt(1/A + 1/C + (1/(A+B)) + (1/(C+D)))
  LB <- sprintf(rnd, exp(log(pt_est)-1.96*logSE))
  UB <- sprintf(rnd, exp(log(pt_est)+1.96*logSE))
  pt_est <- sprintf(rnd, pt_est)
  RR <- paste0(pt_est, " (", LB, ", ", UB, ")")
  if (num_col > 2){
    for (i in 3:num_col){
      B <- sum(dt[,1] == reference & dt[,2] == sort(unique(dt[,2]))[i])
      D <- sum(dt[,1] != reference & dt[,2] == sort(unique(dt[,2]))[i])
      pt_est <- (A/(A+B))/(C/(C+D))
      logSE <- sqrt(1/A + 1/C + (1/(A+B)) + (1/(C+D)))
      LB <- sprintf(rnd, exp(log(pt_est)-1.96*logSE))
      UB <- sprintf(rnd, exp(log(pt_est)+1.96*logSE))
      pt_est <- sprintf(rnd, pt_est)
      RR2 <- paste0(pt_est, " (", LB, ", ", UB, ")")
      RR <- data.frame(RR, RR2)
      colnames(RR)[i-1] <- paste0("Compare: ", sort(unique(dt[,2]))[i])
    }
    colnames(RR)[1] <- paste0("Compare: ", sort(unique(dt[,2]))[2])
  }
  RR
}
