#' num_diff
#'
#' Default comparison function for numeric data
#' @param dt
#' @param num_col
#' @param row_var
#' @param digits
#' @keywords tangram.pipe
#' @export


num_diff <- function(dt, num_col, row_var, digits){
  rnd <- paste0("%.", digits, "f")
  test <- (dt %>%
    filter(dt[,2]==sort(unique(dt[,2]))[1]))[row_var] %>%
    t.test((dt %>%
             filter(dt[,2]==sort(unique(dt[,2]))[2]))[row_var])
  diff <- as.numeric(test$estimate[1]-test$estimate[2]) %>%
    round(digits)
  out <- paste0(diff, " (", sprintf(rnd,test$conf.int[1]), ", ", sprintf(rnd,test$conf.int[2]), ")")
  if (num_col > 2){
    for (i in 3:num_col){
      test <- (dt %>%
                 filter(dt[,2]==sort(unique(dt[,2]))[1]))[row_var] %>%
        t.test((dt %>%
                  filter(dt[,2]==sort(unique(dt[,2]))[i]))[row_var])
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
