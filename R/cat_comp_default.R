#' Chi-Squared Test for Categorical Variables
#'
#' Default comparison function for categorical data
#' @param dt the name of the dataframe object
#' @param digits significant digits to use.
#' @importFrom stats chisq.test
#' @importFrom stats pchisq
#' @export


cat_comp_default <- function(dt, digits){
  rnd <- paste0("%.", digits, "f")
  chisq <- suppressWarnings(chisq.test(table(dt)))
  N <- nrow(dt)
  stat <- round(((N-1)/N)*chisq$statistic,2)
  pvalue <- sprintf(rnd, 1-pchisq(stat, df=chisq$parameter))
  out <- paste0(sprintf(rnd, stat), " (", pvalue, ")")
}
