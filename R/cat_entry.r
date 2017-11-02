#' Add a categorical variable summary row to the table
#'
#' This function adds a categorical variable summary row to a table object.
#' @param tbl The table object.  (This input is generally not specified because the tbl object is passed via the \code{\link[magrittr]{\%>\%}} operator.  See examples.)
#' @param x Name of the row categorical variable.  This is the non-standard implementation, so do not quote the name or provide it with a character string object.
#' @param y Name of the column categorical variable.  Do not provide a character string.  Generally, this parameter is specified in the table header and not in this function.
#' @param dt The data.table where the variables x and y reside.  Generally, this parameter is specified in the table header and not in this function.
#' @param xlab Character string.  Text will be the label of the categorical entry in the table.  If not specified, function defaults to \code{label(x)} if available.  As a last resort, the function will print the variable name.
#' @param fun Function which summarizes the \code{table(x, y, usaNA = "always")} object.  Defaults to \code{count_pct} which provides \code{N (pct)}.
#' @param ... Options passed to fun.  These generally include pvalue and fmt.
#' @keywords table
#' @export
#' @examples

cat_entry <- function(
  tbl
  , x
  , y = NULL
  , dt = NULL
  , xlab = NULL
  , fun = NULL
  , ...
){
  if(is.null(substitute(y))){
    if(is.null(tbl[["header"]][["y"]])){
      stop("No column variable provided for table.")
    }else{
      yvar <- tbl[["header"]][["y"]]
    }
  }else{
    yvar <- deparse(substitute(y))
  }

  if(is.null(substitute(dt))){
    if(is.null(tbl[["header"]][["dt"]])){
      stop("No data.table provided for table calculations.")
    }else{
      dtvar <- tbl[["header"]][["dt"]]
    }
  }else{
    dtvar <- deparse(substitute(dt))
  }

  xvar <- deparse(substitute(x))

  browser()
  x <- eval(
      substitute(
          dt[,table(x, y, useNA = "always")]
        , list(dt = as.symbol(dtvar), x = as.symbol(xvar), y = as.symbol(yvar))
      )
    , env = parent.frame()
  )

  x
}

cat_entry(list(),Species,B , dt = dt %>% mutate(B = Sepal.Length<5))


count_pct <- function(x){
  dimt <- dim(x)
  M <- tbl[-dimt[1], -dimt[2]]
  dimm <- dimt-1
  addout <- get_out(dimt[1]+1, 2 + dimm[2] + dimm[2] - 1 + dimm[2])
  dima <- dim(addout)
  addout[-c(1:2), 1] <- "@@" %|% dimnames(M)[[1]]
  addout[-c(1:2), 1:dimm[2] + 2] <- formatpct(M, fmt)
  addout[2, 1] <- if(is.null(xlab)){label(d2[[1]])}else{xlab}
  addout[2, 2] <- "N (%)"
  addout[1,1:dimm[2] + 2] <- dimnames(M)[[2]]

  miss <- formatpct(rbind(colSums(M),tbl[dimt[1],-dimt[2]]))
  addout[1, (dima[2] - dimm[2]+1):dima[2]] <- "Missing: " %|% dimnames(M)[[2]]
  addout[2, (dima[2] - dimm[2]+1):dima[2]] <- miss[2,]

  for(j in 2:dimm[2]){
    M_compare <- M[,c(1,j)]
    addout[1, dimt[2] + j] <- "p-value: " %|%
      dimnames(M)[[2]][1] %|% " vs " %|% dimnames(M)[[2]][j]
    if(sum(M_compare)==0 | !pvalue) next
    E_compare <- rowSums(M_compare) %*% t(colSums(M_compare)) / sum(M_compare)
    smallest_expected_cell <- min(E_compare)

    if(smallest_expected_cell >= 1){
      withCallingHandlers(cst <- chisq.test(M_compare, correct = FALSE), warning = chi_approx)
      stat <- cst$statistic * (sum(M_compare) - 1)/sum(M_compare)
      pval <- pchisq(stat, cst$parameter, lower.tail = FALSE)
      test_method <- "<sup>EP</sup>"
    }else{
      pval <- fisher.test(M_compare)$p.value
      test_method <- "<sup>FE</sup>"
    }

    addout[2, dimt[2] + j] <- formatp(pval, digits = 3) %|% test_method
  }
}
if(length(out)>0) addout <- addout[-1,]
out[[length(out)+1]] <- addout
