binary_default <- function(dt, reference, row_var, rowlabels, missing, digits){
  dt <- filter(dt, !is.na(dt[,2]))
  rnd <- paste0("%.", digits, "f")
  ct <- dt %>%
    table(useNA=ifelse(missing==TRUE, "ifany", "no")) %>%
    as.matrix() %>%
    cbind(Overall=dt %>%
            table(useNA=ifelse(missing==TRUE, "ifany", "no")) %>%
            rowSums())
  prop <- dt %>%
    table(useNA=ifelse(missing==TRUE, "ifany", "no")) %>%
    prop.table(margin=2) %>%
    round(digits) %>%
    as.matrix() %>%
    cbind(Overall=dt %>%
            table(useNA=ifelse(missing==TRUE, "ifany", "no")) %>%
            prop.table() %>%
            round(digits) %>%
            rowSums())

  out <- matrix(paste0(sprintf(rnd, prop), " (", ct, ")"), nrow=nrow(prop), dimnames=dimnames(prop)) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(paste(row_var))
  row1 <- c(paste(rowlabels), rep("", ncol(out)-1))
  out <- rbind(row1, out)
  if (missing == TRUE){
    out <- out %>% filter(out[,1]==reference | out[,1]=="NA." | out[,1]==rowlabels)
    out[,1] <- gsub("NA.", "Missing", out[,1])
  } else {
    out <- out %>% filter(out[,1]==reference | out[,1]==rowlabels)
  }
  out <- cbind(out[,1], Measure="", out[,(2:ncol(out))])
  out$Measure[1] <- "Col. Prop. (N)"
  out
}
