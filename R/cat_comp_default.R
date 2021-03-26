cat_comp_default <- function(dt, digits){
  fmt <- paste0("%.", digits, "f")
  counts <- table(dt)
  props <- t(counts) / colSums(counts)
  ent <- list()
  name <- c()
  for (i in 2:length(levels(dt[,2]))){
    ent[i-1] <- sprintf(fmt, (log(props[1,] / props[i,]) * props[1,]) %>% sum) 
    name[i-1] <- paste0(levels(dt[,2])[1], " vs. ", levels(dt[,2])[i])
  }

  chisq <- suppressWarnings(chisq.test(table(dt)))
  N <- nrow(dt)
  stat <- ((N-1)/N)*chisq$statistic
  pval <- sprintf(fmt, 1-pchisq(stat, df=chisq$parameter))
  pval <- ifelse(as.numeric(pval)<.0005, "<.001", pval)
  
  out <- ent %>% 
    as.data.frame %>% 
    setNames(name) %>% 
    mutate(Total = paste("p = ", pval))
  
  out
}
