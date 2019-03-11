####################################################################
#' Cross tabulation
#' 
#' A cross-tabulation function with output similar to STATA.
#' 
#' @param x,y Vectors. For dependent and independent values
#' @param weight Vector. An optional vector for a weighted cross tabulation
#' @param prow,pcol,pall Boolean. Calculate percent values for rows, columns,
#' or the whole table, respectively.
#' @param decimals Integer. How many decimals should be returned?
#' @param keep_nas Boolean. Keep NAs and count them as well?
#' @export
crosstab <- function(x, y, weight = 1, 
                     prow = FALSE, pcol = FALSE, pall = FALSE, 
                     decimals = 2, keep_nas = TRUE) {
  
  if (keep_nas) {
    x <- replaceall(x, NA, "N/A")
    y <- replaceall(y, NA, "N/A")
  }
  
  if (length(weight) == 1) {
    weight <- rep(as.numeric(weight), length(x))
  }
  
  weight <- as.numeric(weight)
  t <- round(xtabs(weight ~ x + y))
  tt <- data.frame(t) %>% 
    tidyr::spread(y, Freq) %>% .[,-1] %>%
    mutate(total = rowSums(.)) %>%
    t() %>% data.frame() %>% 
    mutate_all(function(x) as.numeric(as.character(x))) %>%
    mutate(Total = rowSums(.)) %>%
    t() %>% data.frame() %>% 
    mutate_all(function(x) as.numeric(as.character(x)))
  ret <- tt
  
  # pall
  if (pall) {
    all <- tt[nrow(tt),ncol(tt)]
    ret <- tt %>% mutate_all(funs(round(100*./all, decimals)))
  }
  
  # prow
  if (prow) {
    all <- tt[,ncol(tt)]
    ret <- tt %>%
      mutate_all(funs(round(100*./all, decimals)))
  }
  
  # pcol
  if (pcol) {
    all <- tt[nrow(tt),]
    ret <- t(apply(tt, 1, function(x) 100*x/as.numeric(all))) %>%
      data.frame() %>% 
      mutate_all(function(x) round(as.numeric(as.character(x)), decimals))
  }
  
  colnames(ret) <- c(colnames(t), "Total")
  rownames(ret) <- c(rownames(t) ,"Total")
  return(ret)
}
