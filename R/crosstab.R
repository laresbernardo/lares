####################################################################
#' Weighted Cross Tabulation
#' 
#' A cross-tabulation function with output similar to STATA, tidy
#' friendly, with weights if needed. If only one dependent variable
#' crossval() will be recommended; if two, then both variables will be 
#' crossed; if three, both variables will be crossed with third as 
#' weighted values.
#' 
#' @param df Data.frame. 
#' @param ... Variables. Dependent and independent variables. If needed,
#' third value should be the weight variable.
#' @param prow,pcol,pall Boolean. Calculate percent values for rows, columns,
#' or the whole table, respectively.
#' @param decimals Integer. How many decimals should be returned?
#' @param keep_nas Boolean. Keep NAs and count them as well?
#' @param total Boolean. Return total values column?
#' @export
crosstab <- function(df, ..., 
                     prow = FALSE, pcol = FALSE, pall = FALSE, 
                     decimals = 2, keep_nas = TRUE,
                     total = TRUE) {
  
  options(warn=-1)
  
  vars <- quos(...)
  
  if (length(vars) == 1) {
    message("For one variable with/out weights, use freqs(var, wt=weight) instead!")
    ret <- df %>% freqs(!!!vars)
    return(ret)
  }
  
  df <- df %>% select(!!!vars)
  
  if (keep_nas) {
    df <- df %>% replaceall(NA, "N/A")
  }
  
  x <- df[,1]
  y <- df[,2]
  
  if (prow) {
    newx <- y; newy <- x
    x <- newx; y <- newy
  }
  cross_name <- paste(colnames(x), "x", colnames(y))
  
  if (length(vars) == 2) {
    weight <- rep(1, nrow(df))
    decimals <- 0
  } else {
    weight <- df[,3]
    cross_name <- paste0(cross_name, " (", colnames(weight), ")")
  }
  
  dfn <- data.frame(x, y, weight)
  colnames(dfn) <- c("x","y","weight")
  
  ret <- freqs(dfn, x, y, wt=weight) %>% select(-pcum, -p)
  levels <- factor(unique(ret$x), levels = unique(ret$x))
  tab <- tidyr::spread(ret, y, n)
  ret <- tab[match(levels, tab$x),]
  colnames(ret)[1] <- cross_name
  
  # Create totals
  ret <- ret %>% mutate(total = rowSums(.[-1])) 
  if (pcol | prow) {
    ret <- ret %>% mutate_if(is.numeric, funs(round(100*./sum(.), decimals)))
  }
  if (pall) {
    all <- sum(ret[,-1] %>% select(-total))
    ret <- ret %>% mutate_if(is.numeric, funs(round(100*./all, decimals)))
  }
  
  if (!total) {
    ret <- ret %>% select(-total)
  }
  
  return(ret)
  
}
