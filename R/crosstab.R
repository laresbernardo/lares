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
#' @param order Boolean. Sort columns and rows by frequencies? Else, will
#' be sorted alphabetically.
#' @export
crosstab <- function(df, ..., 
                     prow = FALSE, pcol = FALSE, pall = FALSE, 
                     decimals = 2, keep_nas = TRUE, total = TRUE,
                     order = TRUE) {
  
  options(warn=-1)
  
  vars <- quos(...)
  
  names <- gsub("~","",as.character(vars))
  xname <- names[1]
  yname <- names[2]
  if (length(vars) == 3) {
    wname <- names[3] 
  }
  
  if (length(vars) == 1) {
    message("For one variable with/out weights, use freqs(var, wt=weight) instead!")
    ret <- df %>% freqs(!!!vars)
    return(ret)
  }
  
  df <- df %>% select(!!!vars)
  x <- df[,1]
  y <- df[,2]
  
  if (keep_nas) {
    df <- df %>% replace(is.na(.), "NA")
  }
  
  if (prow) {
    newx <- y; newy <- x; newxname <- yname; newyname <- xname;  
    x <- newx; y <- newy; xname <- newxname; yname <- newyname
  }
  
  cross_name <- paste(xname, "x", yname)
  
  if (length(vars) == 2) {
    weight <- rep(1, nrow(df))
    decimals <- 0
  } else {
    weight <- df[,3]
    cross_name <- paste0(cross_name, " (", wname, ")")
  }
  
  dfn <- data.frame(x, y, weight)
  colnames(dfn) <- c("x","y","weight")
  
  ret <- freqs(dfn, x, y, wt=weight) %>% select(-pcum, -p) %>%
    mutate_all(funs(ifelse(as.character(.)!="", ., "NA")))
  levels <- factor(unique(ret$x), levels = unique(ret$x))
  cols <- factor(unique(ret$y), levels = unique(ret$y))
  tab <- tidyr::spread(ret, y, n)
  ret <- tab[match(levels, tab$x),]
  colnames(ret)[1] <- cross_name
  ret <- ret %>% replace(is.na(.), 0)
  
  # Create totals
  ret <- ret %>% mutate(total = rowSums(select_if(., is.numeric)))
  if (pcol | prow) {
    ret <- ret %>% mutate_if(is.numeric, funs(round(100*./sum(.), decimals)))
  }
  if (pall) {
    all <- sum(ret[,-1] %>% select(-total))
    ret <- ret %>% mutate_if(is.numeric, funs(round(100*./all, decimals)))
  }
  
  if (order == TRUE) {
    ret <- ret %>% arrange(desc(total)) # Rows
    order <- c(colnames(ret)[1], as.character(cols), "total")
    ret <- ret[,order] # Columns
  }
  
  if (!total) {
    ret <- ret %>% select(-total)
  }
  
  return(ret)
  
}
