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
#' @param order Boolean. Sort desc the whole table?
#' @param total Boolean. Return total values?
#' @param prow,pcol,pall Boolean. Calculate percent values for rows, columns,
#' or the whole table, respectively.
#' @param decimals Integer. How many decimals should be returned?
#' @param keep_nas Boolean. Keep NAs and count them as well?
#' @param list Boolean. Return as a single list?
#' @export
crosstab <- function(df, ..., 
                     order = TRUE, total = TRUE,
                     prow = FALSE, pcol = FALSE, pall = FALSE, 
                     decimals = 2, keep_nas = TRUE,
                     list = FALSE) {
  
  options(warn=-1)
  
  vars <- quos(...)
  
  if (length(vars) == 1) {
    message("For one variable with weights, use crossval() instead!")
    ret <- df %>% freqs(!!!vars)
    return(ret)
  }
  
  df <- df %>% select(!!!vars)
  
  if (keep_nas) {
    x <- df[,1] %>% replaceall(NA, "N/A")
    y <- df[,2] %>% replaceall(NA, "N/A")
  }
  
  if (length(vars) == 2) {
    weight <- rep(1, length(x))
    decimals <- 0
  } else {
    weight <- df[,3]
  }
  
  weight <- as.numeric(weight)
  t <- round(xtabs(weight ~ x + y), decimals)
  tt <- data.frame(t) %>% 
    tidyr::spread(y, Freq) %>% .[,-1] %>%
    mutate(n = rowSums(.)) %>%
    t() %>% data.frame() %>% 
    mutate_all(function(x) as.numeric(as.character(x))) %>%
    mutate(n = rowSums(.)) %>%
    t() %>% data.frame() %>% 
    mutate_all(function(x) as.numeric(as.character(x)))
  ret <- tt
  
  rows <- data.frame(name = c(rownames(t),"Total"), n = as.vector(unlist(tt[ncol(tt)]))) %>% 
    arrange(desc(n)) %>% mutate(rank = row_number()) %>%
    mutate(rank = ifelse(name == "Total", nrow(tt)+1, rank)) %>% arrange(rank)
  cols <- data.frame(name = c(colnames(t),"Total"), n = as.vector(unlist(tt[nrow(tt),]))) %>% 
    arrange(desc(n)) %>% mutate(rank = row_number()) %>%
    mutate(rank = ifelse(name == "Total", ncol(tt)+1, rank)) %>% arrange(rank)
  
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
  
  if (order) {
    ret <- ret[c(as.character(rows$name[rows$name!="Total"]), "Total"),
               c(as.character(cols$name[cols$name!="Total"]), "Total")]
  }
  
  if (!total) {
    ret <- ret %>% select(-Total) %>% filter(rownames(.) != "Total")
    rownames(ret) <- rownames(t)
  }
  
  if (list) {
    ret <- tidyr::gather(ret) %>% 
      mutate(names = rep(rownames(ret), ncol(ret))) %>%
      select(names, key, value)
    colnames(ret) <- c("dependent", "independent", "values")
  }
  
  return(ret)
  
}
