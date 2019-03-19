####################################################################
#' Cross tabulation
#' 
#' A cross-tabulation function with output similar to STATA.
#' 
#' @param x,y Vectors. For dependent and independent values
#' @param weight Vector. An optional vector for a weighted cross tabulation
#' @param order Boolean. Sort desc the whole table?
#' @param total Boolean. Return total values?
#' @param prow,pcol,pall Boolean. Calculate percent values for rows, columns,
#' or the whole table, respectively.
#' @param decimals Integer. How many decimals should be returned?
#' @param keep_nas Boolean. Keep NAs and count them as well?
#' @param list Boolean. Return as a single list?
#' @export
crosstab <- function(x, y, weight = 1, order = TRUE, 
                     total = TRUE,
                     prow = FALSE, pcol = FALSE, pall = FALSE, 
                     decimals = 2, keep_nas = TRUE,
                     list = FALSE) {
  
  options(warn=-1)
  
  if (keep_nas) {
    x <- replaceall(x, NA, "N/A")
    y <- replaceall(y, NA, "N/A")
  }
  
  if (length(weight) == 1) {
    weight <- rep(weight, length(x))
  }
  
  if (weight == 1) {
    pall <- TRUE
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


####################################################################
#' Ponder Frequency (Weighted Counter)
#' 
#' This function lets the user calculate the weighted frequency in a
#' tidy friendly way.
#' 
#' @param df Data.frame. It must contain a ponder column.
#' @param ... Variables. Column(s) to calculate the weighted frequencies.
#' @param order Boolean. Do you wish to desc sort frequencies?
#' @param compare Boolean. Compare with equal-weighted values?
#' @export
crossval <- function(df, ..., order = TRUE, compare = TRUE) {
  vars <- quos(...)
  if (!"weight" %in% colnames(df)) {
    colnames(df)[grepl("ponder", colnames(df), ignore.case = T)][1] <- "weight" 
  }
  res <- df %>% 
    count(!!!vars, wt = weight) %>% 
    mutate(p = round(100*n/sum(n), 2))
  if (order) {
    res <- res %>% arrange(desc(p)) 
  }
  colnames(res)[1:(ncol(res)-2)] <- paste0(gsub("~","",as.character(vars)),"_weighted")
  if (compare) {
    rese <- df %>% freqs(!!!vars)
    colnames(rese)[(ncol(rese)-3):ncol(rese)] <- rev(c("p_real", "n_real","pcum_real"))
    res <- res %>% cbind(rese[,-(1:(ncol(res)-2))]) %>% .[,-ncol(.)] %>%
      mutate(dif_p = p_real - p)
  }
  return(res)
}