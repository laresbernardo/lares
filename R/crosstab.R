####################################################################
#' Weighted Cross Tabulation
#' 
#' A cross-tabulation function with output similar to STATA, tidy
#' friendly, with weighting possibility.
#' 
#' @family Exploratory
#' @param df Data.frame. 
#' @param ... Variables. Dependent and independent variables. 
#' @param wt Variable, numeric. Weights.
#' @param prow,pcol,pall Boolean. Calculate percent values for rows, columns,
#' or the whole table, respectively.
#' @param decimals Integer. How many decimals should be returned?
#' @param rm.na Boolean. Remove NA values?
#' @param total Boolean. Return total values column?
#' @param order Boolean. Sort columns and rows by frequencies? Else, will
#' be sorted alphabetically
#' @return data.frame. Result of crossing the variables provided in \code{...} and
#' counting how many observations (rows) fall into each criteria.
#' @examples 
#' data(dft) # Titanic dataset
#' crosstab(dft, Survived, Pclass, total = FALSE)
#' # Show values in percentages
#' crosstab(dft, Pclass, Survived, prow = TRUE)
#' crosstab(dft, Pclass, Survived, pall = TRUE)
#' # Weighted by another variable
#' crosstab(dft, Survived, Pclass, wt = Fare, prow = TRUE)
#' @export
crosstab <- function(df, ..., wt = NULL,
                     prow = FALSE, pcol = FALSE, pall = FALSE, 
                     decimals = 2, rm.na = FALSE, total = TRUE,
                     order = TRUE) {
  
  if (pcol) {
    warning("Just change the order of your variables to get this results")
    return(invisible(NULL))
  }
  
  vars <- quos(...)
  wt <- enquo(wt)
  
  if (!length(vars) %in% c(2, 3)) {
    message("Use freqs() instead!")
    ret <- df %>% freqs(!!!vars)
    return(ret)
  }
  
  names <- gsub("~", "", as.character(vars))
  names <- gsub("\\.data\\$", "", names)
  xname <- names[1]
  yname <- names[2]
  
  df <- df %>% mutate_at(vars(1:2), as.character)
    
  first <- df %>% 
    freqs(!!vars[[1]], wt = !!wt, rm.na = rm.na) %>%
    select(!!vars[[1]]) %>% unlist()
  
  second <- df %>% 
    freqs(!!vars[[2]], wt = !!wt, rm.na = rm.na) %>%
    select(!!vars[[2]]) %>% unlist()
  
  aux <- df %>% freqs(!!!vars, wt = !!wt, rm.na = rm.na) %>% replace(is.na(.), '')
  colnames(aux)[1:2] <- c("A","B")

  # Order by frequency
  if (order) {
    aux <- aux %>%
      replaceall(NA, "NA", c("A","B")) %>%
      mutate(A = factor(.data$A, levels = first),
             B = factor(.data$B, levels = second)) 
  }
  
  ret <- aux %>%
    select(.data$A, .data$B, .data$n) %>% 
    tidyr::spread(.data$B, .data$n, drop = FALSE) %>%
    select(.data$A, one_of(levels(aux$B))) %>%
    arrange()
  colnames(ret)[1] <- sprintf("%s x %s", xname, yname)
  
  # Create totals
  ret <- ret %>% mutate(total = rowSums(select_if(., is.numeric), na.rm = TRUE))
  if (pcol | prow) 
    ret <- ret %>% mutate_if(is.numeric, list(~round(100*./sum(., na.rm = TRUE), decimals)))
  if (pall) {
    numericals <- ret[,-c(1, ncol(ret))]
    all <- sum(numericals, na.rm = TRUE)
    ret <- ret %>% mutate_if(is.numeric, list(~round(100*./all, decimals)))
  }
  ret <- arrange(ret, desc(.data$total))
  
  if (!total) ret <- ret %>% select(-.data$total)
  
  return(as_tibble(ret))
  
}
