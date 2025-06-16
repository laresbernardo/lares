encodeit <- function(x, from = "UTF-8", to = "latin1",
                     skip_class = "rpart.rules",
                     sub = "??") {
  if (skip_class %in% class(x)) {
    x
  } else if ("list" %in% class(x)) {
    lapply(x, encodeit)
  } else if ("data.frame" %in% class(x)) {
    data.frame(lapply(x, function(y) encodeit(y)))
  } else if (any(c("factor", "character") %in% class(x))) {
    is_factor <- class(x) %in% "factor"
    if (is_factor) levs <- levels(x)
    x <- as.character(x)
    x <- iconv(x, from, to, sub = sub)
    x <- gsub("(\\?){2,}", sub, x)
    if (is_factor) x <- factor(x, levels = encodeit(levs))
  }
  x
}
