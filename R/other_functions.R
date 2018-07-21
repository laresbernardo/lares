# Package "lares" functions

# Group by, count and percentages
freqs = function(data, ..., plot=F) {

  suppressMessages(require(dplyr))

  output <- data %>%
    dplyr::group_by_(.dots = lazyeval::lazy_dots(...)) %>%
    dplyr::tally() %>% dplyr::arrange(desc(n)) %>%
    dplyr::mutate(p = round(100*n/sum(n),2), pcum = cumsum(p))
  return(output)
}

# Convert year month format YYYY-MM
year_month = function(date) {

  suppressMessages(require(lubridate))
  suppressMessages(require(stringr))

  return(paste(
    year(date),
    str_pad(month(date), 2, pad = "0"),
    sep="-"))
}

# Analyze NAs in a data.frame
nas = function(df, print = TRUE) {

  suppressMessages(require(dplyr))
  suppressMessages(require(VIM))
  suppressMessages(require(funModeling))

  nas <- df_status(df, print=FALSE) %>% filter(q_na > 0) %>% arrange(desc(q_na))
  subset <- subset(df, select=c(nas$variable))
  aggr(subset, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
       labels=names(data), cex.axis=.7, gap=2,
       ylab=c("Histogram of missing data","Pattern"))
}

# Insurance Companies' colours to use in ggplot as scale_fill_manual
gg_fill_customs <- function () {

  suppressMessages(require(ggplot2))

  values <- c("ALLIANZ"="#0038A8",
              "EQUIDAD"="#52CF44",
              "COLPATRIA"="#EE0606",
              "DEL ESTADO"="#F37000",
              "SURAMERICANA"="#1F6D8C",
              "MAPFRE"="#34000D",
              "LA PREVISORA"="#6F9A45",
              "AIG"="#C71585",
              "GENERALI"="#B21F1F",
              "SOLIDARIA"="#E69500",
              "LIBERTY"="#4E629A",
              "BOLIVAR"="#F0F206",
              "CIA"="#8ACBE5")
  return(scale_fill_manual(values=values))
}

# Insurance Companies' colours to use in ggplot as scale_color_manual
gg_colour_customs <- function () {

  suppressMessages(require(ggplot2))

  values <- c("ALLIANZ"="#0038A8",
              "EQUIDAD"="#52CF44",
              "COLPATRIA"="#EE0606",
              "DEL ESTADO"="#F37000",
              "SURAMERICANA"="#1F6D8C",
              "MAPFRE"="#34000D",
              "LA PREVISORA"="#6F9A45",
              "AIG"="#C71585",
              "GENERALI"="#B21F1F",
              "SOLIDARIA"="#E69500",
              "LIBERTY"="#4E629A",
              "BOLIVAR"="#F0F206",
              "CIA"="#8ACBE5")
  return(scale_color_manual(values=values))
}

# Count all categories on factor variables
categoryCounter <- function (df) {

  suppressMessages(require(dplyr))

  cats <- df %>% select_if(is.factor)
  result <- c()

  for (i in 1:ncol(cats)) {
    x <- freqs(cats, cats[,i])
    y <- colnames(cats)[i]
    x <- cbind(variable = y, x)
    result <- rbind(result, x)
  }
  result <- rename(result, category = `cats[, i]`)
  return(result)
}

# Reduce categorical values
categ_reducer <- function(vector, nmin = 0, pmin = 0, pcummax = 100, top = NA, other_label = "other") {
  require(dplyr)
  df <- data.frame(name = vector) %>% lares::freqs(., name)
  if (!is.na(top)) {
    top <- df %>% slice(1:top)
  } else {
    top <- df %>% filter(n >= nmin & p >= pmin & p <= pcummax) 
  }
  vector <- ifelse(vector %in% top$name, as.character(vector), other_label)
  return(vector)
}

# Normalize values
normalize <- function(x) {
 x <- (x-min(x)) / (max(x)-min(x))
 return(x)
}

# Convert a vector into a comma separated text
vector2text <- function(vector, sep=", ") {
  output <- paste(shQuote(vector), collapse=sep)
  return(output)
}


# Clean text
cleanText <- function(d) {
  d <- as.character(d)
  # Only alphanumeric characters and no accents/symbols on letters
  output <- tolower(gsub("[^[:alnum:] ]", "", iconv(d, from="UTF-8", to="ASCII//TRANSLIT")))
  return(output)
}

# Find country from a given IP
ip_country <- function(ip) {
  require(rvest)
  require(dplyr)
  
  ip <- ip[!is.na(ip)]
  ip <- ip[grep("^172\\.|^192\\.168\\.|^10\\.", ip, invert = T)]
  
  countries <- data.frame(ip = c(), country = c())
  for(i in 1:length(ip)) {
    message(paste("Searching for", ip[i]))
    url <- paste0("https://db-ip.com/", ip[i])
    scrap <- read_html(url) %>% html_nodes('.card-body tr') %>% html_text()
    country <- gsub("Country", "", trimws(scrap[grepl("Country", scrap)]))
    result <- cbind(ip = ip[i], country = country)
    countries <- rbind(countries, result)
  } 
  return(countries)
}

# Distance from specific point to line
dist2d <- function(a,b,c) {
  # a is the point
  # b and c are two points from the line
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
}

# Nicely format numerical values
formatNum <- function(x, decimals = 2, type = 1) {
  if (type == 1) {
    format(round(as.numeric(x), decimals), nsmall=decimals, big.mark=".")  
  } else {
    format(round(as.numeric(x), decimals), nsmall=decimals, big.mark=",")  
  }
}
