####################################################################
#' Frequencies Calculations and Plot
#' 
#' This function lets the user group, count, calculate 
#' percentages and cumulatives. It also plots if needed. 
#' Perfect for using with dplyr pipes.
#' 
#' @param vector Vector to group, count, and mutate
#' @param plot Boolean. Do you wish to see a plot?
#' @param rm.na Boolean. Remove NAs from plot?
#' @export
freqs <- function(vector, ..., plot = FALSE, rm.na = FALSE) {
  
  require(dplyr)
  require(lazyeval)
  
  output <- vector %>%
    group_by_(.dots = lazy_dots(...)) %>%
    tally() %>% arrange(desc(n)) %>%
    mutate(p = round(100*n/sum(n),2), pcum = cumsum(p))
  
  if (plot == TRUE) {
    
    require(ggplot2)
    require(scales)
    options(warn=-1)
    
    plot <- ungroup(output)
    
    if (rm.na == TRUE) {
      plot <- na.omit(plot)
    }
    
    # Create some dynamic aesthetics
    plot$labels <- paste0(plot$n," (",plot$p,"%)")
    plot$label_colours <- ifelse(plot$n > mean(range(plot$n)), "m", "f")
    plot$label_hjust <- ifelse(plot$n < min(plot$n) + diff(range(plot$n)) * 0.25, -0.1, 1.05)
    variable <- colnames(plot)[1]
    colnames(plot)[1] <- "names"
    
    # Two features
    if (ncol(output) - 3 == 2) { 
      facet_name <- colnames(plot)[2]
      colnames(plot)[1] <- "facet"
      colnames(plot)[2] <- "names"
      plot$facet[is.na(plot$facet)] <- "NA"
    }
    
    p <- ggplot(plot, aes(x = reorder(as.character(names), n),
                          y = n, label = labels, 
                          fill = p)) +
      geom_col(alpha=0.9, width = 0.8) +
      geom_text(aes(
        hjust = label_hjust,
        colour = label_colours), size = 2.6) + lares::gg_text_customs() +
      coord_flip() + theme_minimal() + guides(colour = FALSE) +
      labs(x = "", y = "Counter", fill = "[%]",
           title = paste("Frequencies and Percentages:", variable)) +
      scale_fill_gradient(low = "lightskyblue2", high = "navy")
    
    if (ncol(output) - 3 == 2) { 
      p <- p + facet_grid(as.character(facet) ~ .) + 
        labs(subtitle = paste("Inside the facet grids:", facet_name)) +
        theme_light()
    }
    print(p)
  }
  
  return(output)
  
}


####################################################################
#' Convert year month format YYYY-MM
#' 
#' This function lets the user convert a date into YYYY-MM format
#' 
#' @param date Date. Date we wish to transform 
#' @export
year_month <- function(date) {
  
  require(lubridate)
  require(stringr)
  
  return(paste(
    lubridate::year(date),
    str_pad(lubridate::month(date), 2, pad = "0"),
    sep="-"))
}


####################################################################
#' Convert year week format YYYY-WW
#' 
#' This function lets the user convert a date into YYYY-WW format
#' 
#' @param date Date. Date we wish to transform
#' @export
year_week <- function(date) {
  
  require(lubridate)
  require(stringr)
  
  return(paste(
    lubridate::year(date),
    str_pad(lubridate::week(date), 2, pad = "0"),
    sep="-"))
}


####################################################################
#' Count Categories on a Dataframe
#' 
#' This function lets the user count unique values in a categorical 
#' dataframe
#'
#' @param df Categorical Vector
#' @export
categoryCounter <- function (df) {
  
  require(dplyr)
  
  cats <- df %>% select_if(is.character)
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


####################################################################
#' Reduce categorical values
#' 
#' This function lets the user reduce categorical values in a vector
#' 
#' @param vector Categorical Vector
#' @param nmin Integer. Number of minimum times a value is repeated
#' @param pmin Numerical. Porcentage of minimum times a value is repeated
#' @param pcummax Numerical. Top cumulative porcentage of most 
#' repeated values
#' @param top Integer. Keep the n most frequently repeated values
#' @param other_label Character. Which value do you wish to replace 
#' the filtered values with?
#' @export
categ_reducer <- function(vector, 
                          nmin = 0, 
                          pmin = 0, 
                          pcummax = 100, 
                          top = NA, 
                          other_label = "other") {
  require(dplyr)
  df <- data.frame(name = vector) %>% lares::freqs(., name)
  if (!is.na(top)) {
    top <- df %>% slice(1:top)
  } else {
    top <- df %>% filter(n >= nmin & p >= pmin & p <= pcummax) 
  }
  vector <- ifelse(vector %in% top$name, 
                   as.character(vector), other_label)
  return(vector)
}


####################################################################
#' Normalize values
#' 
#' This function lets the user normalize numerical values into 
#' the 0 to 1 range
#' 
#' @param x Numeric Vector. Numbers to be transformed into 
#' normalized vector
#' @export
normalize <- function(x) {
  if (is.numeric(x)) {
    x <- (x-min(x)) / (max(x)-min(x))
    return(x) 
  } else {
    stop("Try with a numerical vector!")
  }
}


####################################################################
#' Convert a vector into a comma separated text
#' 
#' Convert a vector into a comma separated text
#' 
#' @param vector Vector. Vector with more than 1 observation
#' @param sep Character. String text wished to insert between values
#' @param quotes Boolean. Bring simple quotes for each 
#' observation (useful for SQL)
#' @export
vector2text <- function(vector, sep=", ", quotes = TRUE) {
  output <- paste(shQuote(vector), collapse=sep)
  if (quotes == FALSE) {
    output <- gsub("'", "", output)
  }
  return(output)
}


####################################################################
#' Clean text
#' 
#' This function lets the user clean text into getting only alphanumeric 
#' characters and no accents/symbols on letters.
#' 
#' @param text Character Vector
#' @param spaces Boolean. Keep spaces?
#' @export
cleanText <- function(text, spaces = TRUE) {
  text <- as.character(text)
  output <- tolower(gsub("[^[:alnum:] ]", "", 
                         iconv(text, from="UTF-8", to="ASCII//TRANSLIT")))
  if (spaces == FALSE) {
    output <- gsub(" ", "", output)
  }
  return(output)
}


####################################################################
#' Find country from a given IP
#' 
#' This function lets the user find a country from a given IP Address
#' 
#' @param ip Vector. Vector with all IP's we wish to search
#' @export
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


####################################################################
#' Distance from specific point to line
#' 
#' This function lets the user calculate the mathematical linear distance 
#' Between a specific point and a line (given geometrical 3 points)
#' 
#' @param a Vector. Coordinates of the point from which we want to 
#' measure the distance
#' @param b Vector. Coordinates of 1st point over the line
#' @param c Vector. Coordinates of 2st point over the line
#' @export
dist2d <- function(a, b = c(0, 0), c = c(1, 1)) {
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1, v2)
  d <- abs(det(m)) / sqrt(sum(v1 * v1))
}


####################################################################
#' Nicely Format Numerical Values
#' 
#' This function lets the user format numerical values nicely
#' 
#' @param x Numerical Vector
#' @param decimals Integer. Amount of decimals to display
#' @param type Integer. 1 for International standards. 2 for 
#' American Standards.  
#' @param scientific Boolean. Scientific notation
#' @export
formatNum <- function(x, decimals = 2, type = 1, scientific = FALSE) {
  if (scientific == FALSE) {
    options(scipen=999)
  } else {
    formatC(numb, format = "e", digits = 2)
  }
  if (type == 1) {
    format(round(as.numeric(x), decimals), nsmall=decimals, 
           big.mark=".", decimal.mark = ",")
  } else {
    format(round(as.numeric(x), decimals), nsmall=decimals, 
           big.mark=",", decimal.mark = ".") 
  }
}


####################################################################
#' One Hot Encoding for a Vector with Comma Separated Values
#' 
#' This function lets the user do one hot encoding on a variable with 
#' comma separated values
#' 
#' @param df Vector or Dataframe. Contains different variables in each 
#' column, separated by a specific character
#' @param variables Character. Which variables should we split into new columns
#' @param sep Character. Which character separates the elements
#' @export
one_hot_encoding_commas <- function(df, variables, sep=","){
  # Note that variables must be provided in strings
  for (var in seq_along(variables)) {
    variable <- variables[var]
    df[df==""|is.na(df)] <- "NAs" # Handling missingness
    x <- as.character(unique(df[[variable]]))
    x <- gsub(" ", "", toString(x)) # So it can split on strings like "A1,A2" and "A1, A2"
    x <- unlist(strsplit(x, sep))
    x <- paste(variable, x, sep="_")
    new_columns <- sort(unique(as.character(x)))
    if (length(new_columns) >= 15) {
      message(paste("You are using more than 15 unique values on this variable:", variable))
    }
    for (i in seq_along(new_columns)){
      df$temp <- NA
      df$temp <- ifelse(grepl(new_columns[i], df[[variable]]), TRUE, FALSE)
      colnames(df)[colnames(df) == "temp"] <- new_columns[i]
    }
  }
  return(df)
}


####################################################################
#' Balance Binary Data by Resampling: Under-Over Sampling
#' 
#' This function lets the user balance a given data.frame by resampling
#' with a given relation rate and a binary feature.
#' 
#' @param df Vector or Dataframe. Contains different variables in each column, separated by a specific character
#' @param variable Character. Which binary variable should we use to resample df
#' @param rate Numeric. How many X for every Y we need? Default: 1
#' @param seed Numeric. Seed to replicate and obtain same values
#' @export
balance_data <- function(df, variable, rate = 1, seed = 0) {
  
  require(dplyr)
  
  set.seed(seed)
  
  df <- rename(df, tag = variable)
  tags <- unique(df$tag)
  
  if (length(unique(df$tag)) != 2) {
    stop("You should use a variable with only 2 unique values!")
  } else {
    message(paste("Resampled from:",lares::vector2text(table(df$tag), sep = " x ", quotes = F)))
    ones <- df %>% filter(tag == tags[1])
    zeros <- df %>% filter(tag == tags[2]) %>% 
      sample_n(rate * nrow(ones))
    balanced <- rbind(ones, zeros)
    message(paste("Into:",lares::vector2text(table(balanced$tag), sep = " x ", quotes = F)))
    balanced <- rename_at(balanced, vars("tag"), funs(paste0(variable)))
    return(balanced)
  }
}


####################################################################
#' List files in a directory
#' 
#' This function lets the user list all files on a given directory.
#' It also lets filter files which contains a string.
#' 
#' @param folder Character. Directory which contains files
#' @param recursive Boolean. Should the listing recurse into directories?
#' @param regex Character. String to use for filtering files
#' @param images Boolean. Bring only image files?
#' @param export Boolean. Do you wish to export list as txt file?
#' @export
listfiles <- function(folder, recursive = TRUE, regex = NA, images = FALSE, export = FALSE) {
  
  if (!file.exists(folder)) {
    stop("That directory doesn't exist; please try again!")
  }
  
  files <- list.files(folder, recursive = recursive)
  address <- paste0(folder, "/", files)
  info <- file.info(address)
  files <- gsub("_", " ", files)
  
  df <- data.frame(filename = files, address, info)
  df$size <- as.integer(df$size/1024)
  imgs <- "jpg|JPG|jpeg|JPEG|png|PNG|gif|GIF"
  
  if (!is.na(regex)) {
    df <- df[grep(regex, df$filename),] 
  }
  
  if (images == TRUE) {
    if (nrow(df) > 250) {
      message(paste("This might take a while... Analizing around", 
                    lares::formatNum(nrow(df), decimals = 0), "files!"))
    }
    
    require(exifr)
    require(dplyr)
    require(lubridate)
    
    tags <- c("FileName", "SourceFile",
              "CreateDate", "DateTimeOriginal", "FileModifyDate",
              "FileTypeExtension", "Megapixels",
              "ImageSize", "ImageWidth", "ImageHeight", 
              "GPSLongitude", "GPSLatitude",
              "Rotation", "Flash", "Duration")
    
    df <- read_exif(folder, recursive = TRUE, tags = tolower(tags)) %>% 
      select(one_of(tags)) %>%
      mutate(DateTimeOriginal = ymd_hms(DateTimeOriginal),
             CreateDate = ymd_hms(CreateDate),
             FileModifyDate = ymd_hms(FileModifyDate))
    
    df <- df[,colSums(is.na(df)) < nrow(df)]
    
  }
  
  if (export == TRUE) {
    write.table(df$filename, file = "files.txt", quote = FALSE, row.names = FALSE) 
  }
  
  row.names(df) <- NULL
  df$address <- NULL
  
  return(df)
  
}
