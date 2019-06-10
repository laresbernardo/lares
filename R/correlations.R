####################################################################
#' Correlation table
#'
#' This function correlates a whole dataframe, filtering automatically
#' all numerical values.
#'
#' @family Calculus
#' @family Correlations
#' @param df Dataframe. It doesn't matter if it's got non-numerical
#' columns: they will be filtered!
#' @param method Character. Any of: c("pearson", "kendall", "spearman")
#' @param dummy Boolean. Should One Hot Encoding be applied to categorical columns? 
#' @param dates Boolean. Do you want the function to create more features
#' out of the date/time columns?
#' @param redundant Boolean. Should we keep redundat columns? i.e. It the
#' column only has two different values, should we keep both new columns?
#' @param logs Boolean. Automatically calculate log(values) for numerical
#' variables (not binaries)
#' @param plot Boolean. Do you wish to see a plot?
#' @param top Integer. Select top N most relevant variables? Filtered 
#' and sorted by mean of each variable's correlations
#' @export
corr <- function(df, method = "pearson", dummy = TRUE, dates = FALSE, 
                 redundant = TRUE, logs = FALSE, plot = FALSE, top = NA) {
  
  options(warn = -1)
  
  # One hot encoding for categorical features
  if (dummy) df <- ohse(df, summary = FALSE, redundant = redundant, dates = dates)
  
  # Select only numerical features and create log+1 for each one
  d <- numericalonly(df, logs = logs)
  
  # Correlations
  rs <- cor(d, use = "pairwise.complete.obs", method = method)
  rs[is.na(rs)] <- 0
  cor <- round(data.frame(rs), 4)
  row.names(cor) <- colnames(cor)
  
  # Top N
  if (!is.na(top)) {
    message(paste("Returning the top", top, "variables only..."))
    imp <- cor %>% 
      summarise_all(funs(mean(.))) %>% t() %>% 
      data.frame(variable = row.names(.), mean = abs(.)) %>%
      arrange(desc(abs(mean)))
    which <- as.vector(imp$variable[1:top])
    cor <- cor %>% select(one_of(which)) %>% 
      filter(row.names(.) %in% which) 
  }
  
  # Plot
  if (plot) corr_plot(cor, logs = FALSE)
  
  return(cor)
  
}


####################################################################
#' Correlation between variable and dataframe
#'
#' This function correlates a whole dataframe with a single feature.
#'
#' @family Exploratory
#' @family Correlations
#' @param df Dataframe.
#' @param ... Object. Name of the variable to correlate
#' @param method Character. Any of: c("pearson", "kendall", "spearman")
#' @param trim Integer. Trim words until the nth character for 
#' categorical values (applies for both, target and values)
#' @param clean Boolean. Use lares::cleanText for categorical values (applies 
#' for both, target and values)
#' @param plot Boolean. Do you wish to plot the result? If set to TRUE, the
#' function will return only the plot and not the result's data
#' @param logs Boolean. Automatically calculate log(values) for numerical
#' variables (not binaries)
#' @param dates Boolean. Do you want the function to create more features
#' out of the date/time columns?
#' @param top Integer. If you want to plot the top correlations, 
#' define how many
#' @param ceiling Numeric. Remove all correlations above... Range: (0-100]
#' @param zeroes Do you wish to keep zeroes in correlations too?
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to 
#' save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @export
corr_var <- function(df, ..., 
                     method = "pearson", 
                     trim = 0,
                     clean = FALSE,
                     plot = TRUE,
                     logs = FALSE, 
                     dates = TRUE,
                     top = NA, 
                     ceiling = 100, 
                     zeroes = FALSE,
                     save = FALSE, 
                     subdir = NA,
                     file_name = "viz_corrvar.png") {
  
  vars <- quos(...)
  
  # Calculate correlations
  rs <- corr(df, method = method, logs = logs, dates = dates)
  var <- as.character(vars[[1]])[2]
  rs <- rs %>% 
    select(-contains(paste0(var,"_log"))) %>%
    filter(!grepl(paste0(var,"_log"), row.names(.)))
  
  # Check if main variable exists
  if (!var %in% colnames(rs)) {
    message(paste("The variable", var, "is not a valid input because that column",
                  "doesn't exist or probably was transformed!"))
    maybes <- colnames(rs)[grepl(var, colnames(rs))]
    if (length(maybes) > 0) {
      message(paste("Maybe you meant one of these:", vector2text(maybes)))
    }
    stop()
  }
  
  d <- data.frame(variables = colnames(rs), corr = rs[, c(var)])
  d <- d[(d$corr < 1 & !is.na(d$corr)),]
  d <- d[order(-abs(d$corr)), ]
  
  original_n <- nrow(d)
  
  if (!zeroes) d <- d[d$corr != 0, ]
  
  # Limit automatically when more than 30 observations
  if (is.na(top) & nrow(d) > 30) {
    top <- 30
    message(paste("Automatically reduced results to the top", top, "variables.",
                  "Use the 'top' parameter to override this limit."))
  }
  
  if (ceiling < 100) {
    d <- d[abs(d$corr) < ceiling/100, ]
    message(paste0("Removing all correlations greater than ", ceiling, "% (absolute)"))
  }
  
  if (!is.na(top)) d <- d[1:as.integer(top), ]
  
  d <- d[complete.cases(d), ]
  
  # Shorten up the long names of some variables
  if (trim > 0) {
    d$variables <- substr(d$variables, 1, trim)
    message(paste("Trimmed all name values into", trim, "characters"))
  }
  
  if (plot) {
    p <- ungroup(d) %>%
      mutate(pos = ifelse(corr > 0, TRUE, FALSE),
             hjust = ifelse(abs(corr) < max(abs(corr))/1.5, -0.1, 1.1)) %>%
      ggplot(aes(x = reorder(variables, abs(corr)), 
                 y = abs(corr), fill = pos, label = signif(100*corr, 3))) +
      geom_hline(aes(yintercept = 0), alpha = 0.5) +
      geom_col() + 
      coord_flip() + theme_minimal() +
      geom_text(aes(hjust = hjust), size = 3, colour = "black") +
      scale_fill_manual(values = c("FALSE" = "#E5586E", "TRUE" = "#59B3D2")) +
      guides(fill = FALSE) +
      labs(title = paste("Correlation of", var, "vs other variables"), 
           x = "", y = "Correlation") +
      scale_y_percent(expand = c(0, 0)) + theme_lares2()
    
    if (!is.na(top) & top < original_n) p <- p + 
        labs(subtitle = paste("Plotting top", top, "out of", 
                              original_n, "variables (original + dummy)"))
  }
  
  if (!is.na(subdir)) {
    options(warn = -1)
    dir.create(file.path(getwd(), subdir), recursive = T)
    file_name <- paste(subdir, file_name, sep = "/")
  }
  
  if (save) p <- p + ggsave(file_name, width = 6, height = 6)
  
  if (plot) {
    return(p)
  } else {
    return(d)  
  }
}


####################################################################
#' Correlation plot
#'
#' This function correlates a whole dataframe with a single feature.
#'
#' @family Visualization
#' @family Correlations
#' @param df Dataframe.
#' @param method Character. Any of: c("pearson", "kendall", "spearman")
#' @param order Character. The ordering method of the correlation matrix.
#' Any of: c("original", "AOE", "FPC", "hclust", "alphabet")
#' @param type Character. The visualization method of correlation matrix 
#' to be used. 
#' Any of c("circle", "square", "ellipse", "number", "pie", "shade" 
#' and "color")
#' @param logs Boolean. Automatically calculate log(values) for numerical
#' variables (not binaries) and plot them
#' @export
corr_plot <- function(df, method = "pearson", order = "FPC", 
                      type = "square", logs = FALSE) {
  
  try_require("corrplot")
  c <- corr(df, method, logs = logs)
  plot <- corrplot(as.matrix(c),
                   order = order,
                   method = type, 
                   type = "lower",
                   diag = FALSE)
  return(plot)
}


####################################################################
#' Correlation Cross-Table
#'
#' This function creates a correlation full study and returns a rank
#' of the highest correlation variables obtained in a cross-table.
#'
#' @family Correlations
#' @family Exploratory
#' @param df Dataframe.
#' @param plot Boolean. Show and return a plot?
#' @param max Numeric. Maximum correlation permited (from 0 to 1)
#' @param top Integer. Return top n results only
#' @param rm.na Boolean. Remove NAs?
#' @export
corr_cross <- function(df, plot = TRUE, max = 1, top = 25, rm.na = FALSE) {
  c <- corr(df, plot = FALSE)
  ret <- data.frame(tidyr::gather(c)) %>% 
    mutate(mix = rep(colnames(c), length(c))) %>%
    mutate(rel = abs(value)) %>% filter(1*rel < max) %>% 
    arrange(desc(rel)) %>%
    mutate(rank = row_number()) %>%
    filter(rank %in% seq(2,length(c)*length(c)*2,2)) %>%
    mutate(redundant = ifelse(gsub("_.*","", key) == gsub("_.*","", mix), TRUE, FALSE)) %>%
    filter(redundant == FALSE) %>%
    filter(!grepl("_NAs", key)) %>%
    {if (rm.na) filter(., !grepl("_NAs", mix)) else .} %>%
    filter(!grepl("_OTHER", key)) %>%
    select(key, mix, value) %>%
    rename(corr = value) %>%
    head(top) 
  if (plot) {
    subtitle <- paste(top, "most relevant")
    if (max < 1) subtitle <- paste0(subtitle," (excluding +", 100*max, "%)")
    if (rm.na) subtitle <- paste(subtitle, paste("[NAs removed]"))
    p <- ret %>% 
      mutate(label = paste(key, "+", mix), abs = abs(corr),
             sign = ifelse(corr < 0, "bad", "good"),
             x = ifelse(corr < 0, -0.1, 1.1)) %>%
      ggplot(aes(x = reorder(label,abs), y = corr, fill = sign)) +
      geom_col() + coord_flip() + guides(fill = FALSE) +
      geom_hline(aes(yintercept = 0), alpha = 0.5) + 
      geom_text(aes(hjust = x, label = signif(100*corr, 3)), size = 3, colour = "white") +
      labs(title = "Ranked Cross-Correlations", subtitle = subtitle,
           x = "", y = "Correlation [%]") +
      scale_fill_manual(values = c("bad" = "#E5586E", "good" = "#59B3D2")) +
      scale_y_percent() + theme_lares2()
    return(p)
  }
  return(ret)
}
