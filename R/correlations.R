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
#' @param ignore Character vector. Which columns do you wish to exlude?
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
corr <- function(df, method = "pearson", ignore = NA, 
                 dummy = TRUE, dates = FALSE, 
                 redundant = TRUE, logs = FALSE, 
                 plot = FALSE, top = NA) {
  
  # Ignored columns
  if (!is.na(ignore)[1]) df <- select(df, -one_of(ignore))
  
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
    cor <- cor[which,which]
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
#' @param ignore Character vector. Which columns do you wish to exlude?
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
                     ignore = NA,
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
  rs <- corr(df, method = method, ignore = ignore, logs = logs, dates = dates)
  var <- as_label(vars[[1]])
  rs <- select(rs, -contains(paste0(var,"_log"))) %>%
    filter(!grepl(paste0(var,"_log"), row.names(.)))
  
  # Check if main variable exists
  if (!var %in% colnames(rs)) {
    warning(paste("Not a valid input:", var, "was transformed or does not exist."))
    maybes <- colnames(rs)[grepl(var, colnames(rs))]
    if (length(maybes) > 0 & maybes[1] %in% colnames(rs)) {
      message(paste0("Maybe you meant one of: ", vector2text(maybes), ". ",
                     "Automatically using '", maybes[1], "'"))
      var <- maybes[1]
    } else stop()
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
      geom_col() + coord_flip() + 
      geom_text(aes(hjust = hjust), size = 3, colour = "black") +
      scale_fill_manual(values = c("FALSE" = "#E5586E", "TRUE" = "#59B3D2")) +
      guides(fill = FALSE) +
      labs(title = paste("Correlations of", var), x = NULL, y = "Correlation [%]") +
      scale_y_percent(expand = c(0, 0)) + theme_lares2()
    
    if (!is.na(top) & top < original_n) p <- p + 
        labs(subtitle = paste("Plotting top", top, "out of", 
                              original_n, "variables (original & dummy)"))
  }
  
  if (!is.na(subdir)) {
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
#' @param ignore Character vector. Which columns do you wish to exlude?
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
corr_plot <- function(df, ignore = NA, method = "pearson", order = "FPC", 
                      type = "square", logs = FALSE) {
  
  try_require("corrplot")
  c <- corr(df, method, ignore, logs = logs)
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
#' @param type Integer. Plot type. 1 is for overall rank. 2 is for local rank.
#' @param max Numeric. Maximum correlation permited (from 0 to 1)
#' @param top Integer. Return top n results only. Only valid when type = 1
#' @param local Integer. Label top n local correlations. Only valid when type = 2
#' @param ignore Character vector. Which columns do you wish to exlude?
#' @param contains Character vector and Boolean. Filter cross-correlations 
#' with variables that contains certain strings (using any value if vector used).
#' @param grid Boolean. Separate into grids?
#' @param rm.na Boolean. Remove NAs?
#' @param dummy Boolean. Should One Hot Encoding be applied to categorical columns? 
#' @param method Character. Any of: c("pearson", "kendall", "spearman")
#' @export
corr_cross <- function(df, plot = TRUE, type = 1, 
                       max = 1, top = 25, local = 1,
                       ignore = NA, contains = NA, grid = FALSE,
                       rm.na = FALSE, dummy = TRUE,
                       method = "pearson") {
  
  if (sum(is.na(df))) warning("There are NA values in your dataframe!")
  c <- corr(df, ignore = ignore, plot = FALSE, dummy = dummy, method = method)
  
  ret <- data.frame(gather(c)) %>% 
    mutate(mix = rep(colnames(c), length(c))) %>%
    mutate(p1 = rep(1:length(c), each = length(c)),
           p2 = rep(1:length(c), length(c)),
           aux = p2 - p1) %>% filter(aux > 0) %>%
    mutate(rel = abs(value)) %>% filter(rel < max) %>% arrange(desc(rel)) %>%
    {if (rm.na) filter(., !grepl("_NAs", mix)) else .} %>%
    filter(!grepl("_OTHER", key)) %>%
    rename(corr = value) %>%
    mutate(value = paste(key, mix)) %>%
    {if (!is.na(contains)) 
      filter(., grepl(vector2text(
        contains, sep = "|", quotes = FALSE), paste(key, mix))) else .} %>%
    select(key, mix, corr)
  
  for (i in 1:ncol(df)) {
    if (i == 1) ret <- mutate(ret, group1 = "fill", group2 = "fill")
    group <- colnames(df)[i]
    aux <- ifelse(grepl(group, ret$key), group, "fill")
    ret$group1 <- ifelse(ret$group1 == "fill", aux, ret$group1)
    aux <- ifelse(grepl(group, ret$mix), group, "fill")
    ret$group2 <- ifelse(ret$group2 == "fill", aux, ret$group2)
  }
  ret <- filter(ret, group1 != group2)
  
  if (plot) {
    n <- ifelse(type == 1, top, local)
    subtitle <- paste(n, "most relevant")
    if (!is.na(contains)[1]) subtitle <- paste(subtitle, "containing", vector2text(contains))
    if (max < 1) subtitle <- paste0(subtitle," (excluding +", 100*max, "%)")
    if (rm.na) subtitle <- paste(subtitle, paste("[NAs removed]"))
    if (type == 1) {
      p <- ret %>% head(top) %>%
        mutate(label = paste(key, "+", mix), abs = abs(corr),
               sign = ifelse(corr < 0, "bad", "good"),
               x = ifelse(corr < 0, -0.1, 1.1)) %>%
        ggplot(aes(x = reorder(label,abs), y = corr, fill = sign)) +
        geom_col() + coord_flip() + guides(fill = FALSE) +
        geom_hline(aes(yintercept = 0), alpha = 0.5) + 
        geom_text(aes(hjust = x, label = signif(100*corr, 3)), size = 3, colour = "white") +
        labs(title = "Ranked Cross-Correlations", subtitle = subtitle,
             x = NULL, y = "Correlation [%]") +
        scale_fill_manual(values = c("bad" = "#E5586E", "good" = "#59B3D2")) +
        scale_y_percent() + theme_lares2()
      if ((!is.na(contains)[1] & length(contains) == 1) | grid)
        p <- p + facet_grid(mix ~ ., scales = "free", space = "free")
    }
    if (type == 2) {
      ret <- rbind(data.frame(ret, group = ret$group1), 
                   data.frame(ret, group = ret$group2)) %>%
        arrange(desc(abs(corr)))
      aux <- position_jitter(width = 0.4, seed = 123)
      p <- ret %>% 
        group_by(group) %>%
        mutate(hjust = ifelse(corr > 0, 1, 0),
               size = abs(corr),
               alpha = ifelse(row_number() <= local, 2, size),
               label = ifelse(row_number() <= local, paste(key, "+", mix), "")) %>%
        ggplot(aes(x = group, y = corr, label = label, colour = group)) + 
        geom_jitter(aes(alpha = alpha, size = size), position = aux) + 
        geom_hline(yintercept = 0, alpha = 0.3) +
        geom_text(aes(hjust = hjust), size = 2.9, position = aux, colour = "black") +
        guides(colour = FALSE, alpha = FALSE, size = FALSE) +
        scale_size(range = c(0.4, 2)) +
        labs(x = NULL, y = "Correlation [%]", subtitle = subtitle,
             title = "Local Cross-Correlations") +
        scale_y_percent() + coord_flip() +
        theme_lares2(pal = 2) 
    }
    return(p)
  }
  return(ret)
}
