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
#' @param pvalue Boolean. Returns a list, with correlations and statistical 
#' significance (p-value) for each value
#' @param ignore Character vector. Which columns do you wish to exlude?
#' @param dummy Boolean. Should One Hot Encoding be applied to categorical columns? 
#' @param limit Integer. Limit one hot encoding to the n most frequent 
#' values of each column. Set to \code{NA} to ignore argument.
#' @param dates Boolean. Do you want the function to create more features
#' out of the date/time columns?
#' @param redundant Boolean. Should we keep redundat columns? i.e. It the
#' column only has two different values, should we keep both new columns?
#' @param logs Boolean. Automatically calculate log(values) for numerical
#' variables (not binaries)
#' @param top Integer. Select top N most relevant variables? Filtered 
#' and sorted by mean of each variable's correlations
#' @examples 
#' data(dft) # Titanic dataset
#' df <- dft[,2:5]
#' corr(df)
#' corr(df, ignore = "Pclass")
#' corr(df, redundant = TRUE)
#' corr(df, method = "spearman")
#' corr(df, pvalue = TRUE)
#' @export
corr <- function(df, method = "pearson", 
                 pvalue = FALSE,
                 ignore = NA, 
                 dummy = TRUE, limit = 10, dates = FALSE, 
                 redundant = FALSE, logs = FALSE, 
                 top = NA) {
  
  # Ignored columns
  if (!is.na(ignore)[1]) df <- select(df, -one_of(ignore))
  
  # One hot encoding for categorical features
  if (dummy) 
    df <- ohse(df, summary = FALSE, limit = limit, redundant = redundant, dates = dates)
  
  # Select only numerical features and create log+1 for each one
  d <- numericalonly(df, logs = logs)
  
  # Correlations
  rs <- suppressWarnings(cor(d, use = "pairwise.complete.obs", method = method))
  rs[is.na(rs)] <- 0
  cor <- round(data.frame(rs), 4)
  row.names(cor) <- colnames(cor)
  
  # Top N
  if (!is.na(top)) {
    message(paste("Returning the top", top, "variables only..."))
    imp <- cor %>% 
      summarise_all(funs(mean(.))) %>% t() %>% 
      data.frame(variable = row.names(.), mean = abs(.)) %>%
      arrange(desc(abs(.data$mean)))
    which <- as.vector(imp$variable[1:top])
    cor <- cor[which, which]
  }
  
  # Statistical significance (p-value)
  if (pvalue) {
    n <- t(!is.na(d)) %*% (!is.na(d))
    t <- (rs*sqrt(n - 2))/sqrt(1 - rs^2)
    p <- -2 * expm1(pt(abs(t), (n - 2), log.p = TRUE))
    p[p > 1] <- 1
    lp <- upper.tri(p)
    pa <- p[lp]
    pa <- p.adjust(pa, "holm")
    p[upper.tri(p, diag = FALSE)] <- pa
    p <- round(data.frame(p), 8)
    row.names(p) <- colnames(rs)
    return(list(cor = cor, pvalue = p))
  }
  
  return(cor)
  
}


####################################################################
#' Correlation between variable and dataframe
#'
#' This function correlates a whole dataframe with a single feature. It
#' automatically run one-hot-smart-encoding (ohse) so no need to input
#' only numerical values.
#'
#' @family Exploratory
#' @family Correlations
#' @param df Dataframe.
#' @param var Variable. Name of the variable to correlate
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
#' @param max_pvalue Numeric. Filter non-significant variables. Range (0, 1]
#' @param limit Integer. Limit one hot encoding to the n most frequent 
#' values of each column. Set to \code{NA} to ignore argument.
#' @param zeroes Do you wish to keep zeroes in correlations too?
#' @param save Boolean. Save output plot into working directory
#' @param subdir Character. Sub directory on which you wish to 
#' save the plot
#' @param file_name Character. File name as you wish to save the plot
#' @examples
#' options("lares.font" = NA) # Temporal
#' data(dft) # Titanic dataset
#' 
#' dft %>% corr_var(Survived, method = "spearman", plot = FALSE, top = 10)
#' 
#' # With plots, results are easier to compare:
#' 
#' # Correlate Survived with everything else and show only significant results
#' dft %>% corr_var(Survived_TRUE, max_pvalue = 0.05)
#' 
#' # Filter out variables with less than 50% of correlation
#' dft %>% corr_var(Survived_TRUE, ceiling = 50)
#' 
#' # Show only 10 values
#' dft %>% corr_var(Survived_TRUE, top = 10)
#' 
#' # Also calculate log(values)
#' dft %>% corr_var(Survived_TRUE, logs = TRUE, top = 15)
#' @export
corr_var <- function(df, var, 
                     ignore = NA,
                     method = "pearson", 
                     trim = 0,
                     clean = FALSE,
                     plot = TRUE,
                     logs = FALSE, 
                     dates = TRUE,
                     top = NA, 
                     ceiling = 100, 
                     max_pvalue = 1,
                     limit = 10,
                     zeroes = FALSE,
                     save = FALSE, 
                     subdir = NA,
                     file_name = "viz_corrvar.png") {
  
  vars <- enquos(var)
  var <- as_label(vars[[1]])
  df <- select(df, -contains(paste0(var,"_log")))
  
  # Calculate correlations
  rs <- corr(df, method = method, ignore = ignore, limit = limit,
             logs = logs, dates = dates, pvalue = TRUE)
  
  # Check if main variable exists
  if (!var %in% colnames(rs$cor)) {
    warning(paste("Not a valid input:", var, "was transformed or does not exist."))
    maybes <- colnames(rs$cor)[grepl(var, colnames(rs$cor))]
    if (length(maybes) > 0 & maybes[1] %in% colnames(rs$cor)) {
      warning(sprintf("Maybe you meant one of: %s", vector2text(head(maybes, 10))))
      message(sprintf("Automatically using '%s", maybes[1]))
      var <- maybes[1]
    } else stop()
  }
  
  d <- data.frame(variables = colnames(rs$cor), 
                  corr = rs$cor[, c(var)],
                  pvalue = rs$pvalue[, c(var)])
  d <- d[(d$corr < 1 & !is.na(d$corr)),]
  d <- d[order(-abs(d$corr)), ]
  
  original_n <- nrow(d)
  
  if (!zeroes) d <- d[d$corr != 0, ]
  
  # Suppress non-statistical significant correlations
  if (max_pvalue < 1) d <- d %>%
    mutate(pvalue = as.numeric(ifelse(is.na(.data$pvalue), 1, .data$pvalue))) %>%
    filter(.data$pvalue <= max_pvalue)
  
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
  
  if (!is.na(top)) d <- head(d, top + 1)
  
  d <- d[complete.cases(d), ]
  
  # Shorten up the long names of some variables
  if (trim > 0) {
    d$variables <- substr(d$variables, 1, trim)
    message(paste("Trimmed all name values into", trim, "characters"))
  }
  
  if (plot) {
    p <- ungroup(d) %>% 
      filter(.data$variables != "pvalue") %>%
      mutate(pos = ifelse(.data$corr > 0, TRUE, FALSE),
             hjust = ifelse(abs(.data$corr) < max(abs(.data$corr))/1.5, -0.1, 1.1)) %>%
      ggplot(aes(x = reorder(.data$variables, abs(.data$corr)), 
                 y = abs(.data$corr), fill = .data$pos, 
                 label = signif(100*.data$corr, 3))) +
      geom_hline(yintercept = 0, alpha = 0.5) +
      geom_col(colour = "transparent") + coord_flip() + 
      geom_text(aes(hjust = .data$hjust), size = 3, colour = "black") +
      scale_fill_manual(values = c("FALSE" = "#E5586E", "TRUE" = "#59B3D2")) +
      guides(fill = FALSE) +
      labs(title = paste("Correlations of", var, "[%]"), x = NULL, y = NULL) +
      scale_y_percent(expand = c(0, 0), position = "right") + 
      theme_lares2(pal = 2)
    
    if (!is.na(top) & top < original_n) p <- p + 
        labs(subtitle = paste(
          "Top", top, "out of", original_n, "variables (original & dummy)"))
    
    if (max_pvalue < 1) 
      p <- p + labs(caption = paste("Correlations with p-value <", max_pvalue))
  }
  
  if (!is.na(subdir)) {
    dir.create(file.path(getwd(), subdir), recursive = T)
    file_name <- paste(subdir, file_name, sep = "/")
  }
  
  if (save) p <- p + ggsave(file_name, width = 6, height = 6)
  if (plot) return(p) else return(d)
}


####################################################################
#' Ranked Cross-Correlation
#'
#' This function creates a correlation full study and returns a rank
#' of the highest correlation variables obtained in a cross-table.
#' 
#' DataScience+ Post: \href{https://bit.ly/2WiLsQB}{Find Insights 
#' with Ranked Cross-Correlations}
#'
#' @family Correlations
#' @family Exploratory
#' @param df Dataframe.
#' @param plot Boolean. Show and return a plot?
#' @param max_pvalue Numeric. Filter non-significant variables. Range (0, 1]
#' @param type Integer. Plot type. 1 is for overall rank. 2 is for local rank.
#' @param max Numeric. Maximum correlation permitted (from 0 to 1)
#' @param top Integer. Return top n results only. Only valid when type = 1. Set
#' value to NA to use all cross-correlations
#' @param local Integer. Label top n local correlations. Only valid when type = 2
#' @param ignore Character vector. Which columns do you wish to exlude?
#' @param contains Character vector. Filter cross-correlations 
#' with variables that contains certain strings (using any value if vector used).
#' @param grid Boolean. Separate into grids?
#' @param rm.na Boolean. Remove NAs?
#' @param dummy Boolean. Should One Hot Encoding be applied to categorical columns? 
#' @param limit Integer. Limit one hot encoding to the n most frequent 
#' values of each column. Set to \code{NA} to ignore argument.
#' @param redundant Boolean. Should we keep redundant columns? i.e. It the
#' column only has two different values, should we keep both new columns?
#' @param method Character. Any of: c("pearson", "kendall", "spearman")
#' @examples 
#' options("lares.font" = NA) # Temporal
#' data(dft) # Titanic dataset
#' 
#' # Only data with no plot
#' corr_cross(dft, plot = FALSE, top = 10)
#' 
#' # Show only most relevant results filtered by pvalue
#' corr_cross(dft, rm.na = TRUE, max_pvalue = 0.05, top = 15)
#' 
#' # Cross-Correlation max values per category
#' corr_cross(dft, type = 2, top = NA)
#' 
#' # Cross-Correlation for certain variables
#' corr_cross(dft, contains = c("Survived", "Fare"))
#' @export
corr_cross <- function(df, plot = TRUE, 
                       max_pvalue = 1,
                       type = 1, max = 1, top = 25, local = 1,
                       ignore = NA, contains = NA, grid = FALSE,
                       rm.na = FALSE, dummy = TRUE, 
                       limit = 10, redundant = FALSE,
                       method = "pearson") {
  
  check_opts(type, 1:2)
  
  if (sum(is.na(df)) & rm.na == FALSE) 
    warning("There are NA values in your data!")
  
  cor <- corr(df, ignore = ignore, dummy = dummy, limit = limit,
              redundant = redundant, method = method, pvalue = TRUE)
  
  aux <- function(c) {
    data.frame(gather(c)) %>% 
      mutate(mix = rep(colnames(c), length(c))) %>%
      mutate(p1 = rep(1:length(c), each = length(c)),
             p2 = rep(1:length(c), length(c)),
             aux = .data$p2 - .data$p1) %>% 
      filter(.data$aux > 0) %>%
      mutate(rel = abs(.data$value)) %>% 
      filter(.data$rel < max) %>% 
      arrange(desc(.data$rel)) %>%
      {if (!is.na(contains[1])) 
        filter(., grepl(paste(
          contains, collapse = ifelse(length(contains) > 1, "|", "")), 
          paste(.data$mix, .data$key))) else .} %>%
      {if (rm.na) filter(., !grepl("_NAs", .data$mix)) else .} %>%
      filter(!grepl("_OTHER", .data$key)) %>%
      rename(corr = .data$value) %>%
      mutate(value = paste(.data$key, .data$mix)) %>%
      select(.data$key, .data$mix, .data$corr)
  }
  
  ret <- aux(cor$cor)
  aux <- aux(cor$pvalue)
  ret <- left_join(ret, rename(aux, pvalue = .data$corr), c("key", "mix")) %>%
    mutate(pvalue = as.numeric(ifelse(is.na(.data$pvalue), 1, .data$pvalue))) %>%
    filter(.data$pvalue <= max_pvalue)
  
  for (i in 1:ncol(df)) {
    if (i == 1) 
      ret <- mutate(ret, group1 = "fill", group2 = "fill")
    group <- colnames(df)[i]
    aux <- ifelse(grepl(group, ret$key), group, "fill")
    ret$group1 <- ifelse(ret$group1 == "fill", aux, ret$group1)
    aux <- ifelse(grepl(group, ret$mix), group, "fill")
    ret$group2 <- ifelse(ret$group2 == "fill", aux, ret$group2)
  }
  ret <- filter(ret, .data$group1 != .data$group2)
  if (nrow(ret) > top & !is.na(top)) {
    message(sprintf("Returning only the top %s. You may override with the `top` parameter", top))
    ret <- slice(ret, 1:top) 
  }
  
  if (plot) {
    n <- ifelse(type == 1, top, local)
    n <- ifelse(n > nrow(ret), nrow(ret), n)
    subtitle <- paste(n, "most relevant")
    if (!is.na(contains)[1]) subtitle <- paste(subtitle, "containing", vector2text(contains))
    if (max < 1) subtitle <- paste0(subtitle," (excluding +", 100*max, "%)")
    if (rm.na) subtitle <- paste(subtitle, paste("[NAs removed]"))
    if (!is.na(contains[1])) ret <- ret %>%
      mutate(facet = gsub(vector2text(contains, sep = "|", quotes = FALSE), "", .data$mix)) %>%
      mutate(facet = gsub("_", "", .data$facet))
    
    if (type == 1) {
      p <- ret %>%
        head(top) %>%
        mutate(label = paste(.data$key, "+", .data$mix), 
               abs = abs(.data$corr),
               sign = ifelse(.data$corr < 0, "bad", "good")) %>%
        ggplot(aes(x = reorder(.data$label, .data$abs), 
                   y = .data$abs, 
                   fill = .data$sign)) +
        geom_col(colour = "transparent") +
        # geom_hline(aes(yintercept = 0), alpha = 0.5) + 
        geom_text(aes(label = signif(100*.data$corr, 3)), 
                  size = 3, colour = "white", hjust = 1.1) +
        coord_flip() + guides(fill = FALSE) +
        labs(title = "Ranked Cross-Correlations", 
             subtitle = subtitle,
             x = NULL, 
             y = "Correlation [%]") +
        scale_fill_manual(values = c("bad" = "#E5586E", "good" = "#59B3D2")) +
        scale_y_percent(expand = c(0, 0)) + 
        theme_lares2(legend = "top")
      if ((!is.na(contains)[1] & length(contains) == 1) | grid) {
        p <- p + facet_grid(.data$facet ~ ., scales = "free", space = "free")
      }
    }
    
    if (type == 2) {
      ret <- rbind(data.frame(ret, group = ret$group1), 
                   data.frame(ret, group = ret$group2)) %>%
        arrange(desc(abs(.data$corr)))
      aux <- position_jitter(width = 0.4, seed = 123)
      p <- ret %>% 
        group_by(.data$group) %>%
        mutate(hjust = ifelse(.data$corr > 0, 1, 0),
               size = abs(.data$corr),
               alpha = ifelse(row_number() <= local, 2, .data$size),
               label = ifelse(row_number() <= local, paste(.data$key, "+", .data$mix), "")) %>%
        ggplot(aes(x = .data$group, y = .data$corr, 
                   label = .data$label, colour = .data$group)) + 
        geom_jitter(aes(alpha = .data$alpha, size = .data$size), position = aux) + 
        geom_hline(yintercept = 0, alpha = 0.3) +
        geom_text(aes(hjust = .data$hjust), size = 2.9, position = aux, colour = "black") +
        guides(colour = FALSE, alpha = FALSE, size = FALSE) +
        scale_size(range = c(0.4, 2)) +
        labs(x = NULL, y = "Correlation [%]", 
             subtitle = subtitle,
             title = "Local Cross-Correlations") +
        scale_y_continuous(labels = function(x) formatNum(x, 0, pos = "%")) + 
        coord_flip() +
        theme_lares2(pal = 2) 
    }
    if (max_pvalue < 1) 
      p <- p + labs(caption = paste("Correlations with p-value <", max_pvalue))
    return(p)
  }
  return(ret)
}

