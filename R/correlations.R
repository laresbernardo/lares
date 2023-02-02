####################################################################
#' Correlation table
#'
#' This function correlates a whole dataframe, running one hot smart
#' encoding (\code{ohse}) to transform non-numerical features.
#' Note that it will automatically suppress columns
#' with less than 3 non missing values and warn the user.
#'
#' @family Calculus
#' @family Correlations
#' @inheritParams ohse
#' @inheritParams numericalonly
#' @param df Dataframe. It doesn't matter if it's got non-numerical
#' columns: they will be filtered.
#' @param method Character. Any of: c("pearson", "kendall", "spearman").
#' @param use Character. Method for computing covariances in the presence
#' of missing values. Check \code{stats::cor} for options.
#' @param pvalue Boolean. Returns a list, with correlations and statistical
#' significance (p-value) for each value.
#' @param padjust Character. NULL to skip or any of \code{p.adjust.methods} to
#' calculate adjust p-values for multiple comparisons using \code{p.adjust()}.
#' @param half Boolean. Return only half of the matrix? The redundant
#' symmetrical correlations will be \code{NA}.
#' @param dec Integer. Number of decimals to round correlations and p-values.
#' @param dummy Boolean. Should One Hot (Smart) Encoding (\code{ohse()})
#' be applied to categorical columns?
#' @param top Integer. Select top N most relevant variables? Filtered
#' and sorted by mean of each variable's correlations.
#' @param ... Additional parameters passed to \code{ohse}, \code{corr},
#' and/or \code{cor.test}.
#' @return data.frame. Squared dimensions (N x N) to match every
#' correlation between every \code{df} data.frame column/variable. Notice
#' that when using \code{ohse()} you may get more dimensions.
#' @examples
#' data(dft) # Titanic dataset
#' df <- dft[, 2:5]
#'
#' # Correlation matrix (without redundancy)
#' corr(df, half = TRUE)
#'
#' # Ignore specific column
#' corr(df, ignore = "Pclass")
#'
#' # Calculate p-values as well
#' corr(df, pvalue = TRUE, limit = 1)
#'
#' # Test when no more than 2 non-missing values
#' df$trash <- c(1, rep(NA, nrow(df) - 1))
#' # and another method...
#' corr(df, method = "spearman")
#' @export
corr <- function(df, method = "pearson",
                 use = "pairwise.complete.obs",
                 pvalue = FALSE,
                 padjust = NULL,
                 half = FALSE,
                 dec = 6,
                 ignore = NULL,
                 dummy = TRUE,
                 redundant = NULL,
                 logs = FALSE,
                 limit = 10,
                 top = NA,
                 ...) {
  # Ignored columns
  if (isTRUE(is.na(ignore)[1])) ignore <- NULL
  df <- select(df, -any_of(ignore))

  # One hot encoding for categorical features
  if (dummy) {
    df <- ohse(df, quiet = TRUE, limit = limit, redundant = redundant, ...)
  }

  # Select only numerical features and create log+1 for each one
  d <- numericalonly(df, logs = logs)

  # Drop columns with not enough data to calculate correlations / p-values
  miss <- missingness(d, summary = FALSE)
  if (is.data.frame(miss)) {
    toDrop <- miss %>%
      mutate(drop = nrow(d) - .data$missing < 3L) %>%
      filter(.data$drop) %>%
      pull(.data$variable)
    if (length(toDrop) > 0) {
      warning("Dropped columns with less than 3 non-missing values: ", v2t(toDrop))
      d <- select(d, -one_of(toDrop))
    }
  }

  # Avoid sd = 0 warning:
  # In cor(x, y) : the standard deviation is zeroIn cor(x, y) : the standard deviation is zero
  d <- Filter(function(x) sd(x, na.rm = TRUE) != 0, d)

  # Correlations
  rs <- suppressWarnings(cor(d, method = method, use = use))
  if (half) for (i in seq_along(rs[, 1])) rs[1:i, i] <- NA
  cor <- round(data.frame(rs), dec)
  colnames(cor) <- row.names(cor) <- colnames(d)

  # Top N
  if (!is.na(top)) {
    message(paste("Returning the top", top, "variables only..."))
    imp <- cor %>%
      summarise_all(list(~ mean(.))) %>%
      t() %>%
      data.frame(variable = row.names(.), mean = abs(.)) %>%
      arrange(desc(abs(.data$mean)))
    which <- as.vector(imp$variable[1:top])
    cor <- cor[which, which]
  }

  # Statistical significance (p-value)
  if (pvalue) {
    output <- list(cor = cor, pvalue = .cor_test_p(d, method = method, ...))
    if (!is.null(padjust)) {
      output[["pvalue_adj"]] <- matrix(
        p.adjust(output$pvalue, method = padjust),
        ncol = ncol(output$cor),
        dimnames = list(names(output$cor), names(output$cor))
      )
    }
    return(output)
  }

  return(cor)
}


####################################################################
#' Correlation between variable and dataframe
#'
#' This function correlates a whole dataframe with a single feature. It
#' automatically runs \code{ohse} (one-hot-smart-encoding) so no need to input
#' only numerical values.
#'
#' @family Exploratory
#' @family Correlations
#' @inheritParams corr
#' @param var Variable. Name of the variable to correlate. Note that if the
#' variable \code{var} is not numerical, 1. you may define which category to select
#' from using `var_category`; 2. You may have to add \code{redundant = TRUE} to
#' enable all categories (instead of \code{n-1}).
#' @param ignore Character vector. Which columns do you wish to exclude?
#' @param trim Integer. Trim words until the nth character for
#' categorical values (applies for both, target and values)
#' @param clean Boolean. Use lares::cleanText for categorical values (applies
#' for both, target and values)
#' @param plot Boolean. Do you wish to plot the result? If set to TRUE, the
#' function will return only the plot and not the result's data
#' @param top Integer. If you want to plot the top correlations,
#' define how many
#' @param ceiling Numeric. Remove all correlations above... Range: (0-1]
#' @param max_pvalue Numeric. Filter non-significant variables. Range (0, 1]
#' @param limit Integer. Limit one hot encoding to the n most frequent
#' values of each column. Set to \code{NA} to ignore argument.
#' @param ranks Boolean. Add ranking numbers?
#' @param zeroes Do you wish to keep zeroes in correlations too?
#' @param save Boolean. Save output plot into working directory
#' @param quiet Boolean. Keep quiet? If not, show messages
#' @param ... Additional parameters passed to \code{corr} and \code{cor.test}
#' @return data.frame. With variables, correlation and p-value results
#' for each feature, arranged by descending absolute correlation value.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dft) # Titanic dataset
#'
#' corr_var(dft, Survived, method = "spearman", plot = FALSE, top = 10)
#'
#' # With plots, results are easier to compare:
#'
#' # Correlate Survived with everything else and show only significant results
#' dft %>% corr_var(Survived_TRUE, max_pvalue = 0.05)
#'
#' # Top 15 with less than 50% correlation and show ranks
#' dft %>% corr_var(Survived_TRUE, ceiling = .6, top = 15, ranks = TRUE)
#' @export
corr_var <- function(df, var,
                     ignore = NULL,
                     trim = 0,
                     clean = FALSE,
                     plot = TRUE,
                     top = NA,
                     ceiling = 1,
                     max_pvalue = 1,
                     limit = 10,
                     ranks = FALSE,
                     zeroes = FALSE,
                     save = FALSE,
                     quiet = FALSE,
                     ...) {
  vars <- enquos(var)
  var <- as_label(vars[[1]])
  df <- select(df, -contains(paste0(var, "_log")))

  # Calculate correlations
  rs <- corr(df, half = FALSE, ignore = ignore, limit = limit, pvalue = TRUE, ...)
  if (is.data.frame(rs)) rs <- list(cor = rs, pvalue = mutate_all(rs, ~1))

  # Check if main variable exists
  if (!var %in% colnames(rs$cor)) {
    msg <- paste("Not a valid input:", var, "was transformed or does not exist.")
    maybes <- colnames(rs$cor)[grepl(var, colnames(rs$cor))]
    if (length(maybes) > 0 && maybes[1] %in% colnames(rs$cor)) {
      msg <- sprintf("%s\n  >> Automatically using '%s'", msg, maybes[1])
      var <- maybes[1]
      fixable <- TRUE
    } else {
      fixable <- FALSE
    }
    if (fixable) warning(msg) else stop(msg)
  }

  d <- as_tibble(data.frame(
    variables = colnames(rs$cor),
    corr = rs$cor[, c(var)],
    pvalue = rs$pvalue[, c(var)]
  ))
  if ("pvalue_adj" %in% names(rs)) {
    d$pvalue_adj <- rs$pvalue_adj[, c(var)]
  }
  d <- d[(d$corr < 1 & !is.na(d$corr)), ]
  d <- d[order(-abs(d$corr)), ]

  original_n <- nrow(d)

  if (!zeroes) d <- d[d$corr != 0, ]

  # Suppress non-statistical significant correlations
  if ("pvalue_adj" %in% colnames(d)) {
    d <- d %>%
      mutate(max_pvalue = as.numeric(ifelse(is.na(.data$pvalue_adj), 1, .data$pvalue_adj))) %>%
      filter(.data$pvalue_adj <= max_pvalue)
    message(">>> Filtered 'max_pvalue' using adjusted p-values")
  } else {
    d <- d %>%
      mutate(pvalue = as.numeric(ifelse(is.na(.data$pvalue), 1, .data$pvalue))) %>%
      filter(.data$pvalue <= max_pvalue)
  }
  if (nrow(d) == 0) warning("Check your 'max_pvalue' input: might be too low!")

  # Limit automatically when more than 20 observations
  if (is.na(top) && nrow(d) > 20) {
    top <- 20
    if (!quiet) message(paste(">>> Reduced results to", top, "largest correlations. Set by 'top' parameter"))
  }

  if (ceiling < 1) {
    d <- d[abs(d$corr) < ceiling, ]
    if (!quiet) message(paste0("Removing all correlations greater than ", 100 * ceiling, "% (absolute)"))
  }

  d <- d[complete.cases(d), ]

  if (!is.na(top)) d <- head(d, top)

  # Shorten up the long names of some variables
  if (trim > 0) {
    d$variables <- substr(d$variables, 1, trim)
    if (!quiet) message(paste("Trimmed all name values into", trim, "characters"))
  }

  # Add ranking numbers
  if (ranks) {
    d <- mutate(d, variables = sprintf("%s. %s", row_number(), .data$variables))
  }

  class(d) <- c("corr_var", class(d))

  if (plot) {
    p <- plot(d, var, max_pvalue = max_pvalue, top = top, limit = original_n)
    return(p)
  }
  return(d)
}

#' @param x corr_var object
#' @rdname corr_var
#' @export
plot.corr_var <- function(x, var, max_pvalue = 1, top = NA, limit = NULL, ...) {
  p <- ungroup(x) %>%
    filter(!.data$variables %in% c("pvalue", "pvalue_adj")) %>%
    mutate(
      pos = ifelse(.data$corr > 0, "positive", "negative"),
      hjust = ifelse(abs(.data$corr) < max(abs(.data$corr)) / 1.5, -0.1, 1.1)
    ) %>%
    ggplot(aes(
      x = reorder(.data$variables, abs(.data$corr)),
      y = abs(.data$corr), fill = .data$pos,
      label = sub("^(-)?0[.]", "\\1.", signif(.data$corr, 3))
    )) +
    geom_hline(yintercept = 0, alpha = 0.5) +
    geom_col(colour = "transparent") +
    coord_flip() +
    geom_text(aes(hjust = .data$hjust), size = 3, colour = "black") +
    guides(fill = "none") +
    labs(title = paste("Correlations of", var), x = NULL, y = NULL) +
    scale_y_continuous(
      expand = c(0, 0), position = "right",
      labels = function(x) sub("^(-)?0[.]", "\\1.", x)
    ) +
    theme_lares(pal = 4)

  if (is.null(limit)) limit <- 100
  if (!is.na(top) && top < limit) {
    p <- p + labs(subtitle = paste(top, "largest correlation variables (original & dummy)"))
  }

  if (max_pvalue < 1) {
    p <- p + labs(caption = paste("Correlations with p-value <", max_pvalue))
  }
  return(p)
}


####################################################################
#' Ranked cross-correlation across all variables
#'
#' This function creates a correlation full study and returns a rank
#' of the highest correlation variables obtained in a cross-table.
#'
#' DataScience+ Post:
#' \href{https://datascienceplus.com/find-insights-with-ranked-cross-correlations/}{Find
#' Insights with Ranked Cross-Correlations}
#'
#' @family Correlations
#' @family Exploratory
#' @inheritParams corr
#' @inheritParams corr_var
#' @param plot Boolean. Show and return a plot?
#' @param max_pvalue Numeric. Filter non-significant variables. Range (0, 1]
#' @param type Integer. Plot type. 1 is for overall rank. 2 is for local rank.
#' @param max Numeric. Maximum correlation permitted (from 0 to 1)
#' @param top Integer. Return top n results only. Only valid when type = 1. Set
#' value to NA to use all cross-correlations
#' @param local Integer. Label top n local correlations. Only valid when type = 2
#' @param contains Character vector. Filter cross-correlations
#' with variables that contains certain strings (using any value if vector used).
#' @param grid Boolean. Separate into grids?
#' @param rm.na Boolean. Remove NAs?
#' @param ... Additional parameters passed to \code{corr}
#' @return Depending on input \code{plot}, we get correlation and p-value results for
#' every combination of features, arranged by descending absolute correlation value,
#' with a data.frame \code{plot = FALSE} or plot \code{plot = TRUE}.
#' @examples
#' Sys.unsetenv("LARES_FONT") # Temporal
#' data(dft) # Titanic dataset
#'
#' # Only data with no plot
#' corr_cross(dft, plot = FALSE, top = 10)
#'
#' # Show only most relevant results filtered by pvalue
#' corr_cross(dft, rm.na = TRUE, max_pvalue = 0.05, top = 15)
#'
#' # Cross-Correlation for certain variables
#' corr_cross(dft, contains = c("Survived", "Fare"))
#'
#' # Cross-Correlation max values per category
#' corr_cross(dft, type = 2, top = NA)
#' @export
corr_cross <- function(df, plot = TRUE,
                       pvalue = TRUE, max_pvalue = 1,
                       type = 1, max = 1, top = 25, local = 1,
                       ignore = NULL, contains = NA, grid = TRUE,
                       rm.na = FALSE, quiet = FALSE,
                       ...) {
  check_opts(type, 1:2)

  cor <- corr(df, half = TRUE, ignore = ignore, pvalue = pvalue, ...)

  cluster <- "cluster_" %in% contains
  if (!is.data.frame(cor)) {
    ret <- .transf(cor$cor, max = max, contains = contains, cluster = cluster, rm.na = rm.na)
    aux <- .transf(cor$pvalue, max = max, contains = contains, cluster = cluster, rm.na = rm.na)
  } else {
    ret <- aux <- .transf(cor, max = max, contains = contains, cluster = cluster, rm.na = rm.na)
    aux$corr <- 0
  }

  ret <- left_join(ret, rename(aux, pvalue = .data$corr), c("key", "mix")) %>%
    mutate(pvalue = as.numeric(ifelse(is.na(.data$pvalue), 1, .data$pvalue))) %>%
    filter(.data$pvalue <= max_pvalue)

  for (i in seq_along(df)) {
    if (i == 1) {
      ret <- mutate(ret, group1 = "fill", group2 = "fill")
    }
    group <- colnames(df)[i]
    aux <- ifelse(grepl(group, ret$key), group, "fill")
    ret$group1 <- ifelse(ret$group1 == "fill", aux, ret$group1)
    aux <- ifelse(grepl(group, ret$mix), group, "fill")
    ret$group2 <- ifelse(ret$group2 == "fill", aux, ret$group2)
  }
  ret <- filter(ret, .data$group1 != .data$group2)
  if (nrow(ret) > top && !is.na(top) && !quiet && type != 2) {
    message(sprintf("Returning only the top %s. You may override with the 'top' argument", top))
    ret <- slice(ret, 1:top)
  }

  ret <- ret %>%
    rowwise() %>%
    mutate(
      cat1 = gsub(paste0(.data$group1, "_"), "", .data$key),
      cat2 = gsub(paste0(.data$group2, "_"), "", .data$mix)
    ) %>%
    select(1:4, .data$group1, .data$cat1, .data$group2, .data$cat2)

  if (plot) {
    n <- ifelse(type == 1, top, local)
    n <- ifelse(n > nrow(ret), nrow(ret), n)
    subtitle <- paste(n, "most relevant")
    if (!is.na(contains)[1]) subtitle <- paste(subtitle, "containing", vector2text(contains))
    if (max < 1) subtitle <- paste0(subtitle, " (excluding +", 100 * max, "%)")
    if (rm.na) subtitle <- paste(subtitle, paste("[NAs removed]"))
    if (!is.na(contains[1])) {
      ret <- ret %>%
        mutate(facet = gsub(vector2text(contains, sep = "|", quotes = FALSE), "", .data$mix)) %>%
        mutate(facet = gsub("_", "", .data$facet))
    }

    if (type == 1) {
      p <- ret %>%
        head(top) %>%
        ungroup() %>%
        mutate(
          label = paste(.data$key, "+", .data$mix),
          abs = abs(.data$corr),
          sign = ifelse(.data$corr < 0, "negative", "positive"),
          hjust = ifelse(.data$abs < max(.data$abs) / 1.5, -0.1, 1.1)
        ) %>%
        ggplot(aes(
          x = reorder(.data$label, .data$abs),
          y = .data$abs,
          label = sub("^(-)?0[.]", "\\1.", signif(.data$corr, 3))
        )) +
        geom_col(aes(fill = .data$sign)) +
        coord_flip() +
        geom_text(aes(hjust = .data$hjust), size = 3, colour = "black") +
        guides(fill = "none", colour = "none") +
        labs(
          title = "Ranked Cross-Correlations",
          subtitle = subtitle,
          x = NULL, y = NULL
        ) +
        scale_y_continuous(
          expand = c(0, 0), position = "right",
          labels = function(x) sub("^(-)?0[.]", "\\1.", x)
        ) +
        theme_lares(pal = 4, ...)

      if ((!is.na(contains)[1] && length(contains) == 1) && grid) {
        p <- p + facet_grid(.data$facet ~ ., scales = "free", space = "free")
      }
    }

    if (type == 2) {
      ret <- rbind(
        data.frame(ret, group = ret$group1),
        data.frame(ret, group = ret$group2)
      ) %>%
        arrange(desc(abs(.data$corr)))
      aux <- position_jitter(width = 0.4, seed = 123)
      p <- ret %>%
        group_by(.data$group) %>%
        mutate(
          hjust = ifelse(.data$corr > 0, 1, 0),
          size = abs(.data$corr),
          alpha = ifelse(row_number() <= local, 2, .data$size),
          label = ifelse(row_number() <= local, paste(.data$key, "+", .data$mix), "")
        ) %>%
        ggplot(aes(
          x = .data$group, y = .data$corr,
          label = .data$label, colour = .data$group
        )) +
        geom_jitter(position = aux, alpha = 0.4) +
        geom_hline(yintercept = 0, alpha = 0.3) +
        geom_text(aes(hjust = .data$hjust), size = 2.9, position = aux, colour = "black") +
        guides(colour = "none", alpha = "none", size = "none") +
        scale_size(range = c(0.4, 2)) +
        labs(
          x = NULL, y = "Correlation",
          subtitle = subtitle,
          title = "Local Cross-Correlations"
        ) +
        scale_y_continuous(labels = function(x) sub("^(-)?0[.]", "\\1.", x)) +
        coord_flip() +
        theme_lares(pal = 2)
    }
    if (max_pvalue < 1) {
      p <- p + labs(caption = paste("Correlations with p-value <", max_pvalue))
    }
    return(p)
  }
  return(ret)
}

.transf <- function(x, max = 1, contains = NA, cluster = FALSE, rm.na = FALSE) {
  aux <- gather(data.frame(x), "key", "value")
  ret <- mutate(aux, mix = rep(unique(aux$key), length = nrow(aux))) %>%
    mutate(p1 = cumsum(!duplicated(.data$mix))) %>%
    group_by(.data$mix) %>%
    mutate(p2 = row_number()) %>%
    ungroup() %>%
    filter(.data$p2 != .data$p1) %>%
    mutate(rel = abs(.data$value)) %>%
    filter(.data$rel < max) %>%
    arrange(desc(.data$rel)) %>%
    {
      if (!is.na(contains[1])) {
        filter(., grepl(
          paste(contains, collapse = "|"),
          paste(.data$mix)
        ))
      } else {
        .
      }
    } %>%
    {
      if (rm.na) filter(., !grepl("_NAs", .data$mix)) else .
    } %>%
    filter(!grepl("_OTHER", .data$key)) %>%
    rename(corr = .data$value) %>%
    mutate(value = paste(.data$key, .data$mix)) %>%
    select(.data$key, .data$mix, .data$corr)
  return(ret)
}


# https://stackoverflow.com/questions/60512043/r-creating-a-p-value-matrix-with-missing-values
.cor_test_p <- function(mat, method = "pearson", exact = TRUE, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      error <- try(tmp <- suppressWarnings(cor.test(
        mat[, i], mat[, j],
        method = method, exact = exact, ...
      )), silent = TRUE)
      if (inherits(error, "try-error")) {
        p.mat[i, j] <- NA
      } else {
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(p.mat)
}
