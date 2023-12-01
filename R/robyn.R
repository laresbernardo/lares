####################################################################
#' Robyn: Generate default hyperparameters
#'
#' Generate a list with hyperparameter defaults values, ready to be
#' passed to \code{Robyn::robyn_inputs()}.
#'
#' @family Robyn
#' @param paid_media_spends Character vector. Paid media variables names.
#' @param media_type Character vector. Must be ength 1 or same as
#' \code{paid_media_spends}. Pick, for every \code{paid_media_spends} value,
#' what type of media it is: "online" or "offline".
#' @param lagged Boolean vector. Must be ength 1 or same as
#' \code{paid_media_spends}. Pick, for every \code{paid_media_spends} value,
#' if you wish to have a lagged effect. Only valid for Weibull adstock.
#' @param adstock Character. Pick one of: "geometric" or "weibull".
#' @param date_type Character. Pick one of: "daily", "weekly, or "monthly".
#' Only valid to transform thetas when using geometric adstock.
#' @return list with default hyperparameters ranges.
#' @examples
#' robyn_hypsbuilder(
#'   paid_media_spends = c(
#'     "branded_search_spend",
#'     "nonbranded_search_spend",
#'     "facebook_spend",
#'     "print_spend",
#'     "ooh_spend",
#'     "tv_spend",
#'     "radio_spend"),
#'   media_type = c(
#'     "online", "online", "online",
#'     "offline", "offline", "offline", "offline"),
#'   adstock = "geometric",
#'   date_type = "weekly")
#' @export
robyn_hypsbuilder <- function(
    paid_media_spends,
    media_type = "default",
    lagged = FALSE,
    adstock = "geometric",
    date_type = "weekly") {
  
  # Check inputs validity
  check_opts(media_type, c("default", "online", "offline"))
  check_opts(adstock, c("geometric", "weibull", "weibull_pdf", "weibull_cdf"))
  check_opts(date_type, c("daily", "weekly", "monthly"))
  check_opts(lagged, c(TRUE, FALSE))
  
  # Hyperparameters names
  hyps <- c("alphas", "gammas")
  hyps2 <- if (adstock %in% "geometric") "thetas" else c("shapes", "scales")
  all_hyps <- c(hyps, hyps2)
  
  # Repeat to all channels when provided 1 value
  if (length(media_type) == 1)
    media_type <- rep(media_type, length(paid_media_spends))
  if (length(lagged) == 1)
    lagged <- rep(lagged, length(paid_media_spends))
  if (any(lagged) && adstock == "geometric")
    stop("To be able to have a lagged effect you need to set Weibull adstock")
  
  # Generate all combinations and data.frame
  df <- expand.grid(paid_media_spends, all_hyps)
  df$media_type <- rep(media_type, length(all_hyps))
  df$lagged <- rep(lagged, length(all_hyps))
  
  # Apply default rules
  df <- df %>%
    mutate(low = case_when(
      .data$Var2 == "alphas" ~ 0.01,
      .data$Var2 == "gammas" ~ 0.3,
      .data$Var2 == "thetas" ~ 0.1,
      .data$Var2 == "shapes" & isTRUE(.data$lagged) ~ 2,
      .data$Var2 == "shapes" ~ 0,
      .data$Var2 == "scales" ~ 0
    )) %>%
    mutate(high = case_when(
      .data$Var2 == "alphas" & .data$media_type == "online" ~ 3,
      .data$Var2 == "alphas" & .data$media_type == "offline" ~ 1,
      .data$Var2 == "alphas" & .data$media_type == "default" ~ 3,
      .data$Var2 == "gammas" ~ 1,
      .data$Var2 == "thetas" ~ 0.5,
      .data$Var2 == "shapes" & isTRUE(.data$lagged) ~ 10,
      .data$Var2 == "shapes" & isFALSE(.data$lagged) ~ 1,
      .data$Var2 == "shapes" ~ 10,
      .data$Var2 == "scales" & isTRUE(.data$lagged) ~ 0.05,
      .data$Var2 == "scales" & isFALSE(.data$lagged) ~ 0.2,
      .data$Var2 == "scales" ~ 0.2
    )) %>%
    mutate(
      low = case_when(
        date_type == "daily" ~ .data$low^(1/7),
        date_type == "monthly" ~ .data$low^(4),
        TRUE ~ .data$low
      ),
      high = case_when(
        date_type == "daily" ~ .data$high^(1/7),
        date_type == "monthly" ~ .data$high^(4),
        TRUE ~ .data$high
      )
    ) %>%
    mutate(Var1 = factor(.data$Var1, levels = paid_media_spends)) %>%
    arrange(.data$Var1)
  
  # Return as named list
  out <- lapply(seq_along(df$Var1), function(x) c(df$low[x], df$high[x]))
  names(out) <- paste(df$Var1, df$Var2, sep = "_")
  out <- append(out, list(train_size = c(0.5, 0.8)))
  return(out)
}


####################################################################
#' Robyn: Model Selection Viz 
#'
#' Consider N models per cluster to select the right one using
#' several metrics to consider such as potential improvement on budget
#' allocator and how many non-zero coefficients there are.
#'
#' @family Robyn
#' @inheritParams corr_var
#' @param InputCollect,OutputCollect Robyn output objects.
#' @param n_per_cluster Integer. How many models per cluster do you
#' want to plot? Default: 5. Keep in mind they will all be considered
#' for the calculations.
#' @param wt Vector. Length 5. Pick the weight for each of the normalized
#' metrics to calculate the score and rank models. In order: R2, Performance
#' (ROAS or CPA), Potential Performance Boost, Non-Zero Coefficients, Amount
#' of models in a cluster.
#' @param cache Use cache functionality for allocator's results?
#' @param ... Additional parameters passed
#' @return list with data.frame and plot.
#' @export
robyn_modelselector <- function(
    InputCollect,
    OutputCollect,
    n_per_cluster = 5,
    wt = c(2, 1, 1, 1, 0.1),
    quiet = FALSE,
    cache = TRUE,
    ...) {
  
  stopifnot(length(wt) == 5)
  
  # Add Default Potential Improvement values
  if (wt[3] != 0) {
    sols <- sort(OutputCollect$allSolutions)
    cache_name <- c("robyn_modelselector", length(sols))
    if (cache & cache_exists(cache_name, ...)) {
      potOpt <- cache_read(cache_name, quiet = quiet, ...)
    } else {
      if (!quiet) message(sprintf(
        ">>> Calculating potential performance improvements on %s Pareto-front models...",
        length(sols)))
      try_require("Robyn")
      defpot <- lapply(sols, function(x){
        if (!quiet) statusbar(which(sols == x), length(sols), x)
        return(suppressMessages(robyn_allocator(
          InputCollect = InputCollect,
          OutputCollect = OutputCollect,
          select_model = x,
          channel_constr_low = 0.5,
          channel_constr_up = 1.5,
          plots = FALSE,
          export = FALSE,
          quiet = TRUE
        )))
      })
      potOpt <- data.frame(
        solID = sols,
        potential_improvement = unlist(lapply(defpot, function(x)
          x$dt_optimOut$optmResponseUnitTotalLift[1])))
      if (cache) cache_write(potOpt, cache_name, quiet = quiet, ...)
    }
  } else {
    potOpt <- data.frame(solID = sols, potential_improvement = 0)
  }
  
  # Check pareto-front models summaries to calculate performance (ROAS/CPA)
  performance <- OutputCollect$allPareto$xDecompAgg %>%
    filter(!is.na(.data$mean_spend)) %>%
    filter(.data$solID %in% OutputCollect$clusters$data$solID) %>%
    group_by(.data$solID) %>%
    summarise(perf = ifelse(
      InputCollect$dep_var_type == "revenue",
      sum(.data$xDecompAgg)/sum(.data$total_spend) - 1,
      sum(.data$total_spend)/sum(.data$xDecompAgg) - 1),
      non_zeroes = 1 - length(.data$rn[which(round(.data$coef, 6) == 0)])/length(.data$rn),
      top_channels = paste(.data$rn[head(rank(-.data$roi_total, ties.method = "first"), 3)], collapse = ", "),
      zero_coef = paste(.data$rn[which(round(.data$coef, 6) == 0)], collapse = ", ")) %>%
    left_join(potOpt, "solID")
  temp <- OutputCollect$clusters$data %>%
    select(.data$solID, .data$cluster, .data$top_sol) %>%
    mutate(cluster = as.character(.data$cluster)) %>%
    left_join(select(OutputCollect$clusters$clusters_means, .data$cluster, .data$n), "cluster") %>%
    rename(sols_in_cluster = "n")
  # Gather everything up
  dfa <- OutputCollect$allPareto$resultHypParam %>%
    filter(.data$solID %in% OutputCollect$clusters$data$solID) %>%
    select(.data$solID, .data$rsq_train) %>% #nrmse, decomp.rssd) %>%
    left_join(performance, "solID") %>%
    left_join(temp, "solID") %>% ungroup() %>%
    mutate(
      score = normalize(
        normalize(.data$rsq_train) * wt[1] + # Most important metric
          normalize(.data$perf) * wt[2] +
          normalize(.data$potential_improvement) * wt[3] +
          normalize(.data$non_zeroes) * wt[4] +
          normalize(.data$sols_in_cluster) * wt[5]), # Some weight to avoid outliers
      note = case_when(
        rank(-.data$score) == 1 ~ "****",
        rank(-.data$score) == 2 ~ "***",
        rank(-.data$score) == 3 ~ "**",
        rank(-.data$score) == 4 ~ "*",
        TRUE ~ ""
      )) %>%
    arrange(desc(.data$score))
  if (!quiet) message("Recommended considering these models first: ", v2t(head(dfa$solID, 4)))
  
  # Metrics Used
  metrics <- c("rsq_train", "perf", "potential_improvement", "non_zeroes")
  metrics_names <- c(
    "R2", ifelse(InputCollect$dep_var_type == "revenue", "ROAS", "CPA"),
    "Potential Boost", "Non-Zeroes")
  caption <- data.frame(metric = c(metrics_names, "Models per Cluster"), value = wt/sum(wt)) %>%
    mutate(lab = sprintf("%s: %s%%", .data$metric, round(100 * .data$value))) %>%
    pull(.data$lab) %>%
    paste(., collapse = " | ")
  
  # Generate plot/dashboard
  p <- dfa %>%
    tidyr::pivot_longer(all_of(metrics)) %>%
    mutate(name_metrics = rep(metrics_names, length.out = nrow(.))) %>%
    mutate(name = factor(.data$name, levels = metrics),
           name_metrics = factor(.data$name_metrics, levels = metrics_names)) %>%
    group_by(.data$name) %>%
    mutate(top = rank(-.data$value)) %>%
    left_join(select(dfa, .data$solID, .data$rsq_train), "solID") %>%
    group_by(.data$cluster) %>% 
    slice(1:(length(metrics) * n_per_cluster)) %>%
    mutate(solID = paste(.data$solID, .data$note)) %>%
    ggplot(aes(y = reorder(.data$solID, .data$rsq_train), x = .data$value)) +
    geom_col(aes(group = .data$name, fill = .data$top)) +
    # geom_vline(xintercept = 1, alpha = 0.5) +
    facet_grid(sprintf("%s (%s)", .data$cluster, .data$sols_in_cluster) ~
                 .data$name_metrics, scales = "free") +
    labs(x = NULL, y = NULL,
         title = "Select a model based on these metrics and rankings",
         subtitle = paste(
           "Showing up to", n_per_cluster,
           "models per cluster and best combined solutions marked with stars"),
         caption = paste("Weights:", caption)) +
    scale_x_percent() +
    theme_lares(legend = FALSE)
  
  # Create the exported object
  ret <- invisible(list(
    data = select(dfa, "solID", all_of(metrics), everything()),
    plot = p))
  class(ret) <- c("robyn_modelselector", class(ret))
  return(ret)
}

#' @param x robyn_modelselector object
#' @rdname robyn_modelselector
#' @export
plot.robyn_modelselector <- function(x, ...) {
  return(x$plot)
}
