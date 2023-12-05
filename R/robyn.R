####################################################################
#' Robyn: Generate default hyperparameters
#'
#' Generate a list with hyperparameter default values, ready to be
#' passed to \code{Robyn::robyn_inputs()}.
#'
#' @family Robyn
#' @param channels Character vector. Paid media and organic variables names.
#' @param media_type Character vector. Must be ength 1 or same as
#' \code{channels}. Pick, for every \code{channels} value,
#' what type of media it is: "online" or "offline".
#' @param adstock Character. Pick one of: "geometric" or "weibull".
#' @param date_type Character. Pick one of: "daily", "weekly, or "monthly".
#' Only valid to transform thetas when using geometric adstock.
#' @param lagged Boolean vector. Must be ength 1 or same as
#' \code{channels}. Pick, for every \code{channels} value,
#' if you wish to have a lagged effect. Only valid for Weibull adstock.
#' @return list with default hyperparameters ranges.
#' @examples
#' robyn_hypsbuilder(
#'   channels = c(
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
    channels,
    media_type = "default",
    adstock = "geometric",
    date_type = "weekly",
    lagged = FALSE) {
  
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
    media_type <- rep(media_type, length(channels))
  if (length(lagged) == 1)
    lagged <- rep(lagged, length(channels))
  if (any(lagged) && adstock %in% c("geometric", "weibull_cdf"))
    stop("To be able to have a lagged effect you need to set 'weibull_pdf' adstock")
 
  # Generate all combinations and data.frame
  df <- expand.grid(channels, all_hyps)
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
    # Applies only for geometric adstock
    mutate(
      low = case_when(
        .data$Var2 == "thetas" & date_type == "daily"~ .data$low^(1/7),
        .data$Var2 == "thetas" & date_type == "monthly"~ .data$low^(4),
        TRUE ~ .data$low
      ),
      high = case_when(
        .data$Var2 == "thetas" & date_type == "daily"~ .data$high^(1/7),
        .data$Var2 == "thetas" & date_type == "monthly"~ .data$high^(4),
        TRUE ~ .data$high
      )
    ) %>%
    mutate(Var1 = factor(.data$Var1, levels = channels)) %>%
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
#' Consider N models per cluster to select the right ones to study using
#' several metrics to consider such as potential improvement on budget
#' allocator, how many non-zero coefficients there are, R squared,
#' historical performance, etc.
#'
#' @family Robyn
#' @inheritParams corr_var
#' @param InputCollect,OutputCollect Robyn output objects.
#' @param metrics Character vector. Which metrics do you want to consider?
#' Pick any combination from: "rsq_train" for trained R squared,
#' "performance" for ROAS or (inverse) CPA, "potential_improvement" for
#' default budget allocator improvement using \code{allocator_limits},
#' "non_zeroes" for non-zero beta coefficients, and "incluster_models" for
#' amount of models per cluster.
#' @param wt Vector. Weight for each of the normalized \code{metrics} selected,
#' to calculate the score and rank models. Must have the same order and length
#' of \code{metrics} parameter input.
#' @param top Integer. How many ranked models to star? The better the model
#' is, the more stars it will have marked.
#' @param allocator_limits Numeric vector, length 2. How flexible do you
#' want to be with the budget allocator? By default, we'll consider a
#' 0.5X and 2X range to let the budget shift across channels.
#' @param n_per_cluster Integer. How many models per cluster do you
#' want to plot? Default: 5. Keep in mind they will all be considered
#' for the calculations.
#' @param cache Use cache functionality for allocator's results?
#' @param ... Additional parameters passed
#' @return list with data.frame and plot.
#' @export
robyn_modelselector <- function(
    InputCollect,
    OutputCollect,
    metrics = c("rsq_train", "performance",
                "potential_improvement",
                "non_zeroes", "incluster_models"),
    wt = c(2, 1, 1, 1, 0.1),
    top = 4,
    n_per_cluster = 5,
    allocator_limits = c(0.5, 2),
    quiet = FALSE,
    cache = TRUE,
    ...) {
  
  stopifnot(length(wt) == length(metrics))
  stopifnot(length(allocator_limits) == 2)
  
  # Available metrics
  metrics_df <- data.frame(
    metric = c("rsq_train", "performance", "potential_improvement",
               "non_zeroes", "incluster_models", "nrmse", "decomp.rssd", "mape"),
    metric_name = c(
      "R^2", ifelse(InputCollect$dep_var_type == "revenue", "ROAS", "CPA (Inversed)"),
      "Potential Boost", "Non-Zeroes", "Models in Cluster",
      "NRMSE", "DECOMP.RSSD", "MAPE"))
  check_opts(metrics, metrics_df$metric)
  
  # Metrics Used
  metrics_used <- filter(metrics_df, .data$metric %in% metrics) %>%
    arrange(match(.data$metric, metrics)) %>%
    left_join(data.frame(metric = metrics, wt = wt), "metric")
  
  # Add Default Potential Improvement values
  if ("potential_improvement" %in% metrics & length(wt[which(metrics == "potential_improvement")]) != 0) {
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
          channel_constr_low = min(allocator_limits),
          channel_constr_up = max(allocator_limits),
          plots = FALSE,
          export = FALSE,
          quiet = TRUE,
          ...
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
    summarise(
      performance = sum(.data$xDecompAgg)/sum(.data$total_spend) - 1,
      non_zeroes = 1 - length(.data$rn[which(round(.data$coef, 6) == 0)])/length(.data$rn),
      top_channels = paste(.data$rn[head(rank(-.data$roi_total, ties.method = "first"), 3)], collapse = ", "),
      zero_coef = paste(.data$rn[which(round(.data$coef, 6) == 0)], collapse = ", ")) %>%
    left_join(potOpt, "solID")
  temp <- OutputCollect$clusters$data %>%
    select(.data$solID, .data$cluster, .data$top_sol) %>%
    mutate(cluster = as.character(.data$cluster)) %>%
    left_join(select(OutputCollect$clusters$clusters_means, .data$cluster, .data$n), "cluster") %>%
    rename(incluster_models = "n")
  # Gather everything up
  dfa <- OutputCollect$allPareto$resultHypParam %>%
    filter(.data$solID %in% OutputCollect$clusters$data$solID) %>%
    select(.data$solID, .data$rsq_train, .data$nrmse, .data$decomp.rssd, .data$mape) %>%
    left_join(performance, "solID") %>%
    left_join(temp, "solID") %>% ungroup() %>%
    mutate(
      score = normalize(
        normalize(.data$rsq_train) * ifelse(
          !"rsq_train" %in% metrics, 0, wt[which(metrics == "rsq_train")]) +
          normalize(.data$performance) * ifelse(
            !"performance" %in% metrics, 0, wt[which(metrics == "performance")]) +
          normalize(.data$potential_improvement) * ifelse(
            !"potential_improvement" %in% metrics, 0, wt[which(metrics == "potential_improvement")]) +
          normalize(.data$non_zeroes) * ifelse(
            !"non_zeroes" %in% metrics, 0, wt[which(metrics == "non_zeroes")]) +
          normalize(.data$incluster_models) * ifelse(
            !"incluster_models" %in% metrics, 0, wt[which(metrics == "incluster_models")]) +
          normalize(.data$nrmse) * ifelse(
            !"nrmse" %in% metrics, 0, wt[which(metrics == "nrmse")]) +
          normalize(.data$decomp.rssd) * ifelse(
            !"decomp.rssd" %in% metrics, 0, wt[which(metrics == "decomp.rssd")]) +
          normalize(.data$mape) * ifelse(
            !"mape" %in% metrics, 0, wt[which(metrics == "mape")])
      ),
      aux = rank(-.data$score)) %>%
    rowwise() %>%
    mutate(note = ifelse(.data$aux %in% 1:top, paste(rep("*", (top + 1) - .data$aux), collapse = ""), "")) %>%
    select(-.data$top_sol, -.data$aux) %>%
    arrange(desc(.data$score), desc(3), desc(4))
  if (!quiet) message("Recommended considering these models first: ", v2t(head(dfa$solID, top)))
  
  caption <- metrics_used %>%
    mutate(value = .data$wt/sum(.data$wt)) %>%
    arrange(desc(.data$value)) %>%
    mutate(lab = sprintf("%s %s%%", .data$metric_name, round(100 * .data$value))) %>%
    pull(.data$lab) %>%
    paste(., collapse = " | ")
  
  # Generate plot/dashboard
  p <- dfa %>%
    mutate(cluster = sprintf("%s (%s)", .data$cluster, .data$incluster_models),
           incluster_models = .data$incluster_models / max(dfa$incluster_models, na.rm = TRUE)) %>%
    tidyr::pivot_longer(all_of(metrics)) %>%
    filter(.data$name %in% metrics) %>%
    mutate(name_metrics = rep(metrics_used$metric_name, length.out = nrow(.)),
           name = factor(.data$name, levels = metrics),
           name_metrics = factor(.data$name_metrics, levels = metrics_used$metric_name)) %>%
    group_by(.data$name) %>%
    mutate(top = rank(-.data$value)) %>%
    left_join(select(dfa, .data$solID, .data$rsq_train), "solID") %>%
    group_by(.data$cluster) %>% 
    slice(1:((length(metrics))* n_per_cluster)) %>%
    mutate(solID = paste(.data$note, .data$solID)) %>%
    ggplot(aes(y = reorder(.data$solID, .data$score), x = .data$value)) +
    geom_col(aes(group = .data$name, fill = .data$top)) +
    # geom_vline(xintercept = 1, alpha = 0.5) +
    facet_grid(.data$cluster ~ .data$name_metrics, scales = "free") +
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
    data = select(dfa, "solID", "score", all_of(metrics), everything(), -.data$note),
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
