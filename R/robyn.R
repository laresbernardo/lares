####################################################################
#' Robyn: Generate default hyperparameters
#'
#' Generate a list with hyperparameter default values, ready to be
#' passed to \code{Robyn::robyn_inputs()}.
#'
#' @family Robyn
#' @param channels Character vector. Paid media and organic variables names.
#' @param media_type Character vector. Must be length 1 or same as
#' \code{channels}. Pick, for every \code{channels} value,
#' what type of media it is: "online" or "offline".
#' @param adstock Character. Pick one of: "geometric" or "weibull".
#' @param date_type Character. Pick one of: "daily", "weekly", or "monthly".
#' Only valid to transform thetas when using geometric adstock. Set to "skip"
#' in case you wish to leave default weekly values.
#' @param lagged Boolean vector. Must be length 1 or same as
#' \code{channels}. Pick, for every \code{channels} value,
#' if you wish to have a lagged effect. Only valid for Weibull adstock.
#' @return list with default hyperparameters ranges.
#' @examples
#' robyn_hypsbuilder(
#'   channels = c(
#'     "branded_search_spend",
#'     "nonbranded_search_spend",
#'     "print_spend",
#'     "ooh_spend",
#'     "tv_spend",
#'     "radio_spend"
#'   ),
#'   media_type = c(
#'     "online", "online", "offline",
#'     "offline", "offline", "offline"
#'   ),
#'   adstock = "geometric",
#'   date_type = "weekly"
#' )
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
  check_opts(date_type, c("daily", "weekly", "monthly", "skip"))
  check_opts(lagged, c(TRUE, FALSE))

  # Hyperparameters names
  hyps <- c("alphas", "gammas")
  hyps2 <- if (adstock %in% "geometric") "thetas" else c("shapes", "scales")
  all_hyps <- c(hyps, hyps2)

  # Repeat to all channels when provided 1 value
  if (length(media_type) == 1) {
    media_type <- rep(media_type, length(channels))
  }
  if (length(lagged) == 1) {
    lagged <- rep(lagged, length(channels))
  }
  if (any(lagged) && adstock %in% c("geometric", "weibull_cdf")) {
    stop("To be able to have a lagged effect you need to set 'weibull_pdf' adstock")
  }
  stopifnot(length(channels) == length(media_type))

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
      .data$Var2 == "alphas" & .data$media_type %in% c("online", "default") ~ 3,
      .data$Var2 == "alphas" & .data$media_type == "offline" ~ 1,
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
        .data$Var2 == "thetas" & date_type == "daily" ~ .data$low^(1 / 7),
        .data$Var2 == "thetas" & date_type == "monthly" ~ .data$low^(4),
        TRUE ~ .data$low
      ),
      high = case_when(
        .data$Var2 == "thetas" & date_type == "daily" ~ .data$high^(1 / 7),
        .data$Var2 == "thetas" & date_type == "monthly" ~ .data$high^(4),
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
#' Robyn: Model Selection by Weighted Criteria Scores
#'
#' @description
#' Consider N best models to select the right ones to study using
#' several criteria/metrics such as potential improvement on budget
#' allocator, how many non-zero coefficients there are, R squared,
#' historical performance, baseline expectation, etc.
#'
#' Read more about this functionality in Medium post:
#' \href{https://medium.com/@laresbernardo/select-the-right-mmm-candidate-based-on-your-specific-criteria-and-business-knowledge-1f583c3cb97a}{here}.
#'
#' @family Robyn
#' @inheritParams cache_write
#' @param InputCollect,OutputCollect Robyn output objects.
#' @param metrics Character vector. Which metrics do you want to consider?
#' Pick any combination from: "rsq_train" for trained R squared,
#' "performance" for ROAS or (inverse) CPA, "potential_improvement" for
#' default budget allocator improvement using \code{allocator_limits},
#' "non_zeroes" for non-zero beta coefficients, "incluster_models" for
#' amount of models per cluster, "baseline_dist" for the difference between
#' the model's baseline and \code{baseline_ref} value. You can also use the
#' standard MOO errors: "nrmse", "decomp.rssd", and "mape" (the lowest the
#' error, the highest the score; same for "baseline_dist").
#' @param wt Vector. Weight for each of the normalized \code{metrics} selected,
#' to calculate the score and rank models. Must have the same order and length
#' of \code{metrics} parameter input.
#' @param baseline_ref Numeric value. Between 0 and 1. What is the baseline
#' percentage you expect? Baseline in this case are all the sales or conversions
#' from non-media channels (organic & paid). Use with "baseline_dist" metric.
#' @param top Integer. How many ranked models to star? The better the model
#' is, the more stars it will have marked.
#' @param allocator_limits Numeric vector, length 2. How flexible do you
#' want to be with the budget allocator? By default, we'll consider a
#' 0.5X and 2X range to let the budget shift across channels.
#' @param n_per_cluster Integer. How many models per cluster do you
#' want to plot? Default: 5. Keep in mind they will all be considered
#' for the calculations.
#' @param cache Use cache functionality for allocator's results?
#' @return list with resulting ranked data.frames, weights and plot.
#' @export
robyn_modelselector <- function(
    InputCollect,
    OutputCollect,
    metrics = c(
      "rsq_train", "performance",
      "potential_improvement",
      "non_zeroes", "incluster_models",
      "baseline_dist"
    ),
    wt = c(2, 1, 0, 1, 0.1, 0),
    baseline_ref = 0,
    top = 4,
    n_per_cluster = 5,
    allocator_limits = c(0.5, 2),
    quiet = FALSE,
    cache = TRUE,
    ...) {
  if (length(OutputCollect$allSolutions) <= 1) {
    msg <- "Not enough models for model selection"
    warning(msg)
    ret <- invisible(list(
      data = tibble(solID = OutputCollect$allSolutions, score = 1),
      plot = noPlot(msg)
    ))
    class(ret) <- c("robyn_modelselector", class(ret))
    return(ret)
  }

  stopifnot(length(wt) == length(metrics))
  stopifnot(length(allocator_limits) == 2)
  stopifnot(baseline_ref >= 0 && baseline_ref <= 1)

  # Available metrics
  metrics_df <- data.frame(
    metric = c(
      "rsq_train", "performance", "potential_improvement",
      "non_zeroes", "incluster_models", "baseline_dist",
      "nrmse", "decomp.rssd", "mape"
    ),
    metric_name = c(
      "R^2", ifelse(InputCollect$dep_var_type == "revenue", "High ROAS", "Low CPA"),
      "Potential Boost", "Non-Zero Betas", "Models in Cluster",
      sprintf("Baseline Distance [%.0f%%]", signif(baseline_ref * 100, 2)),
      "1 - NRMSE", "1 - DECOMP.RSSD", "1 - MAPE"
    )
  )
  check_opts(metrics, metrics_df$metric)

  # Metrics Used
  metrics_used <- filter(metrics_df, .data$metric %in% metrics) %>%
    arrange(match(.data$metric, metrics)) %>%
    left_join(data.frame(metric = metrics, wt = wt), "metric") %>%
    filter(.data$wt > 0)

  # Add Default Potential Improvement values
  sols <- sort(OutputCollect$allSolutions)
  if ("potential_improvement" %in% metrics & isTRUE(wt[which(metrics == "potential_improvement")] != 0)) {
    cache_name <- c("robyn_modelselector", length(sols))
    if (cache & cache_exists(cache_name, ...)) {
      potOpt <- cache_read(cache_name, quiet = quiet, ...)
    } else {
      if (!quiet) {
        message(sprintf(
          ">>> Calculating potential performance improvements on %s Pareto-front models...",
          length(sols)
        ))
      }
      try_require("Robyn")
      defpot <- lapply(sols, function(x) {
        if (!quiet) statusbar(which(sols == x), length(sols), x)
        suppressMessages(robyn_allocator(
          InputCollect = InputCollect,
          OutputCollect = OutputCollect,
          select_model = x,
          channel_constr_low = min(allocator_limits),
          channel_constr_up = max(allocator_limits),
          plots = FALSE,
          export = FALSE,
          quiet = TRUE,
          ...
        ))
      })
      potOpt <- data.frame(
        solID = sols,
        potential_improvement = unlist(lapply(defpot, function(x) {
          x$dt_optimOut$optmResponseUnitTotalLift[1]
        }))
      )
      if (cache) cache_write(potOpt, cache_name, quiet = quiet, ...)
    }
  } else {
    potOpt <- data.frame(solID = sols, potential_improvement = 0)
  }

  # Check pareto-front models summaries to calculate performance (ROAS/CPA)
  performance <- OutputCollect$xDecompAgg %>%
    filter(!is.na(.data$mean_spend)) %>% # get rid of organic
    filter(.data$solID %in% OutputCollect$clusters$data$solID) %>%
    group_by(.data$solID) %>%
    summarise(
      performance = sum(.data$xDecompAgg) / sum(.data$total_spend, na.rm = TRUE)
    )

  # Check pareto-front models to calculate non-zero betas in media channels
  non_zeroes_rate <- OutputCollect$allPareto$xDecompAgg %>%
    filter(.data$solID %in% OutputCollect$clusters$data$solID) %>%
    group_by(.data$solID) %>%
    summarise(
      non_zeroes = length(.data$rn[
        round(.data$coef, 6) > 0 & .data$rn %in% InputCollect$all_media
      ]) /
        length(InputCollect$all_media)
    )

  # Count models per cluster
  if (!"clusters" %in% names(OutputCollect)) {
    OutputCollect$clusters$data <- data.frame(solID = sols, cluster = "None", top_sol = TRUE)
    OutputCollect$clusters$clusters_means <- data.frame(cluster = "None", n = length(sols))
  }
  temp <- OutputCollect$clusters$data %>%
    select(.data$solID, .data$cluster, .data$top_sol) %>%
    mutate(cluster = as.character(.data$cluster)) %>%
    left_join(select(OutputCollect$clusters$clusters_means, .data$cluster, .data$n), "cluster") %>%
    rename(incluster_models = "n")

  # Calculate baselines
  baselines <- OutputCollect$xDecompAgg %>%
    mutate(rn = ifelse(.data$rn %in% c(InputCollect$all_media), .data$rn, "baseline")) %>%
    group_by(.data$solID, .data$rn) %>%
    summarize(contribution = sum(.data$xDecompAgg), .groups = "drop") %>%
    group_by(.data$solID) %>%
    mutate(baseline = .data$contribution / sum(.data$contribution)) %>%
    ungroup() %>%
    filter(.data$rn == "baseline") %>%
    arrange(abs(.data$baseline)) %>%
    mutate(baseline_dist = abs(baseline_ref - .data$baseline)) %>%
    select(c("solID", "baseline", "baseline_dist")) %>%
    arrange(desc(.data$baseline_dist))

  # Gather everything up
  dfa <- OutputCollect$allPareto$resultHypParam %>%
    filter(.data$solID %in% OutputCollect$clusters$data$solID) %>%
    select(.data$solID, .data$rsq_train, .data$nrmse, .data$decomp.rssd, .data$mape) %>%
    left_join(performance, "solID") %>%
    left_join(non_zeroes_rate, "solID") %>%
    left_join(potOpt, "solID") %>%
    left_join(temp, "solID") %>%
    ungroup() %>%
    left_join(baselines, "solID")

  # The following criteria are inverted because the smaller, the better
  inv <- c("baseline_dist", "nrmse", "decomp.rssd", "mape")

  # Calculate normalized and weighted scores
  scores <- list(
    rsq_train = normalize(dfa$rsq_train) * ifelse(
      !"rsq_train" %in% metrics, 0, wt[which(metrics == "rsq_train")]
    ) * ifelse("rsq_train" %in% inv, -1, 1),
    performance = normalize(dfa$performance, na.rm = TRUE) * ifelse(
      !"performance" %in% metrics, 0, wt[which(metrics == "performance")]
    ) * ifelse("performance" %in% inv, -1, 1),
    potential_improvement = normalize(dfa$potential_improvement) * ifelse(
      !"potential_improvement" %in% metrics, 0, wt[which(metrics == "potential_improvement")]
    ) * ifelse("potential_improvement" %in% inv, -1, 1),
    non_zeroes = normalize(dfa$non_zeroes) * ifelse(
      !"non_zeroes" %in% metrics, 0, wt[which(metrics == "non_zeroes")]
    ) * ifelse("non_zeroes" %in% inv, -1, 1),
    incluster_models = normalize(dfa$incluster_models) * ifelse(
      !"incluster_models" %in% metrics, 0, wt[which(metrics == "incluster_models")]
    ) * ifelse("incluster_models" %in% inv, -1, 1),
    # The following are negative/inverted criteria when scoring
    baseline_dist = normalize(dfa$baseline_dist) * ifelse(
      !"baseline_dist" %in% metrics, 0, wt[which(metrics == "baseline_dist")]
    ) * ifelse("baseline_dist" %in% inv, -1, 1),
    nrmse = normalize(dfa$nrmse) * ifelse(
      !"nrmse" %in% metrics, 0, wt[which(metrics == "nrmse")]
    ) * ifelse("nrmse" %in% inv, -1, 1),
    decomp.rssd = normalize(dfa$decomp.rssd) * ifelse(
      !"decomp.rssd" %in% metrics, 0, wt[which(metrics == "decomp.rssd")]
    ) * ifelse("decomp.rssd" %in% inv, -1, 1),
    mape = normalize(dfa$mape) * ifelse(
      !"mape" %in% metrics, 0, wt[which(metrics == "mape")]
    ) * ifelse("mape" %in% inv, -1, 1)
  )
  dfa <- dfa %>%
    mutate(
      score = normalize(rowSums(bind_cols(scores))),
      aux = rank(-.data$score, ties.method = "first")
    ) %>%
    rowwise() %>%
    mutate(note = ifelse(.data$aux %in% 1:top, paste(
      rep("*", (top + 1) - .data$aux),
      collapse = ""
    ), "")) %>%
    select(-.data$top_sol, -.data$aux) %>%
    arrange(desc(.data$score), desc(3), desc(4))
  if (!quiet) message("Recommended considering these models first: ", v2t(head(dfa$solID, top)))

  # Generate plot/dashboard
  caption <- metrics_used %>%
    mutate(value = .data$wt / sum(.data$wt)) %>%
    arrange(desc(.data$value)) %>%
    mutate(lab = sprintf("%s%% %s", round(100 * .data$value), .data$metric_name)) %>%
    pull(.data$lab) %>%
    paste(., collapse = " | ")
  sorting <- dfa %>%
    mutate(cluster = sprintf("%s (%s)", .data$cluster, .data$incluster_models)) %>%
    group_by(.data$cluster, .data$note) %>%
    tally() %>%
    arrange(desc(.data$note)) %>%
    pull(.data$cluster) %>%
    unique()
  pdat <- dfa %>%
    # So that inverted variables have larger relative bars (darker blue)
    mutate_at(all_of(inv), function(x) 1 - x) %>%
    mutate(
      cluster = factor(sprintf("%s (%s)", .data$cluster, .data$incluster_models), levels = sorting),
      incluster_models = .data$incluster_models / max(dfa$incluster_models, na.rm = TRUE)
    ) %>%
    tidyr::pivot_longer(any_of(metrics_used$metric)) %>%
    filter(.data$name %in% metrics_used$metric) %>%
    mutate(
      name_metrics = rep(metrics_used$metric_name, length.out = nrow(.)),
      name = factor(.data$name, levels = metrics_used$metric),
      name_metrics = factor(.data$name_metrics, levels = metrics_used$metric_name)
    ) %>%
    group_by(.data$name) %>%
    mutate(top = rank(-.data$value)) %>%
    left_join(select(dfa, .data$solID, .data$rsq_train), "solID") %>%
    group_by(.data$cluster) %>%
    slice(1:((length(metrics_used$metric)) * n_per_cluster)) %>%
    mutate(solID = paste(.data$note, .data$solID))
  p <- pdat %>%
    ggplot(aes(y = reorder(.data$solID, .data$score), x = .data$value)) +
    geom_col(aes(group = .data$name, fill = .data$top)) +
    # geom_vline(xintercept = 1, alpha = 0.5, linetype = "dotted") +
    facet_grid(.data$cluster ~ .data$name_metrics, scales = "free") +
    labs(
      y = NULL, x = "Criteria Scores (the highest the better)",
      title = "Select a model based on these metrics and rankings",
      subtitle = paste(
        "Showing up to", n_per_cluster,
        "models per cluster and ranked solutions marked with stars"
      ),
      caption = paste("Weights:", caption)
    ) +
    scale_x_percent(position = "top") +
    theme_lares(legend = FALSE)

  # Create the exported object
  ret <- invisible(list(
    data = select(dfa, "solID", "score", all_of(metrics_used$metric), everything(), -.data$note),
    metrics = metrics, weights = wt, baseline_ref = baseline_ref,
    weighted_scores = tibble(solID = dfa$solID, score = dfa$score, bind_cols(scores)),
    plot = p
  ))
  class(ret) <- c("robyn_modelselector", class(ret))
  return(ret)
}

#' @param x robyn_modelselector object
#' @rdname robyn_modelselector
#' @export
plot.robyn_modelselector <- function(x, ...) {
  x$plot
}

####################################################################
#' Robyn: Dynamic Performance and Contribution Report
#'
#' Given a date range, calculate specific and total performance and
#' contribution for each of your marketing and non-marketing channels.
#'
#' @family Robyn
#' @inheritParams cache_write
#' @inheritParams robyn_modelselector
#' @param start_date,end_date Date. Start and end date to filter
#' the data to be reported.
#' @param solID Character. Single ID of the model to report. If there's
#' only one available in OutputCollect, no need to define.
#' @param totals Boolean. Add total rows. This includes summary rows
#' (promotional which is paid and organic channels, baseline, grand total).
#' @param marginals Boolean. Include mROAS or mCPA marginal performance metric
#' as an additional column called "marginal". Calculations are based on
#' mean spend and mean response with mean carryover results,
#' between \code{start_date} and \code{end_date}.
#' @param carryovers Boolean. Add mean percentage of carryover response for
#' date range between \code{start_date} and \code{end_date} on paid channels.
#' Keep in mind organic variables also have carryover but currently not showing.
#' @return data.frame with results on ROAS/CPA, spend, response, contribution
#' per channel, with or without total rows.
#' @examples
#' \dontrun{
#' # You may load an exported model to recreate Robyn objects
#' mod <- Robyn::robyn_recreate(json_file = "your_model.json")
#' robyn_performance(mod$InputCollect, mod$OutputCollect)
#' }
#' @export
robyn_performance <- function(
    InputCollect, OutputCollect,
    start_date = NULL, end_date = NULL,
    solID = NULL, totals = TRUE,
    marginals = FALSE, carryovers = FALSE,
    quiet = FALSE, ...) {
  df <- OutputCollect$mediaVecCollect
  if (!is.null(solID)) {
    if (length(solID) > 1) {
      stop("Select a single model using 'solID' parameter input")
    }
  } else {
    if (length(unique(df$solID)) > 1) {
      stop("Provide single model results or define 'solID' parameter input")
    } else {
      solID <- unique(df$solID)
    }
  }
  if (is.null(start_date)) {
    start_date <- as.Date(InputCollect$window_start)
  }
  if (is.null(end_date)) {
    end_date <- as.Date(InputCollect$window_end)
  }
  stopifnot(start_date <= end_date)

  # Filter data for ID, modeling window and selected date range
  df <- df[df$solID %in% solID, ] %>%
    filter(
      .data$ds >= InputCollect$window_start,
      .data$ds <= InputCollect$window_end,
      .data$ds >= start_date, .data$ds <= end_date
    ) %>%
    select(c("ds", "solID", "type", InputCollect$all_media))
  if (nrow(df) == 0 && !quiet) {
    warning(sprintf(
      "No data for model %s within modeling window (%s:%s) and date range filtered (%s:%s)",
      solID, InputCollect$window_start, InputCollect$window_end,
      start_date, end_date
    ))
    return(NULL)
  }
  spends <- df %>%
    filter(.data$type == "rawSpend") %>%
    summarise_if(is.numeric, function(x) sum(x, na.rm = TRUE))
  response <- df %>%
    filter(.data$type == "decompMedia") %>%
    summarise_if(is.numeric, function(x) sum(x, na.rm = TRUE))
  metric <- ifelse(InputCollect$dep_var_type == "revenue", "ROAS", "CPA")
  if (metric == "ROAS") {
    performance <- response / spends
    total <- sum(response) / sum(spends)
  } else {
    performance <- spends / response
    total <- sum(spends) / sum(response)
  }
  ret <- dplyr::tibble(
    solID = solID,
    start_date = min(df$ds, na.rm = TRUE),
    end_date = max(df$ds, na.rm = TRUE),
    channel = InputCollect$all_media,
    metric = metric,
    performance = unlist(performance),
    spend = unlist(spends),
    response = unlist(response)
  ) %>%
    arrange(desc(.data$spend))
  # Create TOTAL row
  totals_df <- ret[1, 1:3] %>%
    mutate(
      channel = "PROMOTIONAL TOTAL",
      metric = metric,
      performance = total,
      spend = sum(spends),
      response = sum(response)
    )
  # Add marketing contribution to sales/conversions (dynamic depending on date)
  xdvc <- OutputCollect$xDecompVecCollect
  xDecompPerc <- xdvc[xdvc$solID %in% solID, ] %>%
    filter(.data$ds >= start_date, .data$ds <= end_date) %>%
    summarise_if(is.numeric, function(x) sum(x, na.rm = TRUE)) %>%
    # Divide by prediction, not real values
    mutate_all(function(x) x / .$depVarHat) %>%
    tidyr::gather(key = "channel", value = "contribution")
  mktg_contr <- filter(xDecompPerc, .data$channel %in% InputCollect$all_media)
  mksum <- sum(mktg_contr$contribution)
  mktg_contr2 <- rbind(
    data.frame(
      channel = c("PROMOTIONAL TOTAL", "BASELINE", "GRAND TOTAL"),
      contribution = c(mksum, 1 - mksum, 1)
    ),
    mktg_contr
  )
  # Create Baseline row
  resp_baseline <- (1 - mksum) * sum(ret$response) / mksum
  totals_base <- ret[1, 1:3] %>%
    mutate(
      channel = "BASELINE",
      metric = "",
      performance = NA,
      spend = 0,
      response = resp_baseline
    )
  # Create TOTAL row
  grand_total <- ret[1, 1:3] %>%
    mutate(
      channel = "GRAND TOTAL",
      metric = metric,
      performance = (resp_baseline + sum(ret$response)) / sum(ret$spend),
      spend = sum(ret$spend),
      response = resp_baseline + sum(ret$response)
    )
  # Join everything together
  if (totals) ret <- rbind(ret, totals_df, totals_base, grand_total)
  ret <- left_join(ret, mktg_contr2, "channel")

  # Add mROAS/mCPA
  if (marginals) {
    try_require("Robyn")
    ba_temp <- robyn_allocator(
      InputCollect = InputCollect,
      OutputCollect = OutputCollect,
      date_range = c(as.Date(start_date), as.Date(end_date)),
      export = FALSE, quiet = TRUE
    )
    marginal <- ba_temp$dt_optimOut %>%
      select(c("channels", "initResponseMargUnit")) %>%
      rename(
        "channel" = "channels",
        "marginal" = "initResponseMargUnit"
      )
    ret <- left_join(ret, marginal, "channel") %>%
      dplyr::relocate("marginal", .after = "performance")
  }
  # Add carryover response percentage
  if (carryovers) {
    try_require("Robyn")
    carrov <- robyn_immcarr(
      InputCollect, OutputCollect,
      solID = solID,
      start_date = start_date, end_date = end_date, ...
    ) %>%
      filter(.data$type == "Carryover")
    mean_carryovers <- data.frame(
      channel = carrov$rn,
      carryover = signif(carrov$carryover_pct, 4)
    )
    ret <- left_join(ret, mean_carryovers, "channel") %>%
      dplyr::relocate("carryover", .after = "response")
  }
  return(ret)
}
