####################################################################
#' Cross-MMM Budget Optimization across Channels (fast)
#'
#' Given a list of recreated Robyn models, this function optimizes budget
#' allocation across MMM with respective constraints by maximizing
#' response across all channels. This method assumes each model is
#' independent, that can be compared given its spends were cleanly and
#' properly split, they modeled the same metric (revenue or conversion)
#' and units (currency or type of conversion), and date granularity.
#' Recommended to have same channels granularity across markets
#' to simplify results readings and application.
#'
#' This approach is faster and cleaner compared with previous proposal
#' using \code{robyn_xmodels()}.
#'
#' @param models Lists. Recreated Robyn models with \code{robyn_recreate()}.
#' @param initial_budgets Numeric vector. Default will use the total spends
#' per model for the specified or total date range.
#' Must be length 1 or same as \code{models}.
#' @param start_dates,end_dates Character vector. Start and end dates for each
#' specific model. You can specify a single date and will be used in all models.
#' Default empty value will assume you want all available data and date range.
#' Must be length 1 or same as \code{models}.
#' @param channel_constr_low,channel_constr_up Numeric vector.
#' Relative lower and upper constraints per channel compared with mean
#' spend during the time period defined.
#' If mean was zero for date range, historical mean spend value will be used.
#' Must have length 1 to replicate for all channels or same length
#' (and order )as \code{paid_media_spends}.
#' @return List. Contains optimized allocation results and plots.
#' @examples
#' \dontrun{
#' # Calculate cross-brand optimal allocation
#' res <- robyn_xchannels(
#'   models,
#'   start_dates = "2023-01-01",
#'   end_dates = "2023-12-01"
#' )
#' }
#' @rdname robyn_crossmmm
#' @export
robyn_xchannels <- function(
    models,
    initial_budgets = NULL,
    start_dates = NULL,
    end_dates = NULL,
    channel_constr_low = 0.5,
    channel_constr_up = 2,
    quiet = FALSE,
    ...) {
  try_require("Robyn")
  try_require("nloptr")

  # Check all models are same type: metric and interval type
  stopifnot(length(unique(unlist(lapply(models, function(x) x$InputCollect$dep_var_type)))) == 1)
  stopifnot(length(unique(unlist(lapply(models, function(x) x$InputCollect$intervalType)))) == 1)

  # Identify channels per model
  for (i in seq_along(models)) {
    models[[i]]$OutputCollect$selectIDname <- names(models)[i]
  }
  all_channels <- unlist(
    lapply(models, function(x) {
      paste(x$OutputCollect$selectIDname, x$InputCollect$paid_media_spends, sep = "..")
    }),
    use.names = FALSE
  )

  # Setup same constraints if all not provided
  if (length(channel_constr_low) == 1) channel_constr_low <- rep(channel_constr_low, length(all_channels))
  if (length(channel_constr_up) == 1) channel_constr_up <- rep(channel_constr_up, length(all_channels))
  if (length(channel_constr_low) != length(all_channels) || length(channel_constr_up) != length(all_channels)) {
    stop(paste(
      "Inputs 'channel_constr_low' and 'channel_constr_up' must be",
      "the same length as the total number of channels across all models"
    ))
  }
  channel_constraints <- dplyr::tibble(
    channel = all_channels,
    channel_constr_low = channel_constr_low,
    channel_constr_up = channel_constr_up
  )

  # Initial state calculations
  history <- lapply(models, function(x) {
    robyn_performance(
      x$InputCollect, x$OutputCollect,
      start_date = NULL, end_date = NULL
    )
  })
  history <- lapply(history, function(x) {
    mutate(x, channel = paste(names(models)[i], .data$channel, sep = ".."))
  })

  # Extract and check start and end dates
  start_dates <- extract_dates(start_dates, history, "start_date", quiet)
  end_dates <- extract_dates(end_dates, history, "end_date", quiet)

  # Extract initial budgets, periods, and carryover for the date range
  init_budget <- init_carryover <- init_periods <- initial <- NULL
  for (i in seq_along(models)) {
    p <- robyn_performance(
      models[[i]]$InputCollect, models[[i]]$OutputCollect,
      start_date = start_dates[i], end_date = end_dates[i],
      carryovers = TRUE
    )
    paid_ch <- filter(p, .data$channel %in% models[[i]]$InputCollect$paid_media_spends)
    init_budget <- c(init_budget, sum(paid_ch$spend))
    paid_ch$periods <- ps <- models[[i]]$InputCollect$dt_mod %>%
      filter(.data$ds <= end_dates[i], .data$ds >= start_dates[i]) %>%
      nrow()
    init_periods <- c(init_periods, ps)
    initial <- bind_rows(initial, mutate(paid_ch, channel = paste(
      names(models)[i], .data$channel,
      sep = ".."
    )))
  }
  initial <- arrange(initial, factor(.data$channel, levels = all_channels))

  if (is.null(initial_budgets)) {
    names(init_budget) <- names(models)
    initial_budgets <- init_budget
  } else {
    if (length(initial_budgets) == 1) {
      initial_budgets <- rep(initial_budgets, length(models))
    }
  }

  # Spend values based on date range set
  x0 <- initSpendUnit <- initial$spend / initial$periods

  # Calculate historical carryover
  hist_carrs <- lapply(models, function(x) hist_carryover_calc(x))
  hist_carryover <- unlist(lapply(hist_carrs, function(x) lapply(x, mean)))

  # Total budget across brands
  total_budget_unit <- sum(x0)
  total_budget <- sum(initial_budgets)
  stopifnot(length(models) == length(initial_budgets))

  # Channels with no spend
  zero_spend_channel <- names(x0)[which(x0 == 0)]
  # If no spend within window as initial spend, use historical
  if (length(zero_spend_channel) > 0) {
    if (!quiet) {
      message("Media variables with 0 spending during date range: ", v2t(zero_spend_channel))
    }
    histSpendAllUnit <- bind_rows(lapply(history, function(x) {
      filter(x, .data$channel %in% all_channels)
    })) %>% pull(.data$spend)
    x0[zero_spend_channel] <- histSpendAllUnit[zero_spend_channel]
    # x0[zero_spend_channel] <- 1
  }

  ## Set local data & params values
  hypsregex <- "alphas|gammas|thetas|scales|shapes"
  dt_bestCoef <- bind_rows(lapply(models, function(x) {
    filter(
      x$OutputCollect$xDecompAgg,
      .data$solID == x$OutputCollect$selectID,
      .data$rn %in% x$InputCollect$paid_media_spends
    )
  })) %>%
    mutate(rn = paste(.data$solID, .data$rn, sep = ".."))
  dt_hyppar <- bind_cols(lapply(models, function(x) {
    filter(x$OutputCollect$resultHypParam, .data$solID == x$OutputCollect$selectID) %>%
      select(matches(hypsregex)) %>%
      select(matches(paste(x$InputCollect$paid_media_spends, collapse = "|"))) %>%
      dplyr::rename_with(~ paste(x$OutputCollect$selectIDname, ., sep = ".."))
  }))

  ## Get hill parameters for each channel
  hills <- list(
    alphas = unlist(lapply(models, function(x) {
      x$OutputCollect$resultHypParam %>%
        select(matches(paste(x$InputCollect$paid_media_spends, collapse = "|"))) %>%
        select(any_of(paste0(x$InputCollect$paid_media_spends, "_alphas")))
    })),
    inflexions = unlist(lapply(models, inflextion_points)),
    coefs_sorted = dt_bestCoef$coef
  )
  names(hills$coefs_sorted) <- all_channels

  ## Exclude 0 coef and 0 constraint channels for the optimization
  skip_these <- (channel_constr_low == 0 & channel_constr_up == 0)
  zero_constraint_channel <- all_channels[skip_these]
  if (length(zero_constraint_channel) > 0 && !quiet) {
    message(
      "Excluded variables (constrained to 0): ",
      paste(zero_constraint_channel, collapse = ", ")
    )
  }
  if (any(hills$coefs_sorted == 0)) {
    zero_coef_channel <- all_channels[hills$coefs_sorted == 0]
    if (!quiet) {
      message(
        "Excluded variables (coefficients are 0): ",
        paste(zero_coef_channel, collapse = ", ")
      )
    }
  } else {
    zero_coef_channel <- NULL
  }

  # Gather all values that will be used internally on optim (nloptr)
  no_impact <- c(zero_coef_channel, zero_constraint_channel)
  channel_for_allocation_ids <- which(!all_channels %in% no_impact)

  x0 <- x0[channel_for_allocation_ids]
  channel_constr_low <- channel_constr_low[channel_for_allocation_ids]
  channel_constr_up <- channel_constr_up[channel_for_allocation_ids]

  coefs_eval <- hills$coefs_sorted[channel_for_allocation_ids]
  alphas_eval <- hills$alphas[channel_for_allocation_ids]
  inflexions_eval <- hills$inflexions[channel_for_allocation_ids]
  hist_carryover_eval <- hist_carryover[channel_for_allocation_ids]

  # So we can implicitly use these values within eval_f()
  eval_list <- list(
    coefs_eval = coefs_eval,
    alphas_eval = alphas_eval,
    inflexions_eval = inflexions_eval,
    total_budget = NULL,
    total_budget_unit = total_budget_unit,
    hist_carryover_eval = hist_carryover_eval,
    target_value = NULL,
    target_value_ext = NULL,
    dep_var_type = models[[1]]$InputCollect$dep_var_type
  )
  options("ROBYN_TEMP" = eval_list)

  ## Bounded optimization
  nlsMod <- nloptr(
    x0 = x0 * channel_constr_low,
    eval_f = eval_f,
    eval_g_eq = eval_g_eq,
    eval_g_ineq = NULL,
    lb = x0 * channel_constr_low,
    ub = x0 * channel_constr_up,
    opts = list(
      "algorithm" = "NLOPT_LD_AUGLAG",
      "xtol_rel" = 1.0e-10,
      "maxeval" = 100000,
      "local_opts" = list(
        "algorithm" = "NLOPT_LD_SLSQP",
        "xtol_rel" = 1.0e-10
      )
    ),
    target_value = NULL
  )

  ## Get marginal response
  optmSpendUnit <- nlsMod$solution
  optmResponseUnit <- -eval_f(optmSpendUnit)[["objective.channel"]]
  optmResponseMargUnit <- mapply(
    fx_objective,
    x = optmSpendUnit + 1,
    coeff = coefs_eval,
    alpha = alphas_eval,
    inflexion = inflexions_eval,
    x_hist_carryover = hist_carryover_eval,
    get_sum = FALSE,
    SIMPLIFY = TRUE
  ) - optmResponseUnit

  # Clean auxiliary method
  .Options$ROBYN_TEMP <- NULL

  ## Collect channels outputs
  res <- channel_constraints[channel_for_allocation_ids, ] %>%
    mutate(
      model = gsub("\\.\\..*", "", .data$channel),
      channel = gsub(".*\\.\\.", "", .data$channel),
      start_date = initial$start_date[channel_for_allocation_ids],
      end_date = initial$end_date[channel_for_allocation_ids],
      periods = initial$periods[channel_for_allocation_ids],
      # Spend calculations
      initSpend = initial$spend[channel_for_allocation_ids],
      initSpendUnit = initial$spend[channel_for_allocation_ids] / .data$periods,
      initSpendDistr = .data$initSpend / sum(.data$initSpend),
      optmSpendUnit = optmSpendUnit,
      optmSpendDistr = optmSpendUnit / sum(optmSpendUnit),
      optmSpendChg = optmSpendUnit / initSpendUnit,
      # Response calcs
      initResponse = initial$response[channel_for_allocation_ids],
      initResponseUnit = initial$response[channel_for_allocation_ids] / .data$periods,
      optmResponseUnit = optmResponseUnit,
      optmResponseMargUnit = optmResponseMargUnit,
      # Performance metrics
      initROAS = .data$initResponse / .data$initSpend,
      optmROAS = .data$optmResponseUnit / .data$optmSpendUnit,
      initCPA = .data$initSpend / .data$initResponse,
      optmCPA = .data$optmSpendUnit / .data$optmResponseUnit
    ) %>%
    select("model", everything(), -contains("constr"))

  # Build models summary
  mod <- res %>%
    group_by(.data$model, .data$periods, .data$start_date, .data$end_date) %>%
    summarize(
      initSpend = sum(.data$initSpend),
      initSpendUnit = sum(.data$initSpendUnit),
      initSpendDistr = 0,
      optmSpendUnit = sum(.data$optmSpendUnit),
      optmSpendDistr = 0,
      optmSpendChg = 0,
      initResponseUnit = sum(.data$initResponseUnit),
      optmResponseUnit = sum(.data$optmResponseUnit),
      optmRespChange = 0,
      incrementalUnit = 0
    ) %>%
    ungroup() %>%
    mutate(
      initSpendDistr = .data$initSpend / sum(.data$initSpend),
      optmSpendDistr = .data$optmSpendUnit / sum(.data$initSpendUnit),
      optmSpendChg = .data$optmSpendUnit / .data$initSpendUnit,
      optmRespChange = .data$optmResponseUnit / .data$initResponseUnit,
      incrementalUnit = .data$optmResponseUnit - .data$initResponseUnit,
      incrementalTotal = .data$incrementalUnit * .data$periods
    )

  # Build total summary
  tot <- dplyr::tibble(
    models = length(models),
    incrUnit = sum(mod$incrementalUnit),
    incrTotal = sum(mod$incrementalTotal),
    initSpendUnit = sum(mod$initSpendUnit),
    optmSpendUnit = sum(optmSpendUnit),
    optmSpendChg = sum(optmSpendUnit) / sum(mod$initSpendUnit),
    initROAS = sum(res$initResponse) / sum(res$initSpend),
    optmROAS = sum(res$optmResponseUnit) / sum(res$optmSpendUnit)
  )

  output <- list(total = tot, models = mod, channels = res)
  class(output) <- c("robyn_crossmmm2", class(output))
  return(output)
}

eval_f <- function(X, target_value) {
  eval_list <- getOption("ROBYN_TEMP")
  coefs_eval <- eval_list[["coefs_eval"]]
  alphas_eval <- eval_list[["alphas_eval"]]
  inflexions_eval <- eval_list[["inflexions_eval"]]
  hist_carryover_eval <- eval_list[["hist_carryover_eval"]]

  objective <- -sum(mapply(
    fx_objective,
    x = X,
    coeff = coefs_eval,
    alpha = alphas_eval,
    inflexion = inflexions_eval,
    x_hist_carryover = hist_carryover_eval,
    SIMPLIFY = TRUE
  ))

  gradient <- c(mapply(
    fx_gradient,
    x = X,
    coeff = coefs_eval,
    alpha = alphas_eval,
    inflexion = inflexions_eval,
    x_hist_carryover = hist_carryover_eval,
    SIMPLIFY = TRUE
  ))

  objective.channel <- mapply(
    fx_objective.chanel,
    x = X,
    coeff = coefs_eval,
    alpha = alphas_eval,
    inflexion = inflexions_eval,
    x_hist_carryover = hist_carryover_eval,
    SIMPLIFY = TRUE
  )
  list(objective = objective, gradient = gradient, objective.channel = objective.channel)
}

fx_objective <- function(x, coeff, alpha, inflexion, x_hist_carryover, get_sum = TRUE) {
  xAdstocked <- x + mean(x_hist_carryover)
  if (get_sum) {
    xOut <- coeff * sum((1 + inflexion**alpha / xAdstocked**alpha)**-1)
  } else {
    xOut <- coeff * ((1 + inflexion**alpha / xAdstocked**alpha)**-1)
  }
  xOut
}

fx_gradient <- function(x, coeff, alpha, inflexion, x_hist_carryover) {
  xAdstocked <- x + mean(x_hist_carryover)
  -coeff * sum((alpha * (inflexion**alpha) * (xAdstocked**(alpha - 1))) / (xAdstocked**alpha + inflexion**alpha)**2)
}

fx_objective.chanel <- function(x, coeff, alpha, inflexion, x_hist_carryover) {
  xAdstocked <- x + mean(x_hist_carryover)
  -coeff * sum((1 + inflexion**alpha / xAdstocked**alpha)**-1)
}

eval_g_eq <- function(X, target_value) {
  eval_list <- getOption("ROBYN_TEMP")
  constr <- sum(X) - eval_list$total_budget_unit
  grad <- rep(1, length(X))
  list(
    "constraints" = constr,
    "jacobian" = grad
  )
}

extract_dates <- function(dates, history, date_col, quiet) {
  if (is.null(dates)) {
    dates <- as.Date(
      unlist(lapply(history, function(x) {
        unlist(pull(
          filter(x, .data$channel == "PROMOTIONAL TOTAL"),
          .data[[date_col]]
        ))
      })),
      origin = "1970-01-01"
    )
    if (!quiet) {
      message("Extracted ", date_col, ": ", v2t(sprintf("%s (%s)", dates, names(dates))))
    }
  } else {
    if (length(dates) == 1) dates <- rep(dates, length(history))
    if (length(dates) != length(history)) {
      stop(sprintf(
        "Inputs length for '%s' must be either 1 or same as models (%s)",
        date_col, length(history)
      ))
    }
  }
  dates
}

inflextion_points <- function(model) {
  chnAdstocked <- filter(
    model$OutputCollect$mediaVecCollect,
    .data$type == "adstockedMedia",
    .data$solID == model$OutputCollect$selectID
  ) %>%
    select(all_of(model$InputCollect$paid_media_spends)) %>%
    slice(model$InputCollect$rollingWindowStartWhich:model$InputCollect$rollingWindowEndWhich)
  gammas <- model$OutputCollect$resultHypParam %>%
    select(all_of(paste0(model$InputCollect$paid_media_spends, "_gammas"))) %>%
    unlist()
  inflexions <- unlist(lapply(seq_along(chnAdstocked), function(i) {
    c(range(chnAdstocked[, i]) %*% c(1 - gammas[i], gammas[i]))
  }))
  names(inflexions) <- names(gammas)
  inflexions
}

hist_carryover_calc <- function(model) {
  window_loc <- model$InputCollect$rollingWindowStartWhich:
  model$InputCollect$rollingWindowEndWhich
  ics <- lapply(model$InputCollect$paid_media_vars, function(x) {
    robyn_response(
      metric_name = x,
      dt_hyppar = model$OutputCollect$resultHypParam,
      dt_coef = model$OutputCollect$xDecompAgg,
      InputCollect = model$InputCollect,
      OutputCollect = model$OutputCollect,
      quiet = TRUE,
      is_allocator = TRUE
    )$input_carryover[window_loc]
  })
  names(ics) <- model$InputCollect$paid_media_vars
  dplyr::bind_cols(ics)[, model$InputCollect$paid_media_vars]
}
