####################################################################
#' Cross-MMM Budget Optimization across Models
#'
#' Given a list of recreated Robyn models, this function optimizes budget
#' allocation across MMM with respective constraints by maximizing
#' incremental revenue/conversions. This method assumes each model is
#' independent, that can be compared given its spends were cleanly and
#' properly split, they modeled the same metric (revenue or conversion)
#' and units (currency or type of conversion), and date granularity.
#' For best results, ensure channels have similar granularity across
#' markets to simplify interpretation and application of the outputs.
#'
#' @param budget_constr_low,budget_constr_up Numeric vector. Relative minimum
#' and maximum budgets to consider based on \code{initial_budgets}.
#' By default it'll consider 50% and 150% budget constraints.
#' Must be length 1 or same as \code{models}.
#' @param cores Integer. How many cores to use for parallel computations?
#' Set to 1 to not use this option.
#' Default will the minimum between 10 cores and all available cores - 1.
#' @param quiet Boolean. Keep quiet? If not,
#' informative messages will be printed.
#' @param ... Additional parameters to be passed to internal functions.
#' @return Invisible vector with results by letter.
#' @examples
#' # You must have Robyn installed and some models stored as JSON files
#' \dontrun{
#' # Import and recreate the models
#' files <- c("BrandA.json", "BrandB.json", "BrandC.json", "BrandN.json")
#' models <- lapply(files, function(x) Robyn::robyn_recreate(x))
#' names(models) <- gsub("\\.json", "", files)
#'
#' # Calculate cross-brand optimal allocation
#' res <- robyn_xmodels(
#'   models,
#'   cores = 10,
#'   start_dates = "2023-01-01",
#'   end_dates = "2023-12-01"
#' )
#' print(res)
#' res$summary
#' }
#' @export
#' @rdname robyn_crossmmm
robyn_xmodels <- function(
    models, initial_budgets = NULL,
    start_dates = NULL,
    end_dates = NULL,
    budget_constr_low = 0.5,
    budget_constr_up = 1.5,
    channel_constr_low = budget_constr_low,
    channel_constr_up = budget_constr_up,
    cores = NULL,
    quiet = FALSE,
    ...) {
  # Initial state calculations
  perfs <- lapply(models, function(x) {
    robyn_performance(
      x$InputCollect, x$OutputCollect,
      start_date = NULL, end_date = NULL
    ) %>%
      filter(.data$channel == "PROMOTIONAL TOTAL")
  })

  # Extract start and end dates when not provided
  extract_dates <- function(dates, perfs, date_col, quiet) {
    if (is.null(dates)) {
      dates <- as.Date(
        unlist(lapply(perfs, function(x) unlist(pull(x, .data[[date_col]])))),
        origin = "1970-01-01"
      )
      if (!quiet) {
        message("Extracted ", date_col, ": ", v2t(sprintf("%s (%s)", dates, names(dates))))
      }
    } else {
      if (length(dates) == 1) dates <- rep(dates, length(models))
    }
    dates
  }
  start_dates <- extract_dates(start_dates, perfs, "start_date", quiet)
  end_dates <- extract_dates(end_dates, perfs, "end_date", quiet)

  # Extract initial budgets for the date range
  if (is.null(initial_budgets)) {
    for (i in seq_along(models)) {
      p <- robyn_performance(
        models[[i]]$InputCollect, models[[i]]$OutputCollect,
        start_date = start_dates[i], end_date = end_dates[i]
      ) %>%
        filter(.data$channel == "PROMOTIONAL TOTAL") %>%
        pull(.data$spend)
      initial_budgets <- c(initial_budgets, p)
    }
    names(initial_budgets) <- names(models)
    if (!quiet) {
      message("Extracted budgets: ", v2t(
        sprintf("%s (%s)", num_abbr(initial_budgets), names(initial_budgets))
      ))
    }
  } else {
    if (length(initial_budgets) == 1) initial_budgets <- rep(initial_budgets, length(models))
  }

  # Total budget across brands
  total_budget <- sum(initial_budgets)

  # Run dims checks
  stopifnot(length(models) == length(start_dates))
  stopifnot(length(models) == length(end_dates))
  stopifnot(length(models) == length(initial_budgets))

  # Function to calculate total incremental sales given a budget distribution
  calculate_isales <- function(models, budgets, isales = TRUE,
                               start_dates = NULL, end_dates = NULL,
                               budget_constr_low = 0.5,
                               budget_constr_up = 1.5,
                               channel_constr_low = budget_constr_low,
                               channel_constr_up = budget_constr_up,
                               quiet = FALSE, ...) {
    try_require("Robyn")
    stopifnot(length(models) == length(budgets))
    bas <- list()
    for (i in seq_along(budgets)) {
      bas[[names(models)[i]]] <- robyn_allocator(
        InputCollect = models[[i]]$InputCollect,
        OutputCollect = models[[i]]$OutputCollect,
        total_budget = budgets[i],
        date_range = c(start_dates[i], end_dates[i]),
        scenario = "max_response",
        channel_constr_low = channel_constr_low,
        channel_constr_up = channel_constr_up,
        plots = FALSE,
        export = FALSE,
        quiet = TRUE,
        ...
      )
    }
    if (isales) {
      return(unlist(lapply(bas, function(x) {
        x$dt_optimOut$optmResponseTotal[1] - x$dt_optimOut$initResponseTotal[1]
      })))
    } else {
      return(bas)
    }
  }

  # Function to be optimized (minimized)
  objective_function <- function(budgets, total_budget, models,
                                 start_dates = NULL, end_dates = NULL,
                                 budget_constr_low = 0.5,
                                 budget_constr_up = 1.5,
                                 channel_constr_low = budget_constr_low,
                                 channel_constr_up = budget_constr_up,
                                 cores = 10, quiet = FALSE,
                                 ...) {
    # Value to minimize: penalty (tends to zero) - incremental total sales (max)
    isales <- calculate_isales(
      models, budgets,
      start_dates = start_dates,
      end_dates = end_dates,
      budget_constr_low = budget_constr_low,
      budget_constr_up = budget_constr_up,
      channel_constr_low = channel_constr_low,
      channel_constr_up = channel_constr_up, ...
    )

    # Calculate the penalty for not using the total budget
    budget_penalty <- abs(sum(budgets) - total_budget)

    # return(isales - budget_penalty)
    return(sum(isales) - budget_penalty)
  }

  # Optimize budgets across brands
  if (TRUE) {
    tic("robyn_crossbrand")
    if (cores > 1) {
      try_require("parallel")
      try_require("optimParallel")
      if (is.null(cores)) cores <- min(10, detectCores() - 1)
      cl <- makeCluster(cores)
      setDefaultCluster(cl)
      optimfx <- optimParallel
    } else {
      optimfx <- stats::optim
      cl <- NULL
    }
    if (!quiet) {
      message(sprintf(
        ">>> Started cross-brand optimization: %s brands using %s cores...",
        length(models), cores
      ))
    }

    # Interpolate 75% value between limits as initial budget seed
    mult <- budget_constr_low + 0.75 * (budget_constr_up - budget_constr_low)

    result <- tryCatch(
      {
        optimfx(
          parallel = list(cl = cl, loginfo = TRUE),
          fn = objective_function,
          par = initial_budgets * mult,
          lower = initial_budgets * budget_constr_low,
          upper = initial_budgets * budget_constr_up,
          total_budget = total_budget,
          models = models,
          start_dates = start_dates,
          end_dates = end_dates,
          budget_constr_low = budget_constr_low,
          budget_constr_up = budget_constr_up,
          channel_constr_low = channel_constr_low,
          channel_constr_up = channel_constr_up,
          method = "L-BFGS-B",
          control = list(fnscale = -1),
          ...
        )
      },
      error = function(e) {
        if (cores > 1) suppressWarnings(stopCluster(cl))
        stop(e)
      }
    )
    if (cores > 1) suppressWarnings(stopCluster(cl))
    if (!quiet) message(result$message)
    result$elapsed <- toc("robyn_crossbrand")$time
  }

  # Calculate allocation results with optima
  allocations <- calculate_isales(
    models, result$par,
    isales = FALSE,
    start_dates = start_dates,
    end_dates = end_dates,
    budget_constr_low = budget_constr_low,
    budget_constr_up = budget_constr_up,
    channel_constr_low = channel_constr_low,
    channel_constr_up = channel_constr_up,
    ...
  )

  df <- dplyr::tibble(
    model = names(allocations),
    start_date = unlist(start_dates),
    end_date = unlist(end_dates),
    budget = initial_budgets,
    response = unlist(lapply(allocations, function(x) {
      x$dt_optimOut$initResponseTotal[1]
    })),
    mresp_init = unlist(lapply(allocations, function(x) {
      sum(x$dt_optimOut$initResponseMargUnit)
    })),
    optbudget = unlist(lapply(allocations, function(x) {
      x$dt_optimOut$optmSpendTotal[1]
    })),
    optresponse = unlist(lapply(allocations, function(x) {
      x$dt_optimOut$optmResponseTotal[1]
    })),
    mresp_optm = unlist(lapply(allocations, function(x) {
      sum(x$dt_optimOut$optmResponseMargUnit)
    }))
  ) %>%
    mutate(
      budgetpct = round(.data$optbudget / .data$budget, 2),
      # initroas = round(response / budget, 2),
      # optroas = round(optresponse / optbudget, 2),
      incremental = .data$optresponse - .data$response,
      iperc = round(100 * .data$incremental / .data$response, 2)
    )

  # Final outputs
  out <- list(
    inputs = list(
      initial_budgets = initial_budgets,
      total_budget = total_budget,
      budget_constr_low = budget_constr_low,
      channel_constr_up = channel_constr_up,
      channel_constr_low = channel_constr_low,
      channel_constr_up = channel_constr_up,
      cores = cores
    ),
    optimization = result,
    allocations = allocations,
    incremental = sum(df$incremental),
    summary = df,
    mresp_init = sum(unlist(lapply(allocations, function(x) {
      sum(x$dt_optimOut$initResponseMargUnit)
    }))),
    mresp_optm = sum(unlist(lapply(allocations, function(x) {
      sum(x$dt_optimOut$optmResponseMargUnit)
    }))),
    other_arguments = list(...)
  )
  class(out) <- c("robyn_crossmmm", class(out))
  return(out)
}

#' @rdname robyn_crossmmm
#' @param x Object to print: robyn_crossmmm() output.
#' @export
print.robyn_crossmmm <- function(x, ...) {
  cat(glued(
    "
Initial State:
  Models: {paste(names(x$allocations), collapse = ', ')}
  Date Ranges: {date_ranges}
  Budgets: {paste(num_abbr(x$inputs$initial_budgets), collapse = ', ')}
  Distribution: {pct_string(x$inputs$initial_budgets / x$inputs$total_budget)}
  Total Budget: {num_abbr(x$inputs$total_budget, 3)}
  Marginal Response: {signif(mresp_init, 4)}

Iterations:
  Time Elapsed: {x$optimization$elapsed}
  Total Iterations: {x$optimization$counts[[1]]}
  Convergence: {x$optimization$convergence}
  Cores: {x$inputs$cores}

Optimized State:
  Budget Increase: {num_abbr(bi)} ({formatNum(100 * bi / x$inputs$total_budget, abbr = TRUE, sign = TRUE, pos = '%')})
  Incremental: {num_abbr(x$incremental)}
  Optimal Budgets: {paste(num_abbr(x$optimization$par), collapse = ', ')}
  Optimal Distribution: {pct_string(x$optimization$par / x$inputs$total_budget)}
  Compared with Initial: {pct_string(x$optimization$par / x$inputs$initial_budgets)}
  Optimal Marginal Response: {signif(mresp_optm, 4)}
 ",
    pct_string = function(x) {
      paste(paste0(num_abbr(100 * x), "%"), collapse = ", ")
    },
    date_ranges = if (all(length(unique(x$summary$start_date))) == 1 && all(length(unique(x$summary$end_date))) == 1) {
      paste(x$summary$start_date[1], x$summary$end_date[1], sep = ":")
    } else {
      paste(paste(x$summary$start_date, x$summary$end_date, sep = ":"), collapse = ", ")
    },
    mresp_init = sum(unlist(lapply(x$allocations, function(x) {
      sum(x$dt_optimOut$initResponseMargUnit)
    }))),
    mresp_optm = sum(unlist(lapply(x$allocations, function(x) {
      sum(x$dt_optimOut$optmResponseMargUnit)
    }))),
    bi = round(sum(x$summary$optbudget) - x$inputs$total_budget)
  ))
}
