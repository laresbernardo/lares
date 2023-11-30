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
#' hyps_builder(
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
hyps_builder <- function(
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
  hyps <- c("alpha", "gamma")
  hyps2 <- if (adstock %in% "geomtric") "theta" else c("shape", "scale")
  all_hyps <- c(hyps, hyps2)
  
  # Repeat to all channels when provided 1 value
  if (length(media_type) == 1)
    media_type <- rep(media_type, length(paid_media_spends))
  if (length(lagged) == 1)
    lagged <- rep(lagged, length(paid_media_spends))
  if (any(lagged) && adstock == "geomtric")
    stop("To be able to have a lagged effect you need to set Weibull adstock")
  
  # Generate all combinations and data.frame
  df <- expand.grid(paid_media_spends, all_hyps)
  df$media_type <- rep(media_type, length(all_hyps))
  df$lagged <- rep(lagged, length(all_hyps))
  
  # Apply default rules
  df <- df %>%
    mutate(low = case_when(
      .data$Var2 == "alpha" ~ 0.01,
      .data$Var2 == "gamma" ~ 0.3,
      .data$Var2 == "theta" ~ 0.1,
      .data$Var2 == "shape" & isTRUE(.data$lagged) ~ 2,
      .data$Var2 == "shape" ~ 0,
      .data$Var2 == "scale" ~ 0
    )) %>%
    mutate(high = case_when(
      .data$Var2 == "alpha" & .data$media_type == "online" ~ 3,
      .data$Var2 == "alpha" & .data$media_type == "offline" ~ 1,
      .data$Var2 == "alpha" & .data$media_type == "default" ~ 3,
      .data$Var2 == "gamma" ~ 1,
      .data$Var2 == "theta" ~ 0.5,
      .data$Var2 == "shape" & isTRUE(.data$lagged) ~ 10,
      .data$Var2 == "shape" & isFALSE(.data$lagged) ~ 1,
      .data$Var2 == "shape" ~ 10,
      .data$Var2 == "scale" & isTRUE(.data$lagged) ~ 0.05,
      .data$Var2 == "scale" & isFALSE(.data$lagged) ~ 0.2,
      .data$Var2 == "scale" ~ 0.2
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
