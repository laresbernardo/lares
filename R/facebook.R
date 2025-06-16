# Auxiliary constant values
# META_GRAPH_URL <- "https://graph.intern.facebook.com"
META_GRAPH_URL <- "https://graph.facebook.com"
META_API_VER <- "v21.0"

####################################################################
#' Paginate and Process Facebook's API Results
#'
#' Process and paginate raw results from Facebook's API, result of
#' querying the API with \code{httr::GET} or by passing an API link.
#'
#' @family API
#' @family Meta
#' @inheritParams tic
#' @inheritParams cache_write
#' @param input GET's output object (response) or link (character).
#' @param paginate Boolean or integer. Run through all paginations? If set
#' to \code{FALSE}, only the first one will be processed. If set to any other
#' integer value, will process the first N paginations.
#' @param sleep Numeric value. How much should each loop wait until until running
#' the next pagination query?
#' @return data.frame with un-nested processed results or NULL if no results found.
#' @export
fb_process <- function(input, paginate = TRUE, sleep = 0, quiet = FALSE, ...) {
  if (is.character(input)) {
    if (is_url(input)) input <- httr::GET(input)
  }
  if (!"response" %in% class(input)) {
    stop("You must provide a response object (GET's output) or a valid URL")
  }
  tic(paste0("fb_process_", input$url))
  import <- content(input, encoding = "json")

  # Show and return error (and help is token expired)
  if ("error" %in% names(import)) {
    if (!quiet) message(paste("API ERROR:", import$error$message))
    import$url <- input$url
    if ("error" %in% names(import)) {
      if (grepl("expired", import$error$message) & !quiet) {
        message(paste(
          "You must be logged in to your Facebook account and refresh/get a new token:",
          "\n1. Within the Graph API Explorer, select an Application.",
          "\n2. Go to Get Token and select the Page Acces Token needed.",
          "Note that there are navigation arrows if there are lots of accounts.",
          "\n3. Copy and use the Acces Token created."
        ))
        browseURL("https://developers.facebook.com/tools/explorer")
      }
    }
    invisible(import)
  }

  # Check for no-data (user might think there was an error on GET request - warn him!)
  if (length(import[[1]]) == 0) {
    message("NO DATA: No results found for this request")
    invisible(import)
  } else {
    # Deal with GET + response vs jsonlite
    out <- fromJSON(toJSON(import))
    if (all(lapply(out, length) == 1)) {
      out
    } else {
      # First pagination
      results <- list()
      i <- 1
      results[[i]] <- list_flattener(out[[1]], first = TRUE)
      if (!quiet) show_pag_status(results, i, sleep, quiet)

      # Following iterations
      if (exists("paging", out) && as.integer(paginate) >= 1) {
        if (exists("next", out$paging)) {
          i <- i + 1
          out <- fromJSON(out$paging[["next"]])
          results[[i]] <- list_flattener(out$data, i)
          if (!quiet) show_pag_status(results, i, sleep, quiet)
          # Re-run first iteration and overwrite as everything MUST match to bind
          if (i == 2) {
            out <- fromJSON(out$paging[["previous"]])
            results[[1]] <- list_flattener(out$data, i)
            if (!quiet) show_pag_status(results, i, sleep, quiet)
          }
          while (exists("next", out$paging) && (i < as.integer(paginate) || isTRUE(paginate))) {
            i <- i + 1
            res <- try(
              {
                out <- fromJSON(out$paging[["next"]])
                results[[i]] <- list_flattener(out$data, i)
                if (!quiet) show_pag_status(results, i, sleep, quiet)
              },
              silent = TRUE
            )
            if (inherits(res, "try-error")) {
              warning(paste0("Returning partial results given last pagination (", i, ") returned error"))
              break
            }
          }
        }
      }
      done <- bind_rows(results)
      ret <- suppressMessages(type.convert(
        done,
        numerals = "no.loss", as.is = TRUE
      )) %>%
        mutate_at(vars(contains("date")), list(as.Date)) %>%
        mutate_at(vars(contains("id")), list(as.character)) %>%
        mutate_at(vars(contains("url")), list(as.character)) %>%
        mutate_at(vars(contains("name")), list(as.character)) %>%
        as_tibble()

      # Save last pagination and if it returned complete results
      attr(ret, "paging_done") <- !("next" %in% names(out[["paging"]]))
      attr(ret, "paging") <- out[["paging"]]
      toc(paste0("fb_process_", input$url), quiet = quiet, ...)
      ret
    }
  }
}

list_flattener <- function(x, i = 1, first = FALSE) {
  bind_rows(x) %>%
    mutate(get_id = paste(i - !first, row_number(), sep = "-")) %>%
    select(.data$get_id, everything()) %>%
    tidyr::unnest(everything(), names_sep = ".") %>%
    dplyr::as_tibble()
}

show_pag_status <- function(results, i = 1, sleep = 0, quiet = FALSE) {
  if (!quiet) {
    if (i == 1 & !"next" %in% names(results[["paging"]])) {
      # We can skip this message: no paginations
    } else {
      flush.console()
      total <- sum(unlist(lapply(results, nrow)))
      cat(paste(sprintf("\r>>> Pagination imported: %s | Total rows: %s", i, total)))
    }
  }
  Sys.sleep(sleep)
}


####################################################################
#' Facebook API Report Status Check
#'
#' This returns all available FB insights per day including any given
#' breakdown to the specified report level, and place into a data frame.
#' For more information on Ad Insights' API, go to the original
#' \href{https://developers.facebook.com/docs/marketing-api/insights/}{documentaion}.
#'
#' @family API
#' @family Meta
#' @inheritParams fb_insights
#' @inheritParams fb_process
#' @param report_run_id Integer. Report ID to check status.
#' @param live Boolean. Run until status report is finished?
#' @param sleep Boolean. If \code{live=TRUE}, then how many seconds should
#' we wait until next check?
#' @return List with API status results.
#' @examples
#' \dontrun{
#' token <- "YOURTOKEN"
#' report_run_id <- "123456789"
#' fb_report_check(token, report_run_id, live = TRUE, quiet = FALSE)
#' }
#' @export
fb_report_check <- function(token, report_run_id, api_version = NULL,
                            live = FALSE, sleep = 10, quiet = FALSE) {
  keep_running <- TRUE
  api_version <- ifelse(is.null(api_version), META_API_VER, api_version)
  URL <- glued("{META_GRAPH_URL}/{api_version}/{report_run_id}")
  while (isTRUE(keep_running)) {
    async_s <- httr::GET(url = URL, query = list(access_token = token))
    resp <- httr::content(async_s, encoding = "json")
    keep_running <- isTRUE(resp$async_percent_completion < 100 && isTRUE(live))
    if (!quiet) {
      flush.console()
      cat(sprintf("\rStatus: %s (%.0f%%)\n", resp$async_status, resp$async_percent_completion))
    }
    if (keep_running) Sys.sleep(sleep)
  }
  resp
}


####################################################################
#' Facebook Insights API
#'
#' This returns all available FB insights per day including any given
#' breakdown to the specified report level, and place into a data frame.
#' For more information on Ad Insights' API, go to the original
#' \href{https://developers.facebook.com/docs/marketing-api/insights/}{documentaion}.
#'
#' @family API
#' @family Meta
#' @inheritParams cache_write
#' @param token Character. Valid access token with sufficient privileges. Visit the
#' \href{https://developers.facebook.com/tools/explorer}{Facebook API Graph Explorer}
#' to acquire one.
#' @param which Character vector. This is the accounts, campaigns, adsets,
#' or ads IDs to be queried. Remember: if \code{report_level = "account"}, you must
#' start the ID with \code{act_}.
#' @param start_date,end_date Character. The first and last full day to report, in the
#' format \code{"YYYY-MM-DD"}.
#' @param time_increment Character. Group by months ("monthly"),
#' everything together ("all_days") or an integer per days [1-90].
#' Default: each day separately (i.e. "1").
#' @param report_level Character. One of "ad", "adset", "campaign", or "account"
#' @param ad_object Character. One of: "insights" (default), "adsets", ...
#' @param breakdowns Character Vector. One or more of breakdowns for
#' segmentation results. Set to NA for no breakdowns
#' @param fields Character, json format. Leave \code{NA} for default fields OR
#' \code{NULL} to ignore.
#' @param filtering List. Each filter will be a list containing "field",
#' "operator", and "value". Read more about the operators in the official
#' \href{https://developers.facebook.com/docs/marketing-api/insights}{docs}.
#' Example: \code{dplyr::tibble(field = "country", operator = "IN", value = list("PE")))}.
#' @param limit Integer. Query limit by pagination.
#' @param api_version Character. Facebook API version.
#' @param process Boolean. Process GET results to a more friendly format?
#' @param async Boolean. Run an async query. When set to \code{TRUE}, instead of making
#' a GET query, it'll run a POST query and will return a report run ID.
#' @return data.frame with un-nested processed results if \code{process=TRUE} or
#' raw API results as list when \code{process=FALSE}.
#' @examples
#' \dontrun{
#' token <- "YOURTOKEN"
#' which <- "act_20846447"
#'
#' # Platforms' Insights: all ad-sets platforms of "which" account,
#' # aggregated, for the last 30 days
#' platforms <- fb_insights(
#'   token, which,
#'   start_date = Sys.Date() - 30,
#'   time_increment = "all_days",
#'   report_level = "adset",
#'   fields = c(
#'     "account_name",
#'     "adset_id",
#'     "adset_start",
#'     "adset_end"
#'   ),
#'   breakdowns = c(
#'     "publisher_platform",
#'     "platform_position",
#'     "device_platform"
#'   )
#' )
#'
#' # Daily results for all campaigns of "which" account,
#' # with custom performance fields with no breakdowns.
#' insights_adset <- fb_insights(
#'   token, which,
#'   time_increment = "1",
#'   report_level = "campaign",
#'   fields = c(
#'     "adset_id",
#'     "reach",
#'     "frequency",
#'     "spend",
#'     "cpm",
#'     "objective",
#'     "optimization_goal"
#'   )
#' )
#' }
#' @export
fb_insights <- function(token,
                        which,
                        start_date = Sys.Date() - 7,
                        end_date = Sys.Date(),
                        time_increment = "1",
                        report_level = "campaign",
                        ad_object = "insights",
                        breakdowns = NA,
                        fields = NA,
                        filtering = NULL,
                        limit = 100,
                        api_version = NULL,
                        process = TRUE,
                        async = FALSE,
                        ...) {
  set_config(config(http_version = 0))
  check_opts(report_level, c("ad", "adset", "campaign", "account"))

  if (isTRUE(is.na(fields[1]))) {
    fields <- c(
      "campaign_name", "campaign_id",
      "objective", "adset_id",
      "adset_name", "ad_id",
      "ad_name", "impressions",
      "spend", "cpm", "ctr", "cpc",
      "reach", "clicks", "unique_clicks",
      "unique_ctr", "cost_per_unique_click"
    )
  }

  aux <- v2t(which, quotes = FALSE)
  api_version <- ifelse(is.null(api_version), META_API_VER, api_version)
  URL <- glued("{META_GRAPH_URL}/{api_version}/{aux}/{ad_object}")
  api_verb <- ifelse(async, httr::POST, httr::GET)

  # Call insights API
  import <- api_verb(
    URL,
    query = list(
      time_range = paste0('{\"since\":\"', start_date, '\",\"until\":\"', end_date, '\"}'),
      level = report_level,
      fields = if (length(fields) > 1) {
        vector2text(fields, sep = ",", quotes = FALSE)
      } else {
        fields
      },
      breakdowns = if (!is.na(breakdowns[1])) {
        vector2text(breakdowns, sep = ",", quotes = FALSE)
      } else {
        NULL
      },
      filtering = ifelse(!is.null(filtering), toJSON(filtering), ""),
      time_increment = time_increment,
      limit = as.character(limit),
      access_token = token
    ),
    encode = "json"
  )

  if (!process) {
    invisible(import)
  } else {
    fb_process(import, ...)
  }
}

####################################################################
#' Facebook Reach and Frequency API
#'
#' Create or query reach and frequency predictions using Facebook's
#' Reach and Frequency API. For more information on the API and its parameters, go to the
#' \href{https://developers.facebook.com/docs/marketing-api/insights}{original documentaion}.
#'
#' @family API
#' @family Meta
#' @inheritParams fb_insights
#' @param ad_account Character. Ad Account. Remember to start with \code{act_}. If you
#' use the \code{prediction} argument, no need to provide this parameter.
#' @param prediction Integer. Prediction ID if you already created the prediction
#' and wish to query the curve's data. As this prediction already exists, the
#' rest of arguments of this function will be ignored.
#' @param objective Character. Any of: "BRAND_AWARENESS", "LINK_CLICKS", "POST_ENGAGEMENT",
#' "MOBILE_APP_INSTALLS", "CONVERSIONS", "REACH", or "VIDEO_VIEWS".
#' @param days Integer. Amount of days for your campaign's predictions.
#' @param budget Integer. The budget in the Ad Account currency in cents.
#' @param destination_ids Integer vector. Page ID and/or Instagram Account ID.
#' @param countries Character vector. Country's acronyms.
#' @param frequency_cap Integer. Frequency cap over all the campaign duration.
#' @param prediction_mode Integer. "1" for predicting Reach by providing budget,
#' "2" is for predicting Budget given a specific Reach.
#' @param curve Boolean. Return curve data? If not, only prediction will be created.
#' @param ... Additional parameters passed to target specs.
#' @return data.frame with un-nested processed results if \code{process=TRUE} or
#' raw API results as list when \code{process=FALSE}.
#' @examples
#' \dontrun{
#' token <- "YOURTOKEN"
#' account_id <- "act_20846447"
#'
#' # BASIC 1: Create and return data for a new prediction
#' basic1 <- fb_rf(token, account_id, destination_ids = 187071108930, countries = "AR")
#'
#' # BASIC 2: Fetch data for an existing prediction ID
#' basic2 <- fb_rf(token, account_id, prediction = 6317720998974)
#'
#' # ADVANCED (Fully custom prediction)
#' advanced <- fb_rf(token, account_id,
#'   objective = "REACH",
#'   days = 28,
#'   budget = 2000000,
#'   destination_ids = c(187071108930, 1142958119078556),
#'   age_min = 15,
#'   age_max = 65,
#'   genders = 2,
#'   countries = "MX",
#'   publisher_platforms = c(
#'     "facebook",
#'     "instagram",
#'     #' audience_network',
#'     "messenger"
#'   ),
#'   # interests_ids = NA,
#'   facebook_positions = c(
#'     "feed",
#'     #' instant_article',
#'     "marketplace",
#'     "video_feeds",
#'     "story",
#'     "search",
#'     "instream_video"
#'   ),
#'   instagram_positions = c(
#'     "stream",
#'     "story",
#'     "explore"
#'   ),
#'   # audience_network_positions = c(
#'   #  'classic',
#'   #  'instream_video')
#'   messenger_positions = c(
#'     "messenger_home",
#'     "sponsored_messages",
#'     "story"
#'   ),
#'   device_platforms = c(
#'     "mobile",
#'     "desktop"
#'   )
#' )
#' }
#' @export
fb_rf <- function(token,
                  ad_account = NA,
                  prediction = NA,
                  objective = "REACH",
                  days = 28,
                  budget = 2e6,
                  destination_ids = NA,
                  countries = "MX",
                  frequency_cap = 8,
                  prediction_mode = 1,
                  curve = TRUE,
                  api_version = NULL,
                  process = TRUE,
                  ...) {
  set_config(config(http_version = 0))

  if (is.na(prediction[1])) {
    message(">>> Creating prediction...")

    # Transform target_spec from list to JSON format
    target_spec <- list(
      geo_locations = if (length(countries) > 0) {
        list(countries = countries)
      } else {
        NULL
      },
      ...
    )
    ts <- toJSON(target_spec)

    # Force some values that must be integers
    if ("age_min" %in% names(target_spec)) {
      ts <- gsub(paste0("\\[", target_spec[["age_min"]], "\\]"), target_spec[["age_min"]], ts)
    }
    if ("age_max" %in% names(target_spec)) {
      ts <- gsub(paste0("\\[", target_spec[["age_max"]], "\\]"), target_spec[["age_max"]], ts)
    }

    # Create a prediction (returns ID if successful)
    daysecs <- 60 * 60 * 24
    current_time <- as.integer(Sys.time())
    api_version <- ifelse(is.null(api_version), META_API_VER, api_version)
    prediction <- content(POST(
      glued("{META_GRAPH_URL}/{api_version}/{ad_account}/reachfrequencypredictions"),
      query = list(
        start_time = current_time + daysecs,
        end_time = min(
          (current_time + daysecs + (days) * daysecs),
          (current_time + daysecs + (89 * daysecs))
        ),
        objective = toupper(objective),
        frequency_cap = frequency_cap,
        prediction_mode = prediction_mode,
        budget = as.integer(budget),
        destination_ids = toJSON(as.character(destination_ids)),
        target_spec = ts,
        access_token = token
      )
    ), encode = "json")[[1]]

    if ("message" %in% names(prediction)) {
      message(paste(
        prediction$message,
        prediction$error_user_title,
        prediction$error_user_msg,
        sep = "\n"
      ))
      invisible(prediction)
    } else {
      message(glued("Prediction created: {prediction}"))

      if (curve) {
        if (length(prediction) > 1) {
          stop("Please, provide only 1 prediction per query")
        }
        api_version <- ifelse(is.null(api_version), META_API_VER, api_version)
        curves <- GET(
          glued("{META_GRAPH_URL}/{api_version}/{prediction}"),
          query = list(
            fields = "curve_budget_reach",
            access_token = token
          )
        )

        if (process) {
          this <- fb_process(curves, ...)
          curves <- this %>%
            select(-one_of(zerovar(.))) %>%
            mutate(
              budget = .data$budget / 100,
              frequency = .data$impression / .data$reach,
              weekly_frequency = .data$frequency / (days / 7),
              cpm = 1000 * .data$budget / .data$impression,
              prediction = prediction
            )
          curves <- curves %>%
            as.matrix() %>%
            data.frame() %>%
            as_tibble() %>%
            mutate_at(colnames(.)[-c(1, ncol(.))], as.numeric)
        }

        attr(curves, "prediction") <- prediction
        class(curves) <- c(class(curves), "fb_rf")
        curves
      } else {
        prediction
      }
    }
  }
}


####################################################################
#' Facebook Ad Accounts
#'
#' This returns all ad accounts for a FB Business Account FB.
#' For more information on Ad Insights' API, go to the
#' \href{https://developers.facebook.com/docs/marketing-api/insights/}{original documentaion}
#'
#' @family API
#' @family Meta
#' @inheritParams fb_insights
#' @inheritParams fb_process
#' @param business_id Character. Business ID.
#' @param type Character vector. Values: owned, client.
#' @return data.frame with un-nested processed results fetched with API.
#' @examples
#' \dontrun{
#' # Query all accounts (owned and with permissions) of a Business ID
#' accounts <- fb_accounts(YOURTOKEN, YOURBUSINESS)
#' }
#' @export
fb_accounts <- function(token,
                        business_id = "904189322962915",
                        type = c("owned", "client"),
                        limit = 100,
                        api_version = NULL,
                        ...) {
  set_config(config(http_version = 0))
  api_version <- ifelse(is.null(api_version), META_API_VER, api_version)
  output <- NULL

  # Select which type of ad accounts
  type <- paste0(type, "_ad_accounts")

  for (i in seq_along(type)) {
    message(paste(">>> Fetching", type[i]))
    URL <- paste(META_GRAPH_URL, api_version, business_id, type[i], sep = "/")
    continue <- TRUE

    # Call insights
    import <- GET(
      URL,
      query = list(
        fields = "name,account_status,amount_spent,business_country_code",
        limit = as.character(limit),
        access_token = token
      ),
      encode = "json"
    )
    ret <- fb_process(import, quiet = TRUE, ...)

    if (inherits(ret, "data.frame")) {
      ret$type <- type[i]
      output <- bind_rows(output, ret)
    }
  }

  if (!inherits(output, "data.frame")) {
    invisible(NULL)
  } else {
    # Account status dictionary
    output <- mutate(output, account_status = case_when(
      account_status == "1" ~ "ACTIVE",
      account_status == "2" ~ "DISABLED",
      account_status == "3" ~ "UNSETTLED",
      account_status == "7" ~ "PENDING_RISK_REVIEW",
      account_status == "8" ~ "PENDING_SETTLEMENT",
      account_status == "9" ~ "IN_GRACE_PERIOD",
      account_status == "100" ~ "PENDING_CLOSURE",
      account_status == "101" ~ "CLOSED",
      account_status == "201" ~ "ANY_ACTIVE",
      account_status == "202" ~ "ANY_CLOSED"
    ))
    suppressMessages(type.convert(
      output,
      numerals = "no.loss", as.is = TRUE
    )) %>%
      arrange(desc(.data$amount_spent)) %>%
      as_tibble()
  }
}

####################################################################
#' Facebook's Long-Life User API Token
#'
#' Using a 1-hour generic user token you can generate a 60 day token.
#' You will need to have an App ID and App secret, and a valid token.
#' Generate a new valid User Token with the
#' \href{https://developers.facebook.com/tools/explorer}{API Graph}.
#'
#' More info: \href{https://developers.facebook.com/docs/facebook-login/guides/access-tokens/get-long-lived}{Long-Lived Access Tokens}
#'
#' @family API
#' @family Meta
#' @inheritParams fb_insights
#' @param app_id,app_secret Character. Application ID and Secret.
#' @param token Character. User token, created with
#' \href{https://developers.facebook.com/tools/explorer}{API Graph}
#' or with this same \code{fb_token()}'s token.
#' @return Character. String with token requested. If successful, it'll contain
#' an attribute called "expiration" with date and time of expiration.
#' @export
fb_token <- function(app_id, app_secret, token, api_version = NULL) {
  api_version <- ifelse(is.null(api_version), META_API_VER, api_version)
  link <- glued(paste0(
    "{META_GRAPH_URL}/", api_version, "/oauth/access_token?",
    "grant_type=fb_exchange_token&client_id=%s&client_secret=%s&fb_exchange_token=%s"
  ))
  linkurl <- sprintf(link, app_id, app_secret, token)
  ret <- content(GET(linkurl), encode = "json")
  if ("access_token" %in% names(ret)) {
    expiration <- Sys.time() + ret$expires_in
    message(sprintf("Your token was created succesfully. It will expire on %s", expiration))
    output <- ret$access_token
    attr(output, "expiration") <- expiration
    output
  } else {
    message(ret$error$message)
    ret
  }
}

####################################################################
#' Facebook Creatives API
#'
#' For more information: \href{https://developers.facebook.com/docs/marketing-apis}{Marketing API}
#'
#' @family API
#' @family Meta
#' @inheritParams fb_process
#' @inheritParams fb_insights
#' @return data.frame with un-nested processed results if \code{process=TRUE} or
#' raw API results as list when \code{process=FALSE}.
#' @examples
#' \dontrun{
#' token <- YOURTOKEN
#' account <- act_ADACCOUNT
#'
#' # Query all creatives for "which" (account in this case)
#' creatives <- fb_creatives(token, account)
#' }
#' @export
fb_creatives <- function(token, which,
                         api_version = NULL,
                         process = TRUE,
                         ...) {
  set_config(config(http_version = 0))
  api_version <- ifelse(is.null(api_version), META_API_VER, api_version)

  fields <- c(
    "account_id", "object_type", "name", "status", "campaign_id",
    "call_to_action_type", "image_url", "thumbnail_url"
  )
  link <- glued(paste0(
    "{META_GRAPH_URL}/%s/%s/adcreatives?",
    "grant_type=fb_exchange_token",
    "&fields=%s",
    "&access_token=%s"
  ))
  linkurl <- sprintf(
    link, api_version, which,
    vector2text(fields, sep = ",", quotes = FALSE),
    token
  )
  import <- GET(linkurl)
  if (!process) {
    import
  } else {
    fb_process(import, ...)
  }
}

####################################################################
#' Facebook Ads API
#'
#' This returns all available FB ads for any account, campaign, or ad set id.
#' For more information on Ad' API, go to the
#' \href{https://developers.facebook.com/docs/marketing-api/reference/adgroup}{original documentaion}
#'
#' This function was based on FBinsightsR.
#'
#' @family API
#' @family Meta
#' @inheritParams fb_insights
#' @inheritParams fb_process
#' @return data.frame with un-nested processed results if \code{process=TRUE} or
#' raw API results as list when \code{process=FALSE}.
#' @examples
#' \dontrun{
#' token <- YOURTOKEN
#' account <- act_ADACCOUNT
#'
#' # Query all ads for "which" (account) with results in the last 10 days
#' ads <- fb_ads(token, account, start_date = Sys.Date() - 10)
#' }
#' @export
fb_ads <- function(token,
                   which,
                   start_date = Sys.Date() - 31,
                   end_date = Sys.Date(),
                   fields = NA,
                   api_version = NULL,
                   process = TRUE,
                   ...) {
  set_config(config(http_version = 0))
  api_version <- ifelse(is.null(api_version), META_API_VER, api_version)

  if (is.na(fields[1])) {
    fields <- paste(
      "account_id, campaign, campaign_id, objective, adset_id, id,",
      "name, source_ad, cpm, spend, recommendations"
    )
  }

  import <- GET(
    glued("{META_GRAPH_URL}/{api_version}/{which}/ads"),
    query = list(
      time_range = paste0('{\"since\":\"', start_date, '\",\"until\":\"', end_date, '\"}'),
      fields = if (length(fields) > 1) {
        vector2text(fields, sep = ",", quotes = FALSE)
      } else {
        fields
      },
      access_token = token
    ),
    encode = "json"
  )

  if (!process) {
    import
  } else {
    fb_process(import, ...)
  }
}
