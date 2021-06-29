####################################################################
#' Process Facebook's API Objects
#'
#' Process and paginate raw results from Facebook's API, result of
#' querying the API with \code{httr::GET}.
#'
#' @family API
#' @family Facebook
#' @param response GET's output object, class response
#' @param paginate Boolean. Run through all paginations? If not,
#' only the first one will be processed.
#' @return data.frame with un-nested processed results or NULL if no results found.
#' @export
fb_process <- function(response, paginate = TRUE) {
  if (!"response" %in% class(response)) {
    stop("You must provide a response object (GET's output)")
  }

  import <- content(response)

  # Show and return error
  if ("error" %in% names(import)) {
    message(paste("API ERROR:", import$error$message))
    invisible(return(import$error))
  }

  # Check for no-data (user might think there was an error on GET request - warn him!)
  if (length(import[[1]]) == 0) {
    message("No data found!")
    invisible(return(list(NULL)))
  }

  # Deal with GET+response vs jsonlite
  import <- fromJSON(toJSON(import))

  # First pagination
  results <- list()
  i <- 1
  results[[i]] <- .flattener(import[[1]])

  # Following iterations
  if (exists("paging", import) & paginate) {
    if (exists("next", import$paging)) {
      i <- i + 1
      out <- fromJSON(import$paging$`next`)
      results[[i]] <- .flattener(out$data, i)
      # Re-run first iteration as everything MUST match to bind
      if (i == 2) {
        out <- fromJSON(out$paging$`previous`)
        results[[1]] <- .flattener(out$data, i)
      }
      while (exists("next", out$paging)) {
        i <- i + 1
        out <- fromJSON(out$paging$`next`)
        results[[i]] <- .flattener(out$data, i)
      }
    }
  }
  done <- bind_rows(results)

  # So columns that consist in lists but only have 1 element may be used as normal vectors
  are_lists <- unlist(lapply(lapply(done, class), function(x) any(x %in% c("data.frame", "matrix"))))
  nolists <- done[, !are_lists]
  nolists <- replace(nolists, nolists == "NULL", NA)
  total <- bind_cols(lapply(nolists, as.character))

  fixedlists <- bind_cols(lapply(
    apply(done[are_lists], 2, function(x) {
      if (all(unlist(lapply(x, length) <= 1))) {
        x[unlist(lapply(x, is.null))] <- NA
        unlist(x)
      }
    }), function(x) {
      if (!is.null(x)) {
        return(x)
      }
    }
  ))

  if (nrow(fixedlists) > 0) {
    total <- cbind(total, fixedlists, done[are_lists])
  }

  ret <- suppressMessages(type.convert(
    total,
    numerals = "no.loss", as.is = TRUE
  )) %>%
    mutate_at(vars(contains("date")), list(as.Date)) %>%
    mutate_at(vars(contains("id")), list(as.character)) %>%
    mutate_at(vars(contains("url")), list(as.character)) %>%
    mutate_at(vars(contains("name")), list(as.character)) %>%
    as_tibble()
  return(ret)
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
#' @family Facebook
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
#' @param fields Character, json format. Leave \code{NA} for default fields.
#' @param limit Integer. Query limit
#' @param api_version Character. Facebook API version
#' @param process Boolean. Process GET results to a more friendly format?
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
                        limit = 10000,
                        api_version = "v10.0",
                        process = TRUE) {
  set_config(config(http_version = 0))
  check_opts(report_level, c("ad", "adset", "campaign", "account"))

  if (is.na(fields[1])) {
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
  URL <- glued("https://graph.facebook.com/{api_version}/{aux}/{ad_object}")

  # Call insights
  import <- GET(
    URL,
    query = list(
      access_token = token,
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
      time_increment = time_increment,
      limit = as.character(limit)
    ),
    encode = "json"
  )

  if (!process) {
    return(import)
  }
  output <- fb_process(import)
  return(as_tibble(output))
}

####################################################################
#' Facebook Reach and Frequency API
#'
#' Create or query reach and frequency predictions using Facebook's
#' Reach and Frequency API. For more information on the API and its parameters, go to the
#' \href{https://developers.facebook.com/docs/marketing-api/insights}{original documentaion}.
#'
#' @family API
#' @family Facebook
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
#' basic2 <- fb_rf(token, account_id, prediction = 6260368700774)
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
                  api_version = "v10.0",
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
    prediction <- content(POST(
      glued("https://graph.facebook.com/{api_version}/{ad_account}/reachfrequencypredictions"),
      query = list(
        access_token = token,
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
        target_spec = ts
      ),
      encode = "json"
    ))[[1]]

    if ("message" %in% names(prediction)) {
      message(paste(
        prediction$message,
        prediction$error_user_title,
        prediction$error_user_msg,
        sep = "\n"
      ))
      return(invisible(prediction))
    }
    message(glued("Prediction created: {prediction}"))
  }

  if (curve) {
    if (length(prediction) > 1) {
      stop("Please, provide only 1 prediction per query")
    }
    curves <- GET(
      glued("https://graph.facebook.com/{api_version}/{prediction}"),
      query = list(
        access_token = token,
        fields = "curve_budget_reach"
      )
    )

    if (process) {
      curves <- fb_process(curves) %>%
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
    return(curves)
  }
  return(prediction)
}

####################################################################
#' Get Facebook's Page Posts (API Graph)
#'
#' Connect to an API Graph's token of a given page and get posts,
#' comments, shares, and reactions of n posts (with no limits).
#'
#' @family API
#' @family Facebook
#' @inheritParams fb_insights
#' @param n Integer. How many most recent posts do you need?
#' @param limits Integer. For each post, hoy many results do you need?
#' @param comments,shares,reactions Boolean. Include in your query?
#' @return data.frame with un-nested processed results fetched with API.
#' @examples
#' \dontrun{
#' token <- YOURTOKEN
#' # Query latest 10 posts and 50 comments for each
#' posts <- fb_posts(token, n = 10, limits = 50, comments = TRUE)
#' }
#' @export
fb_posts <- function(token,
                     n = 150,
                     limits = 100,
                     comments = FALSE,
                     shares = FALSE,
                     reactions = FALSE) {

  # TOKEN: https://developers.facebook.com/tools/explorer/
  # require(httr)
  # require(jsonlite)
  # require(rlist)
  # require(dplyr)

  set_config(config(http_version = 0))

  fb_comments <- function(posts) {
    comments <- NULL
    if ("data" %in% names(posts)) posts$posts$data <- posts$data
    iters <- ifelse("comments" %in% names(posts$posts$data),
      length(posts$posts$data$comments$data),
      length(posts$posts$data)
    )
    for (i in 1:iters) {
      if (length(posts$posts$data$comments$data) > 0) {
        all <- posts$posts$data$comments$data[[i]]
        id <- posts$posts$data$id[[i]]
      } else {
        all <- data.frame(lapply(posts$posts$data, `[`, "comments")[[i]])
        id <- posts$posts$data[[i]]$id
      }
      if ("data" %in% names(posts)) posts$posts$data <- posts$data
      if (length(all) > 0) {
        ids <- c(t(select(all, starts_with("id"))))
        times <- c(t(select(all, starts_with("created_time"))))
        times <- as.POSIXct(times, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
        coms <- c(t(select(all, starts_with("message"))))
        names <- c(t(select(all, starts_with("from"))))
        name <- names[seq(1, 2 * nrow(all), 2)]
        name_id <- names[seq(2, 2 * nrow(all), 2)]
        dfi <- data.frame(
          post_id = id,
          comment_id = ids,
          comment_time = times,
          comment = coms,
          name = name,
          name_id = name_id
        ) %>%
          filter(comment != "")
        comments <- rbind(comments, dfi)
      }
    }
    return(comments)
  }

  fb_reactions <- function(posts) {
    reactions <- NULL
    if ("data" %in% names(posts)) posts$posts$data <- posts$data
    iters <- ifelse("reactions" %in% names(posts$posts$data),
      length(posts$posts$data$reactions$data),
      length(posts$posts$data)
    )
    for (i in 1:iters) {
      if (length(posts$posts$data$reactions$data) > 0) {
        all <- posts$posts$data$reactions$data[[i]]
        id <- posts$posts$data$id[[i]]
      } else {
        all <- data.frame(lapply(posts$posts$data, `[`, "reactions")[[i]])
        id <- posts$posts$data[[i]]$id
      }
      if (length(all) > 0) {
        ids <- c(t(select(all, starts_with("id"))))
        reac <- c(t(select(all, starts_with("type"))))
        name <- c(t(select(all, starts_with("name"))))
        dfi <- data.frame(
          post_id = id,
          reaction_id = ids,
          reaction = reac,
          name = name
        )
        reactions <- rbind(reactions, dfi)
      }
    }
    return(reactions)
  }

  fb_shares <- function(posts) {
    shares <- NULL
    if ("data" %in% names(posts)) posts$posts$data <- posts$data
    if (length(posts$posts$data$shares) > 0) {
      all <- posts$posts$data$shares$count
      id <- posts$posts$data$id
    } else {
      for (i in seq_along(posts$posts$data)) {
        all <- posts$posts$data[[i]]$shares$count
        id <- posts$posts$data[[i]]$id
      }
    }
    shares <- data.frame(post_id = id, shares = all)
    return(shares)
  }

  fb_pposts <- function(posts) {
    if ("data" %in% names(posts)) posts$posts$data <- posts$data
    plinks <- data.frame(
      id = posts$posts$data$id,
      created_time = as.POSIXct(posts$posts$data$created_time,
        format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"
      ),
      url = posts$posts$data$permalink_url,
      message = posts$posts$data$message,
      status_type = posts$posts$data$status_type
    )
    return(plinks)
  }

  limit_posts <- 100
  total_iters <- ceiling(n / limit_posts)
  all_comments <- all_shares <- all_reactions <- all_posts <- ret <- NULL

  for (iter in 1:total_iters) {
    limit_posts <- ifelse(iter == total_iters, limit_posts - (iter * limit_posts - n), limit_posts)
    limit_posts <- ifelse(n < limit_posts, n, limit_posts)
    if (iter == 1) {
      url <- paste0(
        "https://graph.facebook.com/v3.3/me?fields=",
        "id,name,posts.limit(", limit_posts, ")",
        "{created_time,message,status_type,",
        ifelse(comments, paste0("comments.limit(", limits, "),"), ""),
        ifelse(reactions, paste0("reactions.limit(", limits, "),"), ""),
        ifelse(shares, "shares,", ""),
        "permalink_url}",
        "&access_token=", token
      )
    } else {
      url <- new_url
    }
    get <- GET(url = url)
    char <- rawToChar(get$content)
    json <- fromJSON(char)
    if ("error" %in% names(json)) {
      if (grepl("expired", json$error$message)) {
        message("You must be logged in to your Facebook account and refresh/get the token!")
        message("1. Within the Graph API Explorer, select an Application.")
        message(
          "2. Go to Get Token and select the Page Acces Token needed. ",
          "Note that there are navigation arrows if lots of accounts axists."
        )
        message("3. Copy and use the Acces Token created.")
        url <- paste0(
          "https://developers.facebook.com/tools/explorer/1866795993626030/",
          "?version=v3.3&classic=1"
        )
        browseURL(url)
      }
      error <- paste("API ERROR:", json$error$message)
      return(error)
    }

    if (iter == 1) ret[["account"]] <- data.frame(id = json$id, name = json$name)
    all_posts <- rbind(all_posts, fb_pposts(json))
    all_comments <- if (comments) rbind(all_comments, fb_comments(json))
    all_reactions <- if (reactions) rbind(all_reactions, fb_reactions(json))
    all_shares <- if (shares) rbind(all_shares, fb_shares(json))
    new_url <- ifelse(length(json$paging) > 0, json$paging$`next`, json$posts$paging$`next`)
    if (total_iters > 1) statusbar(iter, total_iters)
  }
  ret[["posts"]] <- all_posts
  ret[["comments"]] <- all_comments
  ret[["reactions"]] <- all_reactions
  ret[["shares"]] <- all_shares
  # ret[["json"]] <- json

  msg <- paste("Succesfully exported", n, "posts from", ret$account$name, "with")
  msg <- ifelse(!is.null(ret$comments), paste(msg, nrow(ret$comments), "comments,"), msg)
  msg <- ifelse(!is.null(ret$reactions), paste(msg, nrow(ret$reactions), "reactions,"), msg)
  msg <- ifelse(!is.null(ret$shares), paste(msg, nrow(ret$shares), "reactions,"), msg)
  msg <- paste(msg, "and one happy client! :)")
  ret[["msg"]] <- msg
  message(msg)
  return(ret)
}


####################################################################
#' Get Facebook's Post Comments (API Graph)
#'
#' Connect to an API Graph's token and get posts comments given the
#' post(s) id.
#'
#' @family API
#' @family Facebook
#' @inheritParams fb_insights
#' @param post_id Character vector. Post id(s)
#' @return data.frame with un-nested processed results fetched with API.
#' @examples
#' \dontrun{
#' token <- YOURTOKEN
#' ids <- c(POST_ID1, POST_ID2)
#'
#' # Query 50 comments for two post ids
#' posts <- fb_post(token, ids, 50)
#' }
#' @export
fb_post <- function(token, post_id, limit = 5000) {
  set_config(config(http_version = 0))

  iters <- length(post_id)
  for (i in 1:iters) {
    if (i == 1) ret <- NULL
    if (i == 1) nodata <- NULL
    url <- paste0(
      "https://graph.facebook.com/v3.0/", post_id[i],
      "/comments?limit=", limit, "&access_token=", token
    )
    get <- GET(url = url)
    char <- rawToChar(get$content)
    json <- fromJSON(char)

    if ("error" %in% names(json)) {
      if (grepl("expired", json$error$message)) {
        message("You must be logged in to your Facebook account and refresh/get the token!")
        message("1. Within the Graph API Explorer, select an Application.")
        message(
          "2. Go to Get Token and select the Page Acces Token needed. ",
          "Note that there are navigation arrows if lots of accounts axists."
        )
        message("3. Copy and use the Acces Token created.")
        url <- paste0(
          "https://developers.facebook.com/tools/explorer/1866795993626030/",
          "?version=v3.3&classic=1"
        )
        browseURL(url)
      }
      error <- paste("API ERROR:", json$error$message)
      return(error)
    } else {
      if (length(json$data) != 0) {
        json$data$post_id <- post_id[i]
        json$data$created_time <- as.POSIXct(
          json$data$created_time,
          format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"
        )
        json$data$from <- json$data$from$name
        json$data <- as.data.frame(json$data, row.names = paste0(i, ":", rownames(json$data)))
        ret <- rbind(ret, json$data)
      } else {
        nodata <- c(nodata, post_id[i])
      }
    }
    if (iters > 1) statusbar(i, iters)
  }
  if (i == iters & length(nodata) > 0) {
    message(paste("NO DATA: no comments on", vector2text(nodata)))
  }
  return(as_tibble(ret))
}


####################################################################
#' Facebook Ad Accounts
#'
#' This returns all ad accounts for a FB Business Account FB.
#' For more information on Ad Insights' API, go to the
#' \href{https://developers.facebook.com/docs/marketing-api/insights/}{original documentaion}
#'
#' @family API
#' @family Facebook
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
                        limit = 1000,
                        api_version = "v8.0") {
  set_config(config(http_version = 0))

  # Starting URL
  url <- "https://graph.facebook.com/"
  output <- NULL

  # Select which type of ad accounts
  type <- paste0(type, "_ad_accounts")

  for (i in seq_along(type)) {
    message(paste("Getting", type[i]))
    URL <- paste0(url, api_version, "/", business_id, "/", type[i])
    continue <- TRUE

    # Call insights
    import <- GET(
      URL,
      query = list(
        access_token = token,
        fields = "name,account_status,amount_spent,business_country_code",
        limit = as.character(limit)
      ),
      encode = "json"
    )

    ret <- fb_process(import)

    if ("data.frame" %in% class(ret)) {
      ret$type <- type[i]
      output <- bind_rows(output, ret)
    }
  }

  if (class(output) != "data.frame") {
    invisible(return(NULL))
  }

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

  output <- suppressMessages(type.convert(output, numerals = "no.loss")) %>%
    arrange(desc(data$.amount_spent)) %>%
    as_tibble()

  return(output)
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
#' @family Facebook
#' @inheritParams fb_insights
#' @inheritParams fb_process
#' @return data.frame with un-nested processed results if \code{process=TRUE} or
#' raw API results as list when \code{process=FALSE}.
#' @examples
#' \dontrun{
#' token <- YOURTOKEN
#' which <- act_ADACCOUNT
#'
#' # Query all ads for "which" with results in the last 10 days
#' ads <- fb_accounts(YOURTOKEN, which, start_date = Sys.Date() - 10)
#' }
#' @export
fb_ads <- function(token,
                   which,
                   start_date = Sys.Date() - 31,
                   end_date = Sys.Date(),
                   fields = NA,
                   api_version = "v8.0",
                   process = TRUE) {
  set_config(config(http_version = 0))

  if (is.na(fields[1])) {
    fields <- paste(
      "campaign_name, campaign_id, objective, adset_id, adset_name, ad_id, ad_name,",
      "impressions, cpm, spend, reach, clicks, unique_clicks, ctr, cpc, unique_ctr,",
      "cost_per_unique_click"
    )
  }

  # Call insights
  import <- GET(
    glued("https://graph.facebook.com/{api_version}/{which}/ads"),
    query = list(
      access_token = token,
      time_range = paste0('{\"since\":\"', start_date, '\",\"until\":\"', end_date, '\"}'),
      fields = if (length(fields) > 1) {
        vector2text(fields, sep = ",", quotes = FALSE)
      } else {
        fields
      }
    ),
    encode = "json"
  )

  if (!process) {
    return(import)
  }

  ret <- fb_process(import)
  if ("data.frame" %in% class(ret)) {
    ret <- ret %>%
      # rename(adcreatives_id = .data$list_id) %>%
      # arrange(desc(.data$created_time)) %>%
      as_tibble()
    return(ret)
  }
}


####################################################################
#' Facebook Creatives API
#'
#' For more information: \href{https://developers.facebook.com/docs/marketing-api/reference/ad-creative/}{Ad Creative}
#'
#' @family API
#' @family Facebook
#' @inheritParams fb_process
#' @inheritParams fb_insights
#' @return data.frame with un-nested processed results if \code{process=TRUE} or
#' raw API results as list when \code{process=FALSE}.
#' @examples
#' \dontrun{
#' token <- YOURTOKEN
#' which <- act_ADACCOUNT
#'
#' # Query all creatives for "which"
#' creatives <- fb_creatives(YOURTOKEN, which)
#' }
#' @export
fb_creatives <- function(token, which,
                         api_version = "v8.0",
                         process = TRUE) {
  set_config(config(http_version = 0))

  fields <- c(
    "account_id", "object_type", "name", "status", "campaign_id",
    "call_to_action_type", "image_url", "thumbnail_url"
  )
  link <- paste0(
    "https://graph.facebook.com/%s/%s/adcreatives?",
    "grant_type=fb_exchange_token",
    "&fields=%s",
    "&access_token=%s"
  )
  linkurl <- sprintf(
    link, api_version, which,
    vector2text(fields, sep = ",", quotes = FALSE),
    token
  )
  import <- GET(linkurl)
  if (!process) {
    return(import)
  }
  ret <- fb_process(import)
  if ("data.frame" %in% class(ret)) {
    ret <- select(ret, one_of("id", .data$fields))
  }
  attr(ret, "cURL") <- linkurl
  return(ret)
}


####################################################################
#' Facebook's Long Life User Token
#'
#' Using a 1-hour generic user token you can generate a 60 day token.
#' You will need to have an App ID and App secret, and a valid token.
#' Generate a new valid User Token with the
#' \href{https://developers.facebook.com/tools/explorer}{API Graph}.
#'
#' More info: \href{https://developers.facebook.com/docs/facebook-login/access-tokens/refreshing/}{Long-Lived Access Tokens}
#'
#' @family API
#' @family Facebook
#' @inheritParams fb_insights
#' @param app_id,app_secret Character. Application ID and Secret.
#' @param token Character. User token, created with
#' \href{https://developers.facebook.com/tools/explorer}{API Graph}
#' or with this same \code{fb_token()}'s token.
#' @return Character. String with token requested.
#' @export
fb_token <- function(app_id, app_secret, token, api_version = "v10.0") {
  link <- paste0(
    "https://graph.facebook.com/", api_version, "/oauth/access_token?",
    "grant_type=fb_exchange_token&client_id=%s&client_secret=%s&fb_exchange_token=%s"
  )
  linkurl <- sprintf(link, app_id, app_secret, token)
  ret <- content(GET(linkurl))
  if ("access_token" %in% names(ret)) {
    message(sprintf(
      "Your token was created succesfully. It will expire on %s",
      Sys.time() + ret$expires_in
    ))
    return(ret$access_token)
  } else {
    return(ret)
  }
}


.flattener <- function(x, i = 1) {
  bind_rows(x) %>%
    mutate(get_id = paste(i, row_number(), sep = "-")) %>%
    select(.data$get_id, everything()) %>%
    data.frame()
}
