####################################################################
#' Get Facebook's Page Posts (API Graph)
#' 
#' Connect to an API Graph's token of a given page and get posts, 
#' comments, shares, and reactions of n posts (with no limits).
#' 
#' @family Scrapper
#' @family API
#' @family Facebook
#' @param token Character. Access token. Generate it for any of your
#' users or apps in \url{https://developers.facebook.com/tools/explorer}
#' @param total Integer. How many most recent posts do you need?
#' @param limit Integer. For each post, hoy many results do you need?
#' @param comments,shares,reactions Boolean. Include in your query?
#' @export
fb_posts <- function(token, 
                     total = 150, 
                     limit = 100,
                     comments = FALSE, 
                     shares = FALSE, 
                     reactions = FALSE) {
  
  #TOKEN: https://developers.facebook.com/tools/explorer/
  # require(httr)
  # require(jsonlite)
  # require(rlist)
  # require(dplyr)
  
  fb_comments <- function(posts) {
    comments <- c()
    if ("data" %in% names(posts)) posts$posts$data <- posts$data
    iters <- ifelse("comments" %in% names(posts$posts$data),
                    length(posts$posts$data$comments$data),
                    length(posts$posts$data))
    for (i in 1:iters) {
      if (length(posts$posts$data$comments$data) > 0) {
        all <- posts$posts$data$comments$data[[i]]
        id <- posts$posts$data$id[[i]]
      } else {
        all <- data.frame(lapply(posts$posts$data, `[`, c('comments'))[[i]]) 
        id <- posts$posts$data[[i]]$id  
      }
      if ("data" %in% names(posts)) posts$posts$data <- posts$data
      if (length(all) > 0) {
        ids <- c(t(select(all, starts_with("id")))) 
        times <- c(t(select(all, starts_with("created_time"))))
        times <- as.POSIXct(times, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
        coms <- c(t(select(all, starts_with("message")))) 
        names <- c(t(select(all, starts_with("from"))))
        name <- names[seq(1, 2*nrow(all),2)]
        name_id <- names[seq(2, 2*nrow(all),2)]
        dfi <- data.frame(post_id = id, comment_id = ids, 
                          comment_time = times, comment = coms,
                          name = name, name_id = name_id) %>%
          filter(comment != "")
        comments <- rbind(comments, dfi)
      }
    }
    return(comments)
  }  
  
  fb_reactions <- function(posts) {
    reactions <- c()
    if ("data" %in% names(posts)) posts$posts$data <- posts$data
    iters <- ifelse("reactions" %in% names(posts$posts$data),
                    length(posts$posts$data$reactions$data),
                    length(posts$posts$data))
    for (i in 1:iters) {
      if (length(posts$posts$data$reactions$data) > 0) {
        all <- posts$posts$data$reactions$data[[i]]
        id <- posts$posts$data$id[[i]]
      } else {
        all <- data.frame(lapply(posts$posts$data, `[`, c('reactions'))[[i]]) 
        id <- posts$posts$data[[i]]$id  
      }
      if (length(all) > 0) {
        ids <- c(t(select(all, starts_with("id")))) 
        reac <- c(t(select(all, starts_with("type")))) 
        name <- c(t(select(all, starts_with("name")))) 
        dfi <- data.frame(post_id = id, reaction_id = ids, reaction = reac, name = name)
        reactions <- rbind(reactions, dfi)
      }
    }
    return(reactions)
  }
  
  fb_shares <- function(posts) {
    shares <- c()
    if ("data" %in% names(posts)) posts$posts$data <- posts$data
    if (length(posts$posts$data$shares) > 0) {
      all <- posts$posts$data$shares$count
      id <- posts$posts$data$id
    } else {
      for (i in 1:length(posts$posts$data)) {
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
                                format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
      url = posts$posts$data$permalink_url,
      message = posts$posts$data$message,
      status_type = posts$posts$data$status_type)
    return(plinks)
  }
  
  limit_posts <- 100
  total_iters <- ceiling(total/limit_posts)
  all_comments <- all_shares <- all_reactions <- all_posts <- ret <- c()
  
  for (iter in 1:total_iters) {
    limit_posts <- ifelse(iter == total_iters, limit_posts - (iter*limit_posts - total), limit_posts)
    limit_posts <- ifelse(total < limit_posts, total, limit_posts)
    if (iter == 1) {
      url <- paste0("https://graph.facebook.com/v3.3/me?fields=",
                    "id,name,posts.limit(",limit_posts,")",
                    "{created_time,message,status_type,",
                    ifelse(comments, paste0("comments.limit(",limit,"),"), ""),
                    ifelse(reactions, paste0("reactions.limit(",limit,"),"), ""),
                    ifelse(shares, "shares,", ""),
                    "permalink_url}",
                    "&access_token=",token)
    } else {url <- new_url}
    get <- GET(url = url)
    char <- rawToChar(get$content)
    json <- fromJSON(char)
    if ("error" %in% names(json)) {
      if (grepl("expired", json$error$message)) {
        message("You must be logged in to your Facebook account and refresh/get the token!")
        message("1. Within the Graph API Explorer, select an Application.")
        message("2. Go to Get Token and select the Page Acces Token needed. ",
                "Note that there are navigation arrows if lots of accounts axists.")
        message("3. Copy and use the Acces Token created.")
        url <- paste0("https://developers.facebook.com/tools/explorer/1866795993626030/",
                      "?version=v3.3&classic=1")
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
  #ret[["json"]] <- json
  
  msg <- paste("Succesfully exported", total, "posts from", ret$account$name, "with")
  msg <- ifelse(!is.null(ret$comments),paste(msg, nrow(ret$comments),"comments,"),msg)
  msg <- ifelse(!is.null(ret$reactions),paste(msg, nrow(ret$reactions),"reactions,"),msg)
  msg <- ifelse(!is.null(ret$shares),paste(msg, nrow(ret$shares),"reactions,"),msg)
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
#' @family Scrapper
#' @family API
#' @family Facebook
#' @param token Character. Access token. Generate it for any of your
#' users or apps in \url{https://developers.facebook.com/tools/explorer}
#' @param post_id Character vector. Post id(s)
#' @export
fb_post <- function(token, post_id) {
  iters <- length(post_id)
  for (i in 1:iters) {
    if (i == 1) ret <- c()
    if (i == 1) nodata <- c()
    url <- paste0("https://graph.facebook.com/v3.0/", post_id[i],
                  "/comments?limit=50000","&access_token=", token)
    get <- GET(url = url)
    char <- rawToChar(get$content)
    json <- fromJSON(char)
    
    if ("error" %in% names(json)) {
      if (grepl("expired", json$error$message)) {
        message("You must be logged in to your Facebook account and refresh/get the token!")
        message("1. Within the Graph API Explorer, select an Application.")
        message("2. Go to Get Token and select the Page Acces Token needed. ",
                "Note that there are navigation arrows if lots of accounts axists.")
        message("3. Copy and use the Acces Token created.")
        url <- paste0("https://developers.facebook.com/tools/explorer/1866795993626030/",
                      "?version=v3.3&classic=1")
        browseURL(url)
      }
      error <- paste("API ERROR:", json$error$message)
      return(error)
    } else {
      if (length(json$data) != 0) {
        json$data$post_id <- post_id[i]
        json$data$created_time <- as.POSIXct(
          json$data$created_time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
        json$data$from <- json$data$from$name
        json$data <- as.data.frame(json$data, row.names = paste0(i,":",rownames(json$data)))
        ret <- rbind(ret, json$data) 
      } else {
        nodata <- c(nodata, post_id[i])
      }
    }
    if (iters > 1) statusbar(i, iters) 
  }  
  if (i == iters & length(nodata) > 0) 
    message(paste("NO DATA: no comments on", vector2text(nodata)))
  return(ret)
}


####################################################################
#' Facebook Ad Accounts
#' 
#' This returns all ad accounts for a FB Business Account FB.
#' For more information on Ad Insights' API, go to the 
#' \href{https://developers.facebook.com/docs/marketing-api/reference/ad-account/insights/}{original documentaion}
#' 
#' @family Scrapper
#' @family API
#' @family Facebook
#' @param token Character. This must be a valid access token with sufficient 
#' privileges. Visit the Facebook API Graph Explorer to acquire one
#' @param business_id Character. Business ID
#' @param type Character vector. Values: owned, client
#' @param api_version Character. Facebook API version
#' @export
fb_accounts <- function(token,
                        business_id = "904189322962915",
                        type = c("owned", "client"),
                        api_version = "v3.3"){
  
  # Starting URL
  url <- "https://graph.facebook.com/"
  output <- c()
  
  # Select which type of ad accounts
  type <- paste0(type, "_ad_accounts")
  
  for (i in 1:length(type)) {
    
    message(paste("Getting", type[i]))
    URL <- paste0(url, api_version, "/", business_id, "/", type[i])
    continue <- TRUE
    
    # Call insights
    import <- content(GET(
      URL,
      query = list(
        access_token = token,
        fields = "name,account_status,amount_spent,business_country_code",
        limit = "1000000"
      ),
      encode = "json"))
    
    # Show and return error
    if ("error" %in% names(import)) {
      message(paste("API ERROR:", import$error$message))
      # Very useful for Shiny apps
      invisible(return(import$error))
    }
    
    ret <- data.frame(bind_rows(import$data))
    
    # Check for no-data (user might think there was an error on GET request - warn him!)
    if (length(import$data) == 0) {
      message("There is no data for this query!")
      continue <- FALSE
    } 
    
    # Condition to detect the next page
    if (exists("next", import$paging) & continue) {
      # Checking from the originally returned list
      out <- fromJSON(import$paging$`next`)
      ret <- bind_rows(ret, data.frame(out$data))
      # Looping through subsequent returned pages
      while (exists("next", out$paging)) {
        out <- fromJSON(out$paging$`next`)
        ret <- bind_rows(ret, data.frame(out$data))
      }
    }
    ret$type <- type[i]
    output <- bind_rows(output, ret)
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
    arrange(desc(amount_spent))
    
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
#' @family Scrapper
#' @family API
#' @family Facebook
#' @param token Character. This must be a valid access token with sufficient 
#' privileges. Visit the Facebook API Graph Explorer to acquire one
#' @param which Character. This is the ad account, campaign, adset, 
#' or ad ID to be queried
#' @param start Character. The first full day to report, in the 
#' format "YYYY-MM-DD"
#' @param end Character. The last full day to report, in the 
#' format "YYYY-MM-DD"
#' @param api_version Character. Facebook API version
#' @export
fb_ads <- function(token,
                   which,
                   start = Sys.Date() - 31, 
                   end = Sys.Date(), 
                   api_version = "v3.3"){
  
  # Auxiliary process sub-lists function
  process_list <- function(l) {
    # islist <- lapply(l[[1]][[1]], is.list)
    # names(islist)[unlist(islist)]
    l <- as.list(l)
    if ("data" %in% names(l)) l <- l$data
    aux <- data.frame(l[[1]])
    if (length(l) > 1) {
      for (i in 2:length(l)) {
        lx <- data.frame(l[[i]])
        aux <- rbind_full(aux, lx)
      }
      if ("id" %in% colnames(aux))
        aux <- rename(aux, list_id = id)
    }
    return(aux)
  }
  
  # Starting URL
  url <- "https://graph.facebook.com/"
  URL <- paste0(url, api_version, "/", which, "/ads")
  
  # Call insights
  import <- content(GET(
    URL,
    query = list(
      access_token = token,
      time_range = paste0('{\"since\":\"',start,'\",\"until\":\"',end,'\"}'),
      fields = paste("id,name,created_time,status,adset_id,campaign_id,",
                     "adcreatives{id,body,image_url,thumbnail_url,object_type}")
    ),
    encode = "json"))
  
  # Show and return error
  if ("error" %in% names(import)) {
    message(paste("API ERROR:", import$error$message))
    # Very useful for Shiny apps
    invisible(return(import$error))
  }
  
  # Check for no-data (user might think there was an error on GET request - warn him!)
  if (length(import$data) == 0) {
    message("There is no data for this query!")
    invisible(return(NULL))
  } 
  
  ret <- data.frame(bind_rows(import$data))
  aux <- process_list(ret$adcreatives)
  if ("id" %in% colnames(aux)) aux <- rename(aux, list_id = id)
  ret <- cbind(select(ret, -adcreatives), aux)
  
  # Condition to detect the next page
  if (exists("next", import$paging)) {
    # Checking from the originally returned list
    out <- fromJSON(import$paging$`next`)
    newout <- cbind(select(out$data, -adcreatives), process_list(out$data$adcreatives))
    ret <- rbind_full(ret, data.frame(newout$data))
    # Looping through subsequent returned pages
    while (exists("next", out$paging)) {
      out <- fromJSON(out$paging$`next`)
      newout <- cbind(select(out$data, -adcreatives), process_list(out$data$adcreatives))
      ret <- rbind_full(ret, data.frame(newout))
    }
  }
  ret <- suppressMessages(type.convert(ret, numerals = "no.loss")) %>%
    mutate_at(vars(contains("date")), list(as.Date)) %>%
    mutate_at(vars(contains("id")), list(as.character)) %>%
    rename(adcreatives_id = list_id) %>%
    arrange(desc(created_time))
  return(ret)
}


####################################################################
#' Facebook Insights API
#' 
#' This returns all available FB insights per day including any given
#' breakdown to the specified report level, and place into a data frame.
#' For more information on Ad Insights' API, go to the 
#' \href{https://developers.facebook.com/docs/marketing-api/insights}{original documentaion}
#' 
#' This function was based on FBinsightsR.
#' 
#' @family Scrapper
#' @family API
#' @family Facebook
#' @param token Character. This must be a valid access token with sufficient 
#' privileges. Visit the Facebook API Graph Explorer to acquire one
#' @param which Character vector. This is the accounts, campaigns, adsets, 
#' or ads IDs to be queried
#' @param start Character. The first full day to report, in the 
#' format "YYYY-MM-DD"
#' @param end Character. The last full day to report, in the 
#' format "YYYY-MM-DD"
#' @param time_increment Character. Agrupación por meses (monthly), 
#' para todo junto (all_days) o un entero para cantidad de días [1-90].
#' Por defecto, traerá cada día por separado (i.e.=1)
#' @param report_level Character. One of ad, adset, campaign, or account
#' @param breakdowns Character Vector. One or more of breakdowns for 
#' segmentation results. Set to NA for no breakdowns
#' @param api_version Character. Facebook API version
#' @export
fb_insights <- function(token,
                        which,
                        start = Sys.Date() - 7, 
                        end = Sys.Date(), 
                        time_increment = "1",
                        report_level = "campaign",
                        breakdowns = NA,
                        api_version = "v3.3"){
  
  # library(jsonlite)
  # library(httr)
  # library(dplyr)
  
  # Starting URL
  url <- "https://graph.facebook.com/"
  output <- c()
  
  for (i in 1:length(which)) {
    aux <- which[i]
    URL <- paste0(url, api_version, "/", aux, "/insights")
    
    type <- c("ad","adset","campaign","account")
    if (!report_level %in% type) 
      stop(paste("Your report_level must be one of:", vector2text(type)))
    
    # Call insights
    import <- content(GET(
      URL,
      query = list(
        access_token = token,
        time_range = paste0('{\"since\":\"',start,'\",\"until\":\"',end,'\"}'),
        level = report_level,
        fields = paste(
          "campaign_name, campaign_id, objective, adset_id, adset_name, ad_id, ad_name,",
          "impressions, cpm, spend, reach, clicks, unique_clicks, ctr, cpc, unique_ctr,",
          "cost_per_unique_click"), 
        breakdowns = if (!is.na(breakdowns)) 
          vector2text(breakdowns, sep = ", ", quotes = FALSE) else NULL,
        time_increment = time_increment,
        limit = "1000000"
      ),
      encode = "json"))
    
    # Show and return error
    if ("error" %in% names(import)) {
      message(paste("API ERROR:", import$error$message))
      # Very useful for Shiny apps
      if (length(which) == 1) invisible(return(import$error))
      next
    }
    
    # Check for no-data (user might think there was an error on GET request - warn him!)
    if (length(import$data) == 0) {
      message(paste("There is no data for", aux))
      next
    } else if (length(which) > 1) message(paste("Data for", aux, "imported..."))
    
    ret <- data.frame(bind_rows(import$data))
    
    # Condition to detect the next page
    if (exists("next", import$paging)) {
      # Checking from the originally returned list
      out <- fromJSON(import$paging$`next`)
      ret <- bind_rows(ret, data.frame(out$data))
      # Looping through subsequent returned pages
      while (exists("next", out$paging)) {
        out <- fromJSON(out$paging$`next`)
        ret <- bind_rows(ret, data.frame(out$data))
      }
    }
    ret <- suppressMessages(type.convert(ret, numerals = "no.loss")) %>%
      mutate_at(vars(contains("date")), list(as.Date)) %>%
      mutate_at(vars(contains("id")), list(as.character)) %>%
      arrange(desc(date_start), desc(spend)) %>%
      mutate(id = aux)
    output <- rbind(output, ret)
  }
  return(ret)
}
