####################################################################
#' Facebook API Graph's Posts
#' 
#' Connect to an API Graph's token of a given page and get posts, 
#' comments, shares, and reactions of n posts (with no limits).
#' 
#' @family Scrapper
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
