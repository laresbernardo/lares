####################################################################
#' Google Trends: Related Plot
#'
#' This function creates a plot with Google Trend's related topics
#' and queries, and let the user compare different keywords.
#'
#' @family Exploratory
#' @family Scrapper
#' @family Google Trends
#' @family Google
#' @param gtrend List. Result from \code{gtrendsR::gtrends(keyword, geo, time)}
#' @param top Integer. Filter top n results only.
#' @param title Character. Custom title for the plot.
#' @param note Character. Add a note to the plot if needed.
#' @param exclude Character vector. Which observations do you wish to exclude?
#' @return plot for Google Trend's results input \code{gtrend}.
#' @export
trendsRelated <- function(gtrend, top = NA, title = NA, note = NA, exclude = NULL) {
  start <- as.Date(min(gtrend$interest_over_time$date))
  end <- as.Date(max(gtrend$interest_over_time$date))
  range <- paste0(start, " - ", end, " (", as.integer(end - start), "d)")

  t <- !is.null(gtrend$related_topics) # Related topics
  q <- !is.null(gtrend$related_queries) # Related queries

  if (!t & !q) {
    stop("No related topics nor queries found!")
  }

  fx <- function(gtrend_related) {
    which <- colnames(gtrend_related)[2]
    ptitle <- ifelse(which == "related_topics", "Related topics", "Related queries")
    colnames(gtrend_related)[2] <- "related"
    more2 <- length(unique(gtrend_related$keyword))

    related <- gtrend_related[gtrend_related$related == "top", ] %>%
      left_join(gtrend_related[gtrend_related$related == "rising", ] %>%
        select(.data$value, .data$subject), "value", "keyword") %>%
      distinct() %>%
      rename(
        rank = .data$subject.x,
        type = .data$subject.y
      ) %>%
      mutate(type_label = ifelse(grepl("+", .data$type), as.character(.data$type), "")) %>%
      mutate(rank = ifelse(.data$rank == "<1", "0.1", as.character(.data$rank))) %>%
      mutate(type = ifelse(is.na(.data$type), "Steady", "Big change")) %>%
      group_by(.data$keyword) %>%
      arrange(desc(as.numeric(.data$rank))) %>%
      mutate(rank = ifelse(is.na(.data$rank), max(.data$rank, na.rm = T), .data$rank)) %>%
      mutate(rank = ifelse(is.infinite(.data$rank), 100, as.integer(.data$rank))) %>%
      mutate(rank = as.integer(as.character(.data$rank))) %>%
      {
        if (more2 > 1) mutate(., value = paste0(.data$value, " (", .data$keyword, ")")) else .
      } %>%
      distinct()
    if (length(exclude) != 0) {
      related <- related %>%
        filter(!.data$value %in% exclude) %>%
        mutate(rank = 100 * .data$rank / max(.data$rank))
    }

    n <- ifelse(is.na(top), round(50 / more2, 0), top)
    if (!is.na(top) | more2 > 3) {
      related <- related %>%
        group_by(.data$keyword) %>%
        arrange(desc(.data$rank)) %>%
        slice(1:n)
      if (is.na(top)) {
        message("Filtering top ", n, " values. Use top to over-write if needed")
      }
    }

    if (length(exclude) != 0) {
      related <- related %>% filter(!.data$value %in% exclude)
    }

    plot <- related %>%
      ggplot(aes(
        x = reorder(.data$value, .data$rank), y = .data$rank,
        fill = .data$type, label = .data$type_label
      )) +
      geom_col() +
      coord_flip() +
      theme_lares() +
      geom_text(hjust = -0.1, size = 2.8) +
      labs(y = "Relevance | % Increase", subtitle = ptitle, x = "", fill = "") +
      theme(legend.position = "bottom") +
      scale_y_comma(
        limits = c(0, 110),
        minor_breaks = seq(0, 100, 10),
        breaks = seq(0, 100, 20)
      )
    if (more2 > 1) plot <- plot + facet_grid(.data$keyword ~ ., scales = "free")
    return(plot)
  }

  if (t) {
    rq1 <- fx(gtrend$related_topics)
  } else {
    message("No related topics found")
  }
  if (q) {
    rq2 <- fx(gtrend$related_queries)
    if (t == FALSE) {
      rq1 <- rq2
    }
  } else {
    message("No related queries found")
  }

  if (!is.na(title)) rq1 <- rq1 + labs(title = title)

  if (!is.na(note)) {
    rq1 <- rq1 + labs(caption = note)
  } else {
    rq1 <- rq1 + labs(caption = range)
  }

  if (t & q) {
    rq1 <- rq1 + guides(fill = "none") + labs(caption = NULL)
    rq2 <- rq2 + labs(title = range) +
      theme(plot.title = element_text(size = 9))
    p <- rq1 + rq2 + plot_layout(nrow = 1, ncol = 2)
    return(p)
  }

  if (t | q) {
    return(rq1)
  }
}

####################################################################
#' Google Trends: Timelines Plot
#'
#' This function creates a plot with google trend's data on timelines
#' and let the user compare different keywords.
#'
#' @family Google Trends
#' @family Google
#' @inheritParams trendsRelated
#' @return plot for Google Trend's results input \code{gtrend}
#' @export
trendsTime <- function(gtrend, title = NA) {
  try_require("gtrendsR")
  int1 <- plot(gtrend) + theme_lares() +
    labs(x = "", colour = "") + ylim(0, 100) +
    theme(legend.position = "top") +
    geom_hline(yintercept = 100, alpha = 0.5)

  if (length(unique(gtrend$interest_over_time$keyword)) == 1) {
    return(int1)
  }

  start <- as.Date(min(gtrend$interest_over_time$date))
  end <- as.Date(max(gtrend$interest_over_time$date))
  range <- paste0(start, " - ", end, " (", as.integer(end - start), "d)")

  int2 <- gtrend$interest_over_time %>%
    mutate(hits = ifelse(.data$hits == "<1", "0.5", as.character(.data$hits))) %>%
    mutate(hits = as.numeric(as.character(.data$hits))) %>%
    group_by(.data$keyword) %>%
    mutate(legend = paste0(.data$keyword, " (", .data$geo, ")")) %>%
    ggplot(aes(x = .data$date, y = .data$hits, fill = .data$legend)) +
    geom_area(alpha = 0.9) +
    theme_lares() +
    guides(fill = "none") +
    labs(x = "", y = "Search hits", fill = "", subtitle = "Mixed hits scale")

  int3 <- gtrend$interest_over_time %>%
    mutate(hits = ifelse(.data$hits == "<1", "0.5", as.character(.data$hits))) %>%
    mutate(hits = as.numeric(as.character(.data$hits))) %>%
    group_by(.data$keyword) %>%
    mutate(hits = 100 * .data$hits / max(.data$hits)) %>%
    mutate(legend = paste0(.data$keyword, " (", .data$geo, ")")) %>%
    ggplot(aes(x = .data$date, y = .data$hits, colour = .data$legend)) +
    geom_line() +
    theme_minimal() +
    guides(colour = "none") +
    labs(
      x = "", y = "Search hits",
      subtitle = "Normalized hit scale",
      caption = range
    ) +
    geom_hline(yintercept = 100, alpha = 0.5) +
    theme_lares()

  if (!is.na(title)) int1 <- int1 + labs(title = title, subtitle = "Real hits scale")

  p <- int1 + int2 + int3 + plot_layout(nrow = 3, ncol = 1)
  return(p)
}
