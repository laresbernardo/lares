####################################################################
#' Google Trends: Related Plot
#'
#' This function creates a plot with google trend's related topics 
#' and queries, and let the user compare different keywords.
#'
#' @param gtrend List. Result from gtrends(keyword, geo, time)
#' @param top Integer. Filter top n results only
#' @param title Character. Custom title for the plot
#' @param note Character. Add a note to the plot if needed
#' @param exclude Character vector. Which observations do you wish to exclude?
#' @export
trendsRelated <- function(gtrend, top = NA, title = NA, note = NA, exclude = c()) {
  
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
    colnames(gtrend_related)[2]<- "related"
    more2 <- length(unique(gtrend_related$keyword))
    
    related <- gtrend_related[gtrend_related$related=="top",] %>%
      left_join(gtrend_related[gtrend_related$related=="rising",] %>% 
                  select(value, subject),"value","keyword") %>%
      distinct() %>% rename(rank = subject.x, type = subject.y) %>%
      mutate(type_label = ifelse(grepl("+", type), as.character(type), "")) %>%
      mutate(rank = ifelse(rank == "<1", "0.1", as.character(rank))) %>%
      mutate(type = ifelse(is.na(type), "Steady", "Big change")) %>%
      group_by(keyword) %>% arrange(desc(as.numeric(rank))) %>%
      mutate(rank = ifelse(is.na(rank), max(rank, na.rm = T), rank)) %>%
      mutate(rank = ifelse(is.infinite(rank), 100, as.integer(rank))) %>%
      mutate(rank = as.integer(as.character(rank))) %>% 
      { if (more2 > 1) mutate(., value = paste0(value, " (", keyword, ")")) else .} %>%
      distinct()
    if (length(exclude) != 0) {
      related <- related %>% filter(!value %in% exclude) %>% 
        mutate(rank = 100*rank/max(rank)) 
    }
    
    n <- ifelse(is.na(top), round(50/more2, 0), top)
    if (!is.na(top) | more2 > 3) {
      related <- related %>% group_by(keyword) %>% arrange(desc(rank)) %>% slice(1:n) 
      if (is.na(top)) {
        message("Filtering top ", n, " values. Use top to over-write if needed") 
      }
    }
    
    if (length(exclude) != 0) {
      related <- related %>% filter(!value %in% exclude)
    }
    
    plot <- related %>% 
      ggplot(aes(x = reorder(value, rank), y = rank, 
                 fill = type, label = type_label)) +
      geom_col() + coord_flip() + theme_lares2() + 
      geom_text(hjust = -0.1, size = 2.8) +
      labs(y = "Relevance | % Increase", subtitle = ptitle, x = "", fill = "") +
      theme(legend.position = "bottom") +
      scale_y_continuous(labels = scales::comma, 
                         limits = c(0,110),
                         minor_breaks = seq(0, 100, 10), 
                         breaks = seq(0, 100, 20))
    if (more2 > 1) {
      plot <- plot + facet_grid(keyword ~ ., scales="free")
    }
    return(plot)
  }
  
  if(t) {
    rq1 <- fx(gtrend$related_topics)
  } else {
    message("No related topics found")
  }
  if(q) {
    rq2 <- fx(gtrend$related_queries)
    if (t == FALSE) {
      rq1 <- rq2
    }
  } else {
    message("No related queries found")
  }
  
  if (!is.na(title)) {
    rq1 <- rq1 + labs(title = title)
  }
  
  if (!is.na(note)) {
    rq1 <- rq1 + labs(caption = note)
  } else {
    rq1 <- rq1 + labs(caption = range)
  }
  
  if (t & q) {
    rq1 <- rq1 + guides(fill = FALSE) + labs(caption = "")
    rq2 <- rq2 + labs(title = range) +
      theme(plot.title = element_text(size = 9))
    return(grid.arrange(rq1, rq2, nrow = 1, ncol = 2))
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
#' @param gtrend List. Result from gtrends(keyword, geo, time)
#' @param title Character. Custom title for the plot
#' @export
trendsTime <- function(gtrend, title = NA) {
  
  int1 <- plot(gtrend) + theme_lares2() + 
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
    mutate(hits = ifelse(hits == "<1", "0.5", as.character(hits))) %>%
    mutate(hits = as.numeric(as.character(hits))) %>%
    group_by(keyword) %>%
    mutate(legend = paste0(keyword, " (", geo, ")")) %>%
    ggplot(aes(x=date, y=hits, fill=legend)) +
    geom_area(alpha=0.9) + theme_lares2() + guides(fill = FALSE) +
    labs(x = "", y = "Search hits", fill = "", subtitle = "Mixed hits scale")
  int3 <- gtrend$interest_over_time %>%
    mutate(hits = ifelse(hits == "<1", "0.5", as.character(hits))) %>%
    mutate(hits = as.numeric(as.character(hits))) %>%
    group_by(keyword) %>%
    mutate(hits = 100*hits/max(hits)) %>%
    mutate(legend = paste0(keyword, " (", geo, ")")) %>%
    ggplot(aes(x=date, y=hits, colour=legend)) +
    geom_line() + theme_minimal() + guides(colour = FALSE) +
    labs(x = "", y = "Search hits", 
         subtitle = "Normalized hit scale",
         caption = range) +
    geom_hline(yintercept = 100, alpha = 0.5)
  if (!is.na(title)) {
    int1 <- int1 + labs(title = title, subtitle = "Real hits scale")
  }
  return(grid.arrange(int1, int2, int3, nrow = 3, ncol = 1))
}
