####################################################################
#' Download and plot daily downloads of CRAN packages
#'
#' Download daily downloads stats from CRAN for any package, and plot.
#' It can also be used as an auxiliary function to plot
#' (\code{cranlogs::cran_downloads}) results.
#'
#' @param input Character vector with package names or data.frame product of
#' \code{cranlogs::cran_downloads}.
#' @param from,to Dates. Range of dates to fetch downloads metrics.
#' @param type Character. Any of: "daily" or "total".
#' @param plot Boolean. Create a plot?
#' @examples
#' \donttest{
#' cran_logs(c("lares", "dplyr"), from = "2021-05-31")
#' }
#' @return List with data.frame and plot if \code{plot=TRUE}.
#' @export
cran_logs <- function(input = "lares",
                      from = Sys.Date() - 31,
                      to = Sys.Date() - 1,
                      type = "daily",
                      plot = TRUE) {
  if (is.vector(input)) {
    check_opts(type, c("daily", "total"))
    base <- sprintf("https://cranlogs.r-pkg.org/downloads/%s", type)
    dates <- sprintf("%s:%s", as.character(from), as.character(to))
    packages <- paste(unique(input), collapse = ",")
    url <- paste(base, dates, packages, sep = "/")
    scrap <- content(GET(url), encoding = "UTF-8")
    if (!"downloads" %in% names(scrap[[1]])) {
      warning("Site currently unavailable")
      return(invisible(url))
    }
    cran.df <- bind_rows(scrap)
    if (type == "daily") {
      cran.df <- cran.df %>%
        mutate(
          date = unlist(lapply(.data$downloads, function(x) x[[1]])),
          count = unlist(lapply(.data$downloads, function(x) x[[2]]))
        ) %>%
        select(.data$date, .data$count, .data$package) %>%
        mutate(date = as.Date(.data$date, origin = "1970-01-01")) %>%
        arrange(desc(.data$date))
    }
  } else {
    check_opts(colnames(input), c("date", "count", "package"), input_name = "column names")
    cran.df <- as_tibble(input)
  }

  package <- unique(cran.df$package)
  if (nrow(cran.df) >= length(package) * 2 && plot && type == "daily") {
    input <- cran.df %>%
      arrange(.data$date) %>%
      group_by(.data$package) %>%
      mutate(
        cum = cumsum(.data$count),
        package = sprintf("%s\n(%s)", .data$package, formatNum(max(.data$cum), abbr = TRUE)),
        MN = mean(.data$count, na.rm = TRUE)
      )
    dMean <- input %>%
      group_by(.data$package) %>%
      summarise(
        MN = mean(.data$count, na.rm = TRUE),
        date = min(.data$date, na.rm = TRUE),
        MN_label = formatNum(.data$MN, 2, abbr = TRUE)
      )
    plot <- ggplot(input, aes(x = .data$date)) +
      geom_line(aes(y = .data$count), colour = lares_pal("simple")[1], alpha = 0.8) +
      theme_lares(legend = "top", pal = 2, panel_colour = "grey95") +
      labs(
        title = glued("CRAN: {x} downloads", x = v2t(package, and = "and")),
        subtitle = sprintf("(%s to %s)", min(input$date), max(input$date)),
        x = "Download Date", y = "Dailly Downloads",
        color = NULL
      ) +
      facet_grid(.data$package ~ ., scales = "free") +
      geom_line(aes(y = .data$MN), linetype = "dashed") +
      geom_text(
        data = dMean, aes(x = .data$date, y = .data$MN, label = .data$MN_label),
        vjust = -0.5, hjust = 1, size = 2.7
      ) +
      scale_y_abbr() +
      expand_limits(y = 0)
    return(list(df = as_tibble(cran.df), plot = plot))
  }
  return(cran.df)
}
