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
                      plot = TRUE) {
  if (is.vector(input)) {
    base <- "https://cranlogs.r-pkg.org/downloads/daily"
    dates <- sprintf("%s:%s", as.character(from), as.character(to))
    packages <- paste(unique(input), collapse = ",")
    url <- paste(base, dates, packages, sep = "/")
    scrap <- content(GET(url))
    cran.df <- bind_rows(scrap) %>%
      mutate(
        date = unlist(lapply(.data$downloads, function(x) x[[1]])),
        count = unlist(lapply(.data$downloads, function(x) x[[2]]))
      ) %>%
      select(.data$date, .data$count, .data$package) %>%
      mutate(date = as.Date(.data$date, origin = "1970-01-01"))
  }

  check_opts(colnames(input), c("date", "count", "package"), input_name = "column names")
  cran.df <- arrange(cran.df, desc(.data$date))

  package <- unique(cran.df$package)
  if (nrow(cran.df) >= length(package) * 2 & plot == TRUE) {
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
      summarise(MN = mean(.data$count, na.rm = TRUE)) %>%
      mutate(
        date = min(input$date),
        MN_label = formatNum(.data$MN, 2, abbr = TRUE)
      )
    plot <- ggplot(input, aes(x = .data$date)) +
      geom_line(aes(y = .data$count), size = 1.2, colour = lares_pal("simple")[1], alpha = 0.8) +
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
        vjust = -0.5, size = 2.5
      ) +
      scale_y_abbr() +
      expand_limits(y = 0)
    return(list(df = as_tibble(cran.df), plot = plot))
  }
  return(cran.df)
}
