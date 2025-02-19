####################################################################
#' Build a Personal Investing Portfolio Report
#'
#' \code{stocks_file()} lets the user download his personal Excel with his
#' Portfolio's data, locally or from Dropbox.
#'
#' @family Investment
#' @family Credentials
#' @inheritParams cache_write
#' @param file Character. Import an Excel file, local or from URL.
#' @param creds Character. Dropbox's credentials (see \code{get_creds()})
#' @param auto Boolean. Automatically use my local personal file? You might want
#' to set in into your .Renviron \code{LARES_PORTFOLIO=~/dir/to/your/file.xlsx} so you
#' can leave all other parameters as \code{NA} and use it every time.
#' @param sheets Character Vector. Names of each sheet containing Portfolio summary,
#' Cash, and Transactions information. Please, keep the order of these tabs.
#' @param keep_old Boolean. Include sold tickers even though not currently in portfolio?
#' @param cache Boolean. Use daily cache if available?
#' @param quiet Boolean. Keep quiet? If not, informative messages will be printed.
#' @return List with portfolio, transactions, and cash data.frames.
#' @examples
#' \dontrun{
#' # Load lares dummy portfolio XLSX
#' file <- system.file("inst/docs", "dummyPortfolio.xlsx", package = "lares")
#' df <- stocks_file(
#'   file = file,
#'   sheets = c("Portafolio", "Fondos", "Transacciones"),
#'   keep_old = FALSE
#' )
#' }
#' @export
#' @rdname stocks_report
stocks_file <- function(file = NA,
                        creds = NA,
                        auto = TRUE,
                        sheets = c("Portafolio", "Fondos", "Transacciones"),
                        keep_old = TRUE,
                        cache = TRUE,
                        quiet = FALSE,
                        ...) {
  cache_file <- c(as.character(Sys.Date()), "stocks_file")
  if (cache_exists(cache_file) && cache) {
    results <- cache_read(cache_file, quiet = quiet)
    return(results)
  }
  processFile <- function(file, sheets = NULL, keep_old = TRUE) {
    mylist <- lapply(sheets, function(x) {
      as_tibble(read.xlsx(
        file,
        sheet = x,
        skipEmptyRows = TRUE,
        detectDates = TRUE
      ))
    })
    if (length(mylist) == 3) {
      names(mylist) <- c("port", "cash", "trans")
      mylist$port$StartDate <- try(as.Date(mylist$port$StartDate, origin = "1889-12-31", ...))
      mylist$trans$Date <- try(as.Date(mylist$trans$Date, origin = "1889-12-31", ...))
      mylist$cash$Date <- try(as.Date(mylist$cash$Date, origin = "1889-12-31", ...))
      if ("Value" %in% colnames(mylist$trans)) {
        mylist$trans <- rename(mylist$trans, Each = .data$Value, Invested = .data$Amount)
      }
      if (!keep_old) mylist$port <- try(mylist$port[mylist$port$Stocks != 0, ])
      mylist <- list("portfolio" = mylist$port, "transactions" = mylist$trans, "cash" = mylist$cash)
    }
    if (length(mylist) == 1) {
      mylist <- mylist[[1]]
    }
    return(mylist)
  }

  # FOR PERSONAL USE
  local <- Sys.info()
  if (auto && Sys.getenv("LARES_PORTFOLIO") != "") {
    if (!quiet) message("Using BL's local file...")
    local <- Sys.getenv("LARES_PORTFOLIO")
    results <- processFile(local, sheets, keep_old)
  } else {
    # FOR EVERYONE'S USE
    if (!is.na(file)) {
      if (file.exists(file) || is_url(file)) {
        results <- processFile(file, sheets, keep_old)
      } else {
        stop("Error: that file doesn't exist or it's not in your working directory!")
      }
    } else {
      # FOR DROPBOX'S USE
      file <- paste0(tempdir(), "/Portfolio LC.xlsx")
      db_download(
        query = "Portfolio LC.xlsx",
        local_path = file,
        xlsx = FALSE, # Do not import as Excel, just download
        token_dir = creds
      )
      results <- processFile(file, sheets, keep_old = keep_old)
    }
  }
  if (length(results) == 3) {
    attr(results$portfolio, "type") <- "stocks_file_portfolio"
    attr(results$transactions, "type") <- "stocks_file_transactions"
    attr(results$cash, "type") <- "stocks_file_cash"
  }
  attr(results, "type") <- "stocks_file"
  cache_write(results, cache_file, quiet = TRUE, ...)
  return(results)
}

####################################################################
#' Download Stocks Historical and Current Values
#'
#' \code{stocks_quote()} lets the user download stocks live data.
#'
#' @family Investment
#' @family Scrapper
#' @inheritParams cache_write
#' @param symbols Character Vector. List of symbols to download historical data.
#' @return data.frame with Symbol, Type of stock, Quote time, current value,
#' Daily Change, Market, and Symbol Name.
#' @examples
#' \donttest{
#' # Multiple quotes at the same time
#' stocks_quote(c("VTI", "VOO", "TSLA"))
#' }
#' @export
#' @rdname stocks_hist
stocks_quote <- function(symbols, ...) {
  ret <- noret <- NULL
  try_require("quantmod")
  for (i in seq_along(symbols)) {
    z <- try(data.frame(getQuote(symbols[i], ...)))
    if ("try-error" %in% class(z)) {
      return(invisible(ret))
    }
    if (length(z) > 0) {
      z <- data.frame(Symbol = symbols[i], z)
      ret <- bind_rows(ret, z)
    } else {
      noret <- rbind(noret, symbols[i])
    }
  }
  if (length(noret) > 0) {
    message(paste("No results for", vector2text(noret)))
  }
  if (length(ret) > 0) {
    colnames(ret) <- c(
      "Symbol", "QuoteTime", "Value", "DailyChange",
      "DailyChangeP", "Open", "High", "Low", "Volume"
    )
    ret <- as_tibble(ret) %>%
      mutate(QuoteTime = as.POSIXct(
        .data$QuoteTime,
        origin = "1970-01-01 00:00:00"
      ))
    row.names(ret) <- NULL
  }
  ret
}

####################################################################
#' Download Stocks Historical Data
#'
#' \code{stocks_hist()} lets the user download stocks historical data.
#'
#' @inheritParams stocks_file
#' @inheritParams cache_write
#' @param from,to Date. Dates for range. If not set, 1 year will be downloaded.
#' Do use more than 4 days or will be over-written.
#' @param today Boolean. Do you wish to add today's live quote? This will happen
#' only if to value is the same as today's date
#' @param tax Numeric. How much [0-99] of your dividends are gone with taxes?
#' @param parg Boolean. Personal argument. Used to personalize stuff, in this
#' case, taxes changed from A to B in given date (hard-coded)
#' @examples
#' \dontrun{
#' df <- stocks_hist(symbols = c("VTI", "META", "FIW"), from = Sys.Date() - 180)
#' print(head(df))
#' plot(df)
#' }
#' @rdname stocks_hist
#' @export
stocks_hist <- function(symbols = c("VTI", "META"),
                        from = Sys.Date() - 365,
                        to = Sys.Date(),
                        today = TRUE,
                        tax = 15,
                        parg = FALSE,
                        cache = TRUE,
                        quiet = FALSE,
                        ...) {
  cache_file <- c(as.character(Sys.Date()), "stocks_hist")
  if (cache_exists(cache_file) && cache) {
    results <- cache_read(cache_file, quiet = quiet)
    return(results)
  }

  try_require("quantmod")
  data <- divs <- NULL

  if (!any(is.na(symbols))) {
    if (length(from) != length(symbols)) {
      from <- rep(from[1], length(symbols))
    }
    for (i in seq_along(symbols)) {
      # Daily quotes (except today)
      symbol <- as.character(symbols[i])
      if (as.Date(from[i]) > (Sys.Date() - 4)) from[i] <- Sys.Date() - 4
      start_date <- as.character(from[i])
      values <- try(suppressWarnings(data.frame(getSymbols(
        symbol,
        env = NULL, from = start_date, to = to, src = "yahoo"
      ))))
      if ("try-error" %in% class(values)) next
      values <- cbind(row.names(values), as.character(symbol), values)
      colnames(values) <- c("Date", "Symbol", "Open", "High", "Low", "Close", "Volume", "Adjusted")
      values <- mutate(values, Adjusted = rowMeans(select(values, .data$High, .data$Close), na.rm = TRUE))
      row.names(values) <- NULL
      # if (as.Date(from[i]) > (Sys.Date() - 4))
      #   values <- head(values, 1)

      # Add right now's data
      if (today && to == Sys.Date()) {
        now <- stocks_quote(symbol, ...)
        # Append to historical data / replace most recent
        if (length(now) > 0) {
          now <- data.frame(
            Date = as.character(as.Date(now$QuoteTime)),
            Symbol = symbol,
            Open = now$Value, High = now$Value,
            Low = now$Value, Close = now$Value,
            Volume = NA, Adjusted = now$Value
          )
          values <- filter(values, as.Date(.data$Date) != as.Date(now$Date))
          values <- rbind(values, now)
        }
      }
      # Append to other symbols' data
      data <- rbind(data, values)

      # Dividends (with Taxes deduction)
      d <- suppressWarnings(getDividends(
        as.character(symbol),
        from = start_date, split.adjust = FALSE
      ))
      if (isTRUE(nrow(d) > 0)) {
        div <- data.frame(
          Symbol = rep(symbol, nrow(d)),
          Date = ymd(row.names(data.frame(d))),
          Div = as.vector(d),
          DivTaxP = tax / 100
        )
        if (parg) {
          div <- mutate(div, DivTaxP = ifelse(
            .data$Date < as.Date("2020-03-03"),
            (30 / 100), # Interactive Brokers (Colombia)
            (15 / 100) # Schwab (Argentina/Veneezuela)
          ))
        }
        div <- mutate(div,
          DivReal = as.vector(d) * (1 - .data$DivTaxP),
          Symbol = as.character(.data$Symbol)
        )
        divs <- rbind(divs, div)
      }

      if (!quiet && length(symbols) > 1) {
        info <- paste(symbol, "since", start_date, "   ")
        statusbar(i, length(symbols), info)
      }
    }
  } else {
    message("You need to define which stocks to bring. Use the 'symbols=' parameter.")
  }

  results <- data %>%
    select(.data$Date, .data$Symbol, .data$Close) %>%
    rename(Value = .data$Close) %>%
    filter(.data$Value > 0) %>%
    mutate(
      Date = as.Date(.data$Date),
      Symbol = as.character(.data$Symbol)
    )
  if (length(divs) > 0) {
    results <- left_join(results, divs, by = c("Date", "Symbol"))
  }
  results <- results %>%
    replace(is.na(.), 0) %>%
    arrange(desc(.data$Date))

  results <- as_tibble(results)
  attr(results, "type") <- "stocks_hist"
  class(results) <- c("stocks_hist", class(results))
  cache_write(results, cache_file, quiet = TRUE)
  return(results)
}

#' @rdname stocks_hist
#' @aliases stocks_hist
#' @param x stocks_hist object
#' @param type Integer. Select type of plot.
#' @export
plot.stocks_hist <- function(x, type = 1, ...) {
  if (!inherits(x, "stocks_hist")) stop("Object must be class stocks_hist")
  if (type == 1) {
    group_by(x, .data$Symbol) %>%
      mutate(
        RelValue = (.data$Value - last(.data$Value)),
        Growth = .data$RelValue / last(.data$Value),
        Total = first(.data$Growth)
      ) %>%
      ungroup() %>%
      arrange(desc(.data$Total)) %>%
      mutate(
        Symbol = sprintf("%s (%s%%)", .data$Symbol, round(100 * .data$Total, 2)),
        Symbol = factor(.data$Symbol, unique(.data$Symbol))
      ) %>%
      ggplot(aes(x = .data$Date, y = .data$Growth, colour = .data$Symbol)) +
      geom_line(alpha = 0.8) +
      theme_lares(pal = 2, legend = "left") +
      scale_y_percent(position = "right") +
      labs(
        title = "Relative Growth since Origin", colour = NULL,
        subtitle = sprintf("%s - %s", min(x$Date), max(x$Date)),
        x = NULL, y = "Value Growth [%]"
      )
  }
}


####################################################################
#' Daily Stocks Dataframe
#'
#' \code{daily_stocks()} creates a dataframe with all relevant metrics and values,
#' for each ticker or symbol, for every day since inception.
#'
#' @inheritParams cache_write
#' @param hist Dataframe. Result from \code{stocks_hist()}
#' @param trans Dataframe. Result from \code{stocks_file()$transactions}
#' @param tickers Dataframe. Result from \code{stocks_file()$portfolio}
#' @param window Character. Choose any of: "1W", "1M", "6M", "1Y", "YTD", "5Y", "MAX"
#' @return data.frame. Processed at date and symbol level.
#' @export
#' @rdname stocks_report
daily_stocks <- function(hist, trans, tickers = NA, window = "MAX", ...) {
  check_attr(hist, check = "stocks_hist")
  check_attr(trans, check = "stocks_file_transactions")
  if (!is.na(tickers)[1]) {
    check_attr(tickers, check = "stocks_file_portfolio")
  }
  check_opts(window, c("1W", "1M", "6M", "1Y", "YTD", "5Y", "MAX"))

  with_div <- "DivReal" %in% colnames(hist)

  daily <- expand.grid(Date = unique(hist$Date), Symbol = unique(hist$Symbol)) %>%
    left_join(hist, c("Date", "Symbol")) %>%
    left_join(trans, by = c("Date", "Symbol")) %>%
    select(-.data$CODE, -.data$Description) %>%
    mutate(Date = as.Date(.data$Date)) %>%
    arrange(desc(.data$Date), .data$Symbol) %>%
    group_by(.data$Symbol) %>%
    tidyr::fill(.data$Value, .direction = "up") %>%
    replace(is.na(.), 0) %>%
    arrange(.data$Date) %>%
    {
      if (!with_div) mutate(., DivReal = 0) else .
    } %>%
    group_by(.data$Symbol) %>%
    mutate(
      CumQuant = cumsum(.data$Quant),
      CumInvested = cumsum(.data$Invested),
      CumCost = cumsum(.data$Cost),
      CumValue = .data$CumQuant * .data$Value,
      CumROI = 100 * (.data$CumValue / .data$CumInvested - 1),
      DivPreTax = .data$Div * .data$CumQuant,
      Dividend = .data$DivReal * .data$CumQuant,
      DifUSD = .data$CumValue - .data$Invested - lag(.data$CumValue),
      CumDivPreTax = cumsum(.data$DivPreTax),
      CumDividend = cumsum(.data$Dividend)
    ) %>%
    select(
      .data$Date, .data$Symbol, .data$Value, .data$Quant, .data$Each,
      .data$Invested, .data$Cost, .data$Dividend,
      .data$CumValue, .data$CumInvested, .data$CumROI, .data$CumQuant,
      .data$DifUSD, .data$CumDivPreTax, .data$CumDividend, .data$CumCost
    ) %>%
    group_by(.data$Date, .data$Symbol) %>%
    slice(1) %>%
    arrange(desc(.data$Date), desc(.data$CumValue)) %>%
    ungroup()

  if ("Type" %in% colnames(tickers)) {
    tickers_structure <- c("Symbol", "Type")
    if (!all(tickers_structure %in% colnames(tickers))) {
      stop(paste(
        "The structure of the 'tickers' table should be:",
        paste(shQuote(tickers_structure), collapse = ", ")
      ))
    }
    daily <- left_join(daily, select(tickers, .data$Symbol, .data$Type), "Symbol")
  } else {
    daily$Type <- "No type"
  }

  levs <- unique(daily$Symbol[daily$Date == max(daily$Date)])
  daily <- daily %>%
    mutate(Symbol = factor(.data$Symbol, levels = levs)) %>%
    .filter_window(window) %>%
    group_by(.data$Date) %>%
    mutate(
      wt_value = weighted_value(.data$Value, n = .data$Quant),
      wt_total = .data$wt_value * .data$Quant,
      wt = signif(100 * .data$wt_total / sum(.data$wt_total), 4)
    ) %>%
    ungroup()

  attr(daily, "type") <- "daily_stocks"
  return(daily)
}

####################################################################
#' Daily Portfolio Dataframe
#'
#' \code{daily_portfolio()} creates a data.frame with all relevant
#' metrics and values, for the overall portfolio, for every day
#' since inception.
#'
#' @inheritParams daily_stocks
#' @param cash Dataframe. Result from \code{stocks_file()$cash}
#' @param cash_fix Numeric. If, for some reason, you need to fix your
#' cash amount for all reports, set the amount here
#' @return data.frame. Processed at date and portfolio level.
#' @export
#' @rdname stocks_report
daily_portfolio <- function(hist, trans, cash, cash_fix = 0, window = "MAX") {
  check_attr(hist, check = "stocks_hist")
  check_attr(trans, check = "stocks_file_transactions")
  check_attr(cash, check = "stocks_file_cash")

  temp <- expand.grid(
    Date = unique(hist$Date),
    Symbol = unique(hist$Symbol)
  ) %>%
    left_join(daily_stocks(hist, trans), c("Date", "Symbol")) %>%
    mutate(Date = as.Date(.data$Date)) %>%
    arrange(desc(.data$Date), .data$Symbol) %>%
    filter(!is.na(.data$CumQuant)) %>%
    group_by(.data$Symbol) %>%
    tidyr::fill(.data$Value, .data$CumInvested, .data$CumValue,
      .data$CumDividend, .data$CumCost,
      .direction = "up"
    ) %>%
    group_by(.data$Date) %>%
    summarise_if(is.numeric, list(~ sum(., na.rm = TRUE))) %>%
    arrange(.data$Date) %>%
    mutate(
      ROI = 100 * (.data$CumValue / .data$CumInvested - 1),
      DifUSD = .data$CumValue - .data$Invested - lag(.data$CumValue)
    ) %>%
    select(
      .data$Date, .data$CumInvested, .data$CumValue, .data$DifUSD,
      .data$ROI, .data$Invested, .data$CumCost, .data$CumDividend, .data$Dividend
    )

  days <- tibble(Date = as.Date(min(hist$Date):Sys.Date(), origin = "1970-01-01")) %>%
    left_join(temp, "Date") %>%
    tidyr::fill(.data$ROI, .data$CumInvested, .data$CumValue,
      .data$CumDividend, .data$CumCost,
      .direction = "up"
    ) %>%
    mutate(Date = as.character(.data$Date)) %>%
    left_join(mutate(cash, Date = as.character(.data$Date)) %>% select(.data$Date, .data$Cash), "Date") %>%
    mutate(Date = as.Date(.data$Date)) %>%
    replace(is.na(.), 0) %>%
    arrange(.data$Date) %>%
    mutate(
      Cash = as.numeric(ifelse(is.na(.data$Cash), 0, .data$Cash)),
      CumCash = round(
        cumsum(.data$Cash) + cumsum(.data$Dividend) + cash_fix -
          .data$CumInvested - .data$CumCost
      )
    ) %>%
    filter(.data$CumInvested != 0) %>%
    arrange(desc(.data$Date)) %>%
    ungroup()

  # One row per day only
  ret <- days %>%
    group_by(
      .data$Date, .data$CumInvested, .data$CumValue,
      .data$CumCost, .data$CumDividend, .data$ROI
    ) %>%
    summarise_if(is.numeric, sum) %>%
    arrange(desc(.data$Date)) %>%
    ungroup() %>%
    mutate(Portfolio = .data$CumCash + .data$CumValue) %>%
    .filter_window(window) %>%
    ungroup()

  attr(ret, "type") <- "daily_portfolio"
  return(ret)
}


####################################################################
#' Calculate weighted stock values using FIFO/LIFO
#'
#' @inheritParams cache_write
#' @param value Numeric vector. Representing the values of the stock.
#' @param n Numeric vector. Representing the volume of the operation.
#' Positive for 'Buy' and negative for 'Sale'.
#' @param technique Character. Pick any of FIFO or LIFO, or NULL to skip.
#' @param n_stocks Integer. Specify the number of stocks to consider. By
#' default will sum positive values of \code{n}.
#' @param buy_only Boolean. Consider only buy (positive) values?
#' @param type Integer. 1 for returning the value, 2 for returning the
#' data.frame with the details ("df" attribute)
#' @return The calculated weighted mean value.
#' @examples
#' values <- c(10, 20, 30, 40, 50)
#' weights <- c(2, 3, -4, 5, 6)
#' mean(values)
#' weighted_value(values)
#' weighted.mean(values, weights)
#' weighted_value(values, weights, buy_only = FALSE)
#' # Using FIFO and LIFO
#' weighted_value(values, weights, "FIFO")
#' weighted_value(values, weights, "LIFO", n_stocks = 8)
#' @export
weighted_value <- function(value,
                           n = rep(1, length(value)),
                           technique = NULL,
                           n_stocks = NULL,
                           buy_only = TRUE,
                           type = 1,
                           ...) {
  check_opts(technique, c("FIFO", "LIFO"))
  stopifnot(length(value) == length(n))
  df <- data.frame(value = value, n = n, total = n)
  if (buy_only) df <- df[df$n > 0, ]
  if (is.null(n_stocks)) n_stocks <- sum(n, na.rm = TRUE)
  if (nrow(df) > 0) {
    df$id <- seq_len(nrow(df))
    if ("LIFO" %in% technique) {
      df <- df %>%
        arrange(desc(.data$id)) %>%
        mutate(cum = cumsum(.data$n), n_stocks = n_stocks) %>%
        rowwise() %>%
        mutate(
          total = min(.data$n, .data$n_stocks - .data$cum + .data$n),
          total = ifelse(.data$total < 0, 0, .data$total)
        ) %>%
        arrange(.data$id) %>%
        data.frame()
    } else if ("FIFO" %in% technique) {
      df <- df %>%
        mutate(cum = cumsum(.data$n), n_stocks = n_stocks) %>%
        rowwise() %>%
        mutate(
          total = min(.data$n, .data$n_stocks - .data$cum + .data$n),
          total = ifelse(.data$total < 0, 0, .data$total)
        ) %>%
        data.frame()
    }
    ret <- sum(df$value * df$total, na.rm = TRUE) / sum(df$total, na.rm = TRUE)
    attr(ret, "df") <- select(df, -any_of(c("id", "cum")))
    if (type == 2) {
      return(attr(ret, "df"))
    } else {
      return(ret)
    }
  } else {
    return(0)
  }
}

################# PLOTTING FUNCTIONS #################

####################################################################
#' Investing Portfolio Reporting Plots
#'
#' \code{splot_summary()} plots a summary for the whole portfolio, showing
#' how much have you invested, how much has each ticker changed, etc.
#'
#' @family Investment
#' @param p Dataframe. Result from \code{daily_portfolio()}
#' @param s Dataframe. Result from \code{daily_stocks()}
#' @param save Boolean. Save plot into a local file?
#' @return ggplot object
#' @export
#' @rdname stocks_plots
splot_summary <- function(p, s, save = FALSE) {
  check_attr(p, check = "daily_portfolio")
  check_attr(s, check = "daily_stocks")

  today <- filter(p, .data$Date == max(p$Date))
  summary <- rbind(
    paste0("Portfolio: $", formatNum(today$Portfolio, 0), " | ", today$Date),
    paste0("Stocks: $", formatNum(today$CumValue, 0), " & Cash: $", formatNum(today$CumCash, 0)),
    paste0(
      "ROI: ", formatNum(today$ROI, 2), "% ($",
      formatNum(today$CumValue - today$CumInvested, 0), ")"
    ),
    paste0(
      "Dividends: $", formatNum(today$CumDividend, 0), " | Expenses: $",
      formatNum(today$CumCost, 0)
    )
  )

  today <- s %>%
    group_by(.data$Symbol) %>%
    filter(.data$Date == max(p$Date)) %>%
    filter(.data$CumQuant > 0) %>%
    ungroup()
  tops <- max(rbind(today$CumInvested, today$CumValue))
  plot <- today %>%
    mutate(
      shapeflag = ifelse(.data$CumROI < 0, 25, 24),
      box = -tops / 5.5,
      Symbol = factor(
        paste0(
          .data$Symbol, " (",
          formatNum(100 * .data$CumValue / sum(.data$CumValue)), "%)"
        ),
        levels = paste0(.data$Symbol, " (", formatNum(100 * .data$CumValue / sum(.data$CumValue)), "%)")
      ),
      DifUSD = .data$CumValue - .data$CumInvested,
      DifPer = 100 * .data$DifUSD / .data$CumInvested
    ) %>%
    ggplot(aes(x = reorder(.data$Symbol, .data$CumInvested))) +
    geom_hline(yintercept = 0, colour = "black") +
    geom_col(aes(y = .data$CumInvested, fill = .data$Symbol, group = 1)) +
    geom_col(aes(y = .data$CumInvested + .data$DifUSD, fill = .data$Symbol), alpha = 0.5) +
    geom_col(aes(y = .data$box), fill = "grey", alpha = 0.5) +
    geom_point(aes(y = .data$CumInvested + .data$DifUSD, shape = .data$shapeflag), colour = "black") +
    scale_shape_identity() +
    geom_text(
      aes(
        label = paste0("$", formatNum(.data$DifUSD, 0)),
        y = .data$CumInvested + .data$DifUSD
      ),
      size = 3, hjust = -.2, vjust = -0.2
    ) +
    geom_text(
      aes(
        label = paste0(round(.data$DifPer, 1), "%"),
        y = .data$CumInvested + .data$DifUSD
      ),
      size = 2.9, hjust = -.2, vjust = 1.2
    ) +
    geom_text(aes(label = paste0("$", formatNum(.data$CumValue, 0)), y = .data$box),
      size = 3, hjust = -.1, vjust = -0.2
    ) +
    geom_text(aes(
      label = paste0(.data$CumQuant, " @$", formatNum(.data$CumValue / .data$CumQuant, 1)),
      y = .data$box
    ), size = 2, hjust = -.1, vjust = 1.5) +
    geom_text(
      aes(
        label = paste0("$", formatNum(.data$CumInvested, 1)),
        y = 0, colour = .data$Symbol
      ),
      size = 2, hjust = 0, vjust = -0.2
    ) +
    geom_text(
      aes(
        label = paste0("@$", formatNum(.data$CumInvested / .data$CumQuant, 1)),
        y = 0, x = .data$Symbol, colour = .data$Symbol
      ),
      size = 2, hjust = 0, vjust = 1.5
    ) +
    annotate("label",
      x = length(unique(today$CumQuant)) * 0.25, y = tops * 0.6,
      label = vector2text(summary, "\n", quotes = F), size = 3.5, hjust = 0, alpha = 0.55
    ) +
    scale_y_comma(limits = c(NA, tops * 1.12), expand = c(0, 0)) +
    labs(y = NULL, x = NULL, title = "Stocks Distribution and Growth") +
    guides(fill = "none", colour = "none") +
    coord_flip() +
    theme_lares(pal = 1)

  if (save) {
    plot <- plot +
      ggsave("portf_stocks_change.png", width = 8, height = 8, dpi = 300)
  }

  return(plot)
}


####################################################################
#' Portfolio Plots: Daily Change
#'
#' \code{splot_change()} plots each stock's change through history, since
#' inception, with weighted attributions or absolute values.
#'
#' @inheritParams splot_summary
#' @inheritParams stocks_file
#' @param rel Boolean. Relative delta values (weighted with portfolio)?
#' If not, absolute monetary delta values.
#' @param group Boolean. Group stocks by stocks type?
#' @param n_days Integer. How many days back you want to see?
#' sold entirely?
#' @export
#' @rdname stocks_plots
splot_change <- function(p, s,
                         rel = TRUE,
                         group = FALSE,
                         n_days = 365,
                         keep_old = FALSE,
                         save = FALSE) {
  check_attr(p, check = "daily_portfolio")
  check_attr(s, check = "daily_stocks")

  current_stocks <- as.character(s$Symbol[s$Date == max(s$Date) & s$CumQuant > 0])
  start_stocks <- as.character(s$Symbol[s$Date == max(s$Date) - n_days & s$CumQuant > 0])

  d <- s %>%
    filter(.data$CumQuant > 0) %>%
    arrange(.data$Date) %>%
    {
      if (!is.na(n_days)) filter(., .data$Date >= max(s$Date) - n_days) else .
    } %>%
    select(
      .data$Date, .data$Symbol, .data$Value, .data$CumQuant, .data$CumValue,
      .data$CumInvested, .data$Invested, .data$wt
    ) %>%
    {
      if (!keep_old) filter(., as.character(.data$Symbol) %in% current_stocks) else .
    } %>%
    group_by(.data$Symbol) %>%
    mutate(first_date = .data$Date == min(.data$Date)) %>%
    # run_fix = .data$Symbol %in% start_stocks) %>%
    # mutate(CumValue = .data$CumValue - .data$CumValue[.data$first_date] * as.integer(.data$run_fix)) %>%
    mutate(CumValue = .data$CumValue - .data$CumValue[.data$first_date]) %>%
    {
      if (rel) {
        mutate(., Hist = .data$CumValue / .data$CumInvested)
      } else {
        mutate(., Hist = .data$CumValue)
      }
    } %>%
    mutate(Hist = ifelse(.data$CumQuant == 0, NA, .data$Hist)) %>%
    mutate(BuySell = case_when(
      .data$Invested > 0 ~ "Bought",
      .data$Invested < 0 ~ "Sold",
    )) %>%
    mutate(BuySellShape = case_when(
      .data$BuySell == "Bought" ~ "24",
      .data$BuySell == "Sold" ~ "25"
    )) %>%
    mutate(Hist = ifelse(.data$CumQuant > 0, .data$Hist, tail(.data$Hist, 1))) %>%
    mutate(Symbol = factor(.data$Symbol, levels = unique(s$Symbol))) %>%
    filter(.data$CumQuant > 0)

  # Plot auxiliary stuff
  days <- as.integer(difftime(range(d$Date)[2], range(d$Date)[1], units = "days"))
  plot <- ggplot(d, aes(x = .data$Date, y = .data$Hist)) +
    geom_vline(xintercept = max(s$Date), linetype = "dashed", alpha = 0.7) +
    ylab(glued("Total changed [{x}]", x = ifelse(rel, "%", "$"))) +
    geom_hline(yintercept = 0, alpha = 0.8, colour = "black") +
    geom_line(aes(colour = .data$Symbol),
      alpha = 0.9, size = 0.5, na.rm = TRUE
    ) +
    geom_point(
      data = filter(d, !is.na(.data$BuySellShape)),
      aes(shape = .data$BuySellShape, colour = .data$Symbol),
      fill = "grey10", na.rm = TRUE
    ) +
    ggplot2::scale_shape_manual(values = c(24, 25), labels = c("Bought", "Sold")) +
    scale_size(range = c(0, 3.2)) +
    guides(size = "none", colour = "none", fill = "none") +
    xlim(min(d$Date), max(d$Date) + round(days * 0.06)) +
    labs(
      title = glued("Weighted Stocks Daily Change [{x}]", x = ifelse(rel, "%", "$")),
      x = NULL, colour = NULL,
      subtitle = sprintf(
        "%s delta since %s or first transaction",
        ifelse(rel, "Relative total", "Total"), min(d$Date)
      ),
      shape = "Transaction"
    ) +
    theme_lares(pal = 2, legend = "top") +
    geom_text(
      data = d[which(d$Date == max(d$Date)), ],
      aes(label = .data$Symbol, colour = .data$Symbol),
      hjust = -0.1, size = 3
    )

  if (group) plot <- plot + facet_grid(.data$Type ~ ., scales = "free", switch = "both")
  if (!is.na(n_days)) plot <- plot + labs(caption = glued("Last {n_days} days"))
  if (rel) {
    plot <- plot + scale_y_percent(position = "right")
  } else {
    plot <- plot + scale_y_formatNum(position = "right", abbr = TRUE, pos = "$")
  }
  if (save) {
    plot <- plot +
      ggsave("portf_stocks_histchange.png", width = 8, height = 5, dpi = 300)
  }
  return(plot)
}


####################################################################
#' Portfolio Plots: Growth (Cash + Invested)
#'
#' \code{splot_growth()} plots your portfolio's growth, in cash and investment,
#' since inception.
#'
#' @inheritParams splot_summary
#' @export
#' @rdname stocks_plots
splot_growth <- function(p, save = FALSE) {
  check_attr(p, check = "daily_portfolio")
  newp <- p %>%
    ungroup() %>%
    mutate(
      CumValue = .data$CumValue - tail(.data$CumValue, 1),
      CumCash = .data$CumCash - tail(.data$CumCash, 1),
      Portfolio = .data$Portfolio - tail(.data$Portfolio, 1)
    )
  caption <- paste0(
    "Portfolio: $", formatNum(newp$Portfolio[1], 0), " (",
    as.integer(range(p$Date)[2] - range(p$Date)[1]), " days)",
    "\nInvested: $", formatNum(newp$CumValue[1], 0), " (",
    round(100 * newp$CumValue[1] / newp$Portfolio[1], 1), "%)"
  )
  aux <- rbind(
    data.frame(Date = newp$Date, Amount = newp$CumCash, Type = "Cash"),
    data.frame(Date = newp$Date, Amount = newp$CumValue, Type = "Stocks")
  ) %>%
    left_join(select(newp, .data$Date, .data$Cash) %>%
      filter(.data$Cash != 0) %>%
      mutate(Type = "Cash"), by = c("Date", "Type")) %>%
    group_by(.data$Type) %>%
    mutate(Amount = .data$Amount - tail(.data$Amount, 1)) %>%
    ungroup() %>%
    group_by(.data$Date) %>%
    mutate(Portfolio = sum(.data$Amount))
  days <- nrow(aux) / 2

  plot <- ggplot(aux, aes(x = .data$Date, y = .data$Amount)) +
    geom_area(aes(y = .data$Amount, fill = .data$Type), position = "stack", alpha = 0.7) +
    labs(title = "Daily Total Portfolio Value", y = NULL, x = NULL, fill = "") +
    scale_y_dollar(
      position = "right",
      breaks = round(seq(min(aux$Amount), max(aux$Amount), length.out = 14))
    ) +
    xlim(min(aux$Date - round(days * 0.03)), max(aux$Date) + round(days * 0.03)) +
    annotate("text",
      label = caption, x = max(newp$Date),
      y = 0.09 * max(aux$Amount),
      size = 3.3, colour = "black", hjust = 1.1
    ) +
    geom_vline(xintercept = max(newp$Date), alpha = 0.5) +
    theme_lares(pal = 1) +
    theme(legend.position = "top", legend.justification = c(0, 1)) +
    geom_hline(yintercept = 0, size = 0.3, color = "black") +
    geom_hline(
      yintercept = aux$Amount[1], alpha = 0.9,
      colour = names(lares_pal()$palette[3])
    ) +
    geom_hline(
      yintercept = max(aux$Amount), size = 0.2,
      linetype = "dashed", colour = "#3DA4AB"
    )

  points <- aux[!is.na(aux$Cash) & aux$Cash != 0, ]
  if (nrow(points) > 0) {
    plot <- plot +
      geom_point(data = points, aes(x = .data$Date, y = .data$Portfolio)) +
      geom_text(
        data = points,
        aes(
          x = .data$Date, y = .data$Portfolio, hjust = -0.6,
          label = formatNum(.data$Cash, 2, abbr = TRUE)
        ),
        size = 2.7, angle = 90
      )
  }
  if (save) {
    plot <- plot +
      ggsave("portf_total_hist.png", width = 8, height = 5, dpi = 300)
  }
  return(plot)
}


####################################################################
#' Portfolio Plots: Dividends per Year and Quarter
#'
#' \code{stocks_plots()} plots a portfolio's historical dividends incomes
#' grouped by quarter an year.
#'
#' @inheritParams splot_summary
#' @param type Integer. Typo of plot. 1 for incomes.
#' @export
#' @rdname stocks_plots
splot_divs <- function(p, type = 1) {
  if (type == 1) {
    p %>%
      mutate(
        year = year(.data$Date),
        quarter = date_cuts(.data$Date, "Q")
      ) %>%
      group_by(.data$year, .data$quarter) %>%
      summarise(Dividend = sum(.data$Dividend)) %>%
      ungroup() %>%
      group_by(year) %>%
      mutate(DividendYear = sum(.data$Dividend)) %>%
      ungroup() %>%
      mutate(year = sprintf("%s\n%s", .data$year, formatNum(.data$DividendYear, pre = "$", abbr = TRUE))) %>%
      filter(.data$DividendYear > 0) %>%
      ggplot(aes(x = .data$quarter, y = .data$Dividend, fill = .data$quarter)) +
      geom_col() +
      facet_grid(. ~ year) +
      scale_y_dollar() +
      labs(
        title = "Dividends per Quarter and Year",
        subtitle = sprintf("Total: %s", formatNum(sum(p$Dividend), 0, pre = "$")),
        x = NULL, y = "Dividends (post-tax)"
      ) +
      theme_lares(pal = 1, grid = "Yy", legend = "none")
  }
}


####################################################################
#' Portfolio Plots: Daily ROI
#'
#' \code{splot_roi()} plots a portfolio's historical ROI since inception
#' or since last n days, with 2 moving average lines.
#'
#' @inheritParams splot_summary
#' @param n_days Integer. How many days back you want to see?
#' @param historical Boolean. Historical ROI metric? If not, ROI
#' will be calculated locally for n_days parameter
#' @param ma Numeric Vector. Select 2 values for moving averages.
#' Set to NA to turn this metric off
#' @export
#' @rdname stocks_plots
splot_roi <- function(p, n_days = 365, historical = TRUE, ma = c(12, 50), save = FALSE) {
  check_attr(p, check = "daily_portfolio")

  if (is.na(n_days)) {
    n_days <- nrow(p)
  }
  if (n_days > nrow(p)) {
    message(sprintf(
      "As there are less observations than for %s days, changed n_days all days: %s",
      n_days, nrow(p)
    ))
    n_days <- nrow(p)
  }

  n_days <- ifelse(n_days < max(ma), max(ma) + 2, n_days)
  newp <- rbind(p, mutate(tail(p, 1), Date = .data$Date - 1, ROI = 0, CumValue = 0))

  caption <- ifelse(is.na(ma)[1], "",
    paste("Showing moving averages for", ma[1], "and", ma[2], "days.")
  )

  if (!historical) newp <- mutate(newp, ROI = .data$ROI - newp$ROI[n_days])
  if (!is.na(n_days)) {
    newp <- slice(newp, 1:n_days)
    mas <- function(x, n = 5) {
      stats::filter(x, rep(1 / n, n), sides = 1)
    }
    if (n_days != nrow(p)) {
      caption <- paste(
        caption,
        "\nShowing last", n_days, "days,",
        ifelse(historical, "with historical ROI results.",
          "with relative ROI results."
        )
      )
    }
  }

  if (!is.na(ma)[1]) {
    newp <- newp %>%
      mutate(
        ma1 = mas(.data$ROI, ma[1]),
        ma2 = mas(.data$ROI, ma[2])
      )
  }

  pos <- mutate(newp, ROI = ifelse(.data$ROI >= 0, .data$ROI, 0.0001))
  neg <- mutate(newp, ROI = ifelse(.data$ROI < 0, .data$ROI, -0.0001))

  plot <- ggplot(newp, aes(x = .data$Date)) +
    geom_hline(yintercept = 0, size = 0.3, color = "black") +
    geom_hline(yintercept = newp$ROI[1], alpha = 0.9, colour = "#40A4D8") +
    geom_hline(
      yintercept = max(newp$ROI), size = 0.2,
      linetype = "dashed", colour = "#3DA4AB"
    ) +
    geom_hline(
      yintercept = min(newp$ROI), size = 0.2,
      linetype = "dashed", colour = "tomato"
    ) +
    geom_area(data = pos, aes(y = .data$ROI), fill = "#3DA4AB", alpha = 0.3) +
    geom_area(data = neg, aes(y = .data$ROI), fill = "tomato", alpha = 0.3) +
    geom_line(aes(y = .data$ROI), size = 0.5) +
    scale_x_date(date_labels = "%b%y") +
    labs(
      y = "ROI [%]", x = NULL,
      title = "Portfolio's Daily Growth (%)",
      subtitle = paste0(
        newp$Date[1], " | ROI: ",
        formatNum(newp$ROI[1], 2), "% ($",
        formatNum(newp$CumValue[1] - newp$CumInvested[1], 0),
        ") | Stocks Value: $", formatNum(newp$CumValue[1], 0),
        " | Portfolio: $", formatNum(newp$Portfolio[1], 0)
      ),
      caption = if (!is.na(ma)[1]) caption
    ) +
    theme_lares(legend = "top")

  if (!is.na(ma)[1]) {
    plot <- plot +
      geom_line(aes(y = .data$ma1),
        size = 0.7,
        colour = "#071D49", alpha = 0.9, na.rm = TRUE
      ) +
      geom_line(aes(y = .data$ma2),
        size = 0.7,
        colour = "darkorange", alpha = 0.9, na.rm = TRUE
      ) +

      if (save) {
        plot <- plot +
          ggsave("portf_daily_change.png", width = 8, height = 5, dpi = 300)
      }
  }

  return(plot)
}


####################################################################
#' Portfolio Plots: Types of Stocks
#'
#' \code{splot_types()} lets the user plot types or categories of tickers.
#'
#' @inheritParams splot_summary
#' @export
#' @rdname stocks_plots
splot_types <- function(s, save = FALSE) {
  check_attr(s, check = "daily_stocks")

  plot <- s %>%
    filter(.data$Date == max(.data$Date), .data$CumQuant > 0) %>%
    group_by(.data$Type) %>%
    mutate(label = paste0(.data$Type, "\n", formatNum(
      100 * sum(.data$CumValue) / sum(s$CumValue[s$Date == max(s$Date)]), 0
    ), "%")) %>%
    ggplot() +
    geom_bar(aes(x = "", y = .data$CumValue, fill = .data$Symbol),
      width = 1, stat = "identity"
    ) +
    facet_grid(. ~ label, scales = "free") +
    theme_lares(pal = 1) +
    scale_y_formatNum(expand = c(0, 0), abbr = TRUE, pre = "$") +
    labs(x = NULL, y = "Total value", title = "Portfolio's Category Distribution")
  if (save) {
    plot <- plot +
      ggsave("portf_distribution.png", width = 8, height = 5, dpi = 300)
  }
  return(plot)
}


####################################################################
#' ETF's Sectors Breakdown
#'
#' \code{etf_sector()} scraps etf.com data for sector breakdown on ETFs.
#' Use \code{splot_etf()} for visualization.
#'
#' @family Investment
#' @inheritParams stocks_hist
#' @param etf Character Vector. Which ETFs you wish to scrap?
#' @return data.frame with ETF break.down data by sector
#' @examples
#' \donttest{
#' etf_sector(etf = "VTI")
#' }
#' @export
etf_sector <- function(etf = "VTI", quiet = FALSE, cache = TRUE) {
  if ("daily_stocks" %in% attr(etf, "type")) {
    etf <- as.character(unique(etf$Symbol[etf$Date == max(etf$Date)]))
  }
  cache_file <- c(as.character(Sys.Date()), "etf_sector", etf)
  if (cache_exists(cache_file) && cache) {
    results <- cache_read(cache_file, quiet = quiet)
    return(results)
  }

  if (!quiet) message(">>> Downloading sectors for each ETF...")
  ret <- data.frame()
  nodata <- NULL
  for (i in seq_along(etf)) {
    info <- toupper(etf[i])
    url <- sprintf("https://etfdb.com/etf/%s/", info)
    # exists <- tryCatch({!httr::http_error(url)}, error = function(err) {FALSE})
    sector <- tryCatch(
      suppressWarnings(content(GET(url))),
      error = function(err) {
        closeAllConnections()
        gc()
        nodata <- c(nodata, info)
        return(NULL)
      }
    )
    if (is.null(sector)) next
    tables <- sector %>% html_table()
    sector_table <- lapply(tables, function(x) {
      "Sector" %in% colnames(x)
    }) %>% unlist()
    if (sum(sector_table) > 0) {
      sec <- tables[sector_table][[1]] %>%
        mutate(ETF = info) %>%
        mutate(Percentage = as.numeric(gsub("%", "", .data$Percentage)))
      ret <- rbind(ret, sec)
    } else {
      nodata <- c(nodata, info)
    }
    if (!quiet && length(etf) > 1) {
      statusbar(i, length(etf), info)
    }
  }

  attr(ret, "type") <- "etf_sector"
  if (length(unique(nodata)) > 0) {
    if (!quiet) message("Some ticks were not found as ETF: ", v2t(unique(nodata)))
  }
  if (nrow(ret) == 0) {
    if (!quiet) message("No data found for given Tickers!")
    invisible(return(NULL))
  } else {
    cache_write(ret, cache_file, quiet = TRUE)
    return(ret)
  }
}


####################################################################
#' Portfolio's Sector Distribution (ETFs)
#'
#' \code{splot_etf()} lets the user plot his portfolio's distribution,
#' specifically ETF's sectors.
#'
#' @family Scrapper
#' @inheritParams splot_summary
#' @inheritParams stocks_file
#' @param keep_all Boolean. Keep "Not Known / Not ETF"?
#' @export
#' @rdname stocks_plots
splot_etf <- function(s, keep_all = FALSE, cache = TRUE, save = FALSE) {
  check_attr(s, check = "daily_stocks")
  if (!"Date" %in% colnames(s)) s$Date <- Sys.Date()
  cache_file <- c(as.character(Sys.Date()), "splot_etf", s)
  if (cache_exists(cache_file) && cache) {
    etfs <- cache_read(cache_file, quiet = quiet)
  } else {
    etfs <- etf_sector(s)
  }

  if (length(etfs) > 0) {
    df <- etfs %>%
      right_join(
        select(s, .data$Symbol, .data$CumValue, .data$Date) %>%
          filter(.data$Date == max(.data$Date)) %>%
          mutate(Symbol = as.character(.data$Symbol)),
        by = c("ETF" = "Symbol")
      ) %>%
      {
        if (keep_all == FALSE) filter(., !is.na(.data$Sector)) else .
      } %>%
      mutate(
        ETF = factor(.data$ETF, levels = s$Symbol[s$Date == max(s$Date)]),
        Sector = ifelse(is.na(.data$Sector),
          "Not Known / Not ETF",
          as.character(.data$Sector)
        )
      ) %>%
      replace(., is.na(.), 100) %>%
      mutate(Value = .data$CumValue * .data$Percentage / 100) %>%
      group_by(.data$Sector) %>%
      mutate(ValueSector = sum(.data$Value)) %>%
      ungroup() %>%
      group_by(.data$Sector) %>%
      mutate(label = paste0(
        .data$Sector, " (",
        signif(100 * .data$ValueSector / sum(s$CumValue[s$Date == max(s$Date)]), 2), "%)"
      ))

    plot <- ggplot(df, aes(
      x = reorder(.data$label, .data$ValueSector),
      y = .data$Value, fill = .data$ETF
    )) +
      geom_bar(width = 1, stat = "identity") +
      coord_flip() +
      scale_y_dollar(expand = c(0, 0)) +
      theme_lares(pal = 1) +
      labs(
        x = NULL, y = "Total value [$]", fill = NULL,
        title = "Portfolio's Sector Distribution (ETFs)"
      )

    if (save) {
      plot <- plot +
        ggsave("portf_distribution_etfs.png", width = 8, height = 5, dpi = 300)
    }
    return(plot)
  } else {
    return(noPlot("No data here!"))
  }
}


####################################################################
#' Portfolio's Calculations and Plots
#'
#' \code{stocks_obj()} lets the user create his portfolio's calculations and
#' plots for further study.
#'
#' @inheritParams stocks_file
#' @inheritParams daily_portfolio
#' @inheritParams stocks_hist
#' @inheritParams daily_stocks
#' @param data List. Containing the following dataframes: portfolio,
#' transactions, cash. They have to follow the original xlsx format
#' @param sectors Boolean. Return sectors segmentation for ETFs?
#' @param parg Boolean. Personal argument. Used to personalize stuff, in this
#' case, taxes changed from A to B in given date (hard-coded)
#' @return List. Aggregated results and plots.
#' @export
#' @rdname stocks_report
stocks_obj <- function(data = stocks_file(),
                       cash_fix = 0,
                       tax = 30,
                       sectors = FALSE,
                       parg = FALSE,
                       window = c("1M", "YTD", "1Y", "MAX"),
                       cache = TRUE,
                       quiet = FALSE) {
  check_attr(data, check = "stocks_file")
  check_opts(window, c("1W", "1M", "6M", "1Y", "YTD", "5Y", "MAX"))

  ret <- list()
  trans <- data$transactions
  cash <- data$cash
  tickers <- data$portfolio

  message(">>> Downloading historical data for each stock...")
  ret[["quotes"]] <- hist <- stocks_hist(
    symbols = tickers$Symbol,
    from = tickers$StartDate,
    tax = tax,
    parg = parg,
    cache = cache,
    quiet = quiet
  )

  ws <- length(window)
  ret[["stocks"]] <- s <- daily_stocks(hist, trans, tickers, window = "MAX")
  ret[["portfolio"]] <- p <- daily_portfolio(hist, trans, cash, cash_fix = cash_fix, window = "MAX")

  # Fixed plots
  ret[["plots_fixed"]] <- list(
    "Positions" = splot_summary(p, s),
    "Types of Stocks" = splot_types(s),
    "ETFs by Industry" = if (sectors) splot_etf(s, cache = cache) else NULL,
    "Dividends Incomes" = if (sum(p$Dividend) > 0) splot_divs(p, type = 1) else NULL
  )

  # Relative plots (using time windows)
  message(glued(">>> Running calculations and plots for {ws} time window{ifelse(ws>1,'s','')}..."))
  plots_relative <- lapply(window, function(x) {
    # Filter the data given the window
    s <- daily_stocks(hist, trans, tickers, window = x)
    p <- daily_portfolio(hist, trans, cash, cash_fix = cash_fix, window = x)

    # Visualizations
    plots <- list()
    # plots[["ROI"]] <- splot_roi(p, n_days = 5)
    plots[["History"]] <- splot_growth(p)
    plots[["Relative Growth"]] <- splot_change(p, s, rel = TRUE, n_days = NA, keep_old = FALSE)
    plots[["Absolute Growth"]] <- splot_change(p, s, rel = FALSE, n_days = NA, keep_old = FALSE)
    plots
  })
  names(plots_relative) <- sprintf("plots_%s", window)
  ret[["plots_relative"]] <- plots_relative

  attr(ret, "type") <- "stocks_obj"
  return(ret)
}
# x <- stocks_obj(window = c("1M","1Y","YTD"))

####################################################################
#' Portfolio's Full Report and Email
#'
#' \code{stocks_report()} lets the user create his portfolio's full report with
#' plots and send it to an email with the HTML report attached
#'
#' @inheritParams stocks_file
#' @param data Character. \code{stocks_obj()} output. If NA, automatic
#' parameters and \code{stocks_file()} defaults will be used.
#' @param dir Character. Directory for HTML report output. If set to NA,
#' current working directory will be used. If mail sent, file will be erased
#' @param mail Boolean. Do you want to send an email with the report attached?
#' If not, an HTML file will be created in dir
#' @param attachment Boolean. Create and add report as attachment if
#' \code{mail=TRUE}? If not, no report will be rendered and only tabulated
#' summaries will be included on email's body.
#' @param to Character. Email to send the report to
#' @param sectors Boolean. Return sectors segmentation for ETFs?
#' @param keep Boolean. Keep HTML file when sent by email?
#' @param creds Character. Credential's user (see \code{get_creds()}) for
#' sending mail and Dropbox interaction.
#' @return Invisible list. Aggregated results and plots.
#' @examples
#' \dontrun{
#' list <- stocks_obj()
#' stocks_report(list, dir = "~/Desktop")
#' }
#' @export
#' @rdname stocks_report
stocks_report <- function(data = NA,
                          keep_old = TRUE,
                          dir = NA,
                          mail = FALSE,
                          attachment = TRUE,
                          to = "laresbernardo@gmail.com",
                          sectors = FALSE,
                          keep = FALSE,
                          creds = NA,
                          cache = TRUE) {
  try_require("rmarkdown")
  tic("stocks_report")

  if (is.na(data)[1]) {
    df <- stocks_file(creds = creds, keep_old = keep_old, cache = cache)
    data <- stocks_obj(df, sectors = sectors, parg = is.na(creds), cache = cache)
  }

  check_attr(data, check = "stocks_obj")

  # pandoc <- Sys.getenv("RSTUDIO_PANDOC")
  # Sys.setenv(RSTUDIO_PANDOC = pandoc)
  if (is.na(dir)) dir <- getwd()

  # Summary as tables
  data$summary_df1 <- data$stocks %>%
    filter(.data$Date == max(.data$Date), .data$CumQuant > 0) %>%
    mutate(Change = sprintf(
      "%s (%s)",
      formatNum(100 * .data$DifUSD / (.data$CumValue - .data$DifUSD), 2, pos = "%", sign = TRUE),
      formatNum(.data$DifUSD / .data$CumQuant, 2, sign = TRUE)
    )) %>%
    arrange(desc(abs(.data$DifUSD / .data$CumValue))) %>%
    select(
      .data$Symbol, .data$Value, .data$Change, .data$CumQuant,
      .data$DifUSD, .data$CumValue, .data$wt, .data$CumROI
    ) %>%
    rename(
      "Abs.Change" = .data$DifUSD, "Quant" = .data$CumQuant,
      "Total Value" = .data$CumValue, "Weight [%]" = .data$wt,
      "Hist.ROI [%]" = .data$CumROI
    )
  data$summary_df2 <- data$portfolio %>%
    filter(.data$Date == max(.data$Date)) %>%
    mutate(DifP = round(100 * .data$DifUSD / (.data$CumValue - .data$DifUSD), 2)) %>%
    mutate_if(is.numeric, function(x) formatNum(x, 2, signif = 6)) %>%
    mutate_all(as.character) %>%
    select(.data$Date, one_of(sort(colnames(.)))) %>%
    tidyr::gather() %>%
    rename("Metric" = .data$key, "Value" = .data$value) %>%
    mutate("Window" = c(
      rep("Today", 2), rep("Total", 5),
      rep("Today", 4), rep("Total", 2)
    )) %>%
    arrange(.data$Window)

  # Can be more accurate with names but works for me!
  params <- list(
    summary_df1 = data$summary_df1,
    summary_df2 = data$summary_df2,
    plots_fixed = data[["plots_fixed"]],
    plots_relative = data[["plots_relative"]]
  )

  if (attachment) {
    message(">>> Rendering HTML report...")
    html_file <- "stocksReport.html"
    render(system.file("docs", "stocksReport.Rmd", package = "lares"),
      output_file = html_file,
      output_dir = dir,
      params = params,
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    message("HTML report created succesfully!")
  }

  if (mail) {
    try_require("knitr")
    subject <- sprintf(
      "Report: %s | %s (%s)", max(data$portfolio$Date),
      paste0(data$summary_df2$Value[data$summary_df2$Metric == "DifUSD"]),
      paste0(data$summary_df2$Value[data$summary_df2$Metric == "DifP"], "%")
    )
    html_body <- paste0(
      "<p>Stocks Summary:</p>",
      kable(data$summary_df1,
        align = "lrcrrrr", row.names = TRUE,
        digits = c(0, 2, 0, 2, 0, 0, 1), format = "html"
      ),
      "<p>Portfolio Status:</p>",
      kable(data$summary_df2, align = "lr", row.names = FALSE, format = "html")
    )

    message(">>> Sending email...")
    mail_send(
      to = to,
      subject = subject,
      html = html_body,
      attachment = if (attachment) html_file else NULL,
      creds = creds,
      quiet = FALSE
    )
    if (!keep && attachment) invisible(file.remove(html_file))
  }

  toc("stocks_report")
  return(invisible(data))
}

.filter_window <- function(df, window) {
  new_df <- df %>%
    arrange(desc(.data$Date)) %>%
    {
      if (window == "1W") filter(., .data$Date >= Sys.Date() - 7) else .
    } %>%
    {
      if (window == "1M") filter(., .data$Date >= Sys.Date() %m-% months(1)) else .
    } %>%
    {
      if (window == "6M") filter(., .data$Date >= Sys.Date() %m-% months(6)) else .
    } %>%
    {
      if (window == "1Y") filter(., .data$Date >= Sys.Date() %m-% years(1)) else .
    } %>%
    {
      if (window == "YTD") filter(., .data$Date >= floor_date(Sys.Date(), "year")) else .
    } %>%
    {
      if (window == "5Y") filter(., .data$Date >= Sys.Date() %m-% years(5)) else .
    }

  if ("ROI" %in% colnames(df) && window == "MAX") { # Dataframe: portfolio
    # Add a last row (first date) with no data
    new_df <- rbind(new_df, df %>% arrange(.data$Date) %>% slice(1) %>% ungroup() %>% mutate_if(
      is.numeric, function(x) 0
    ) %>% mutate(Date = .data$Date - 1))
  }
  if (!"ROI" %in% colnames(df) && window == "MAX") { # Dataframe: stocks
    new_df <- rbind(new_df, df %>% arrange(.data$Date) %>%
      group_by(.data$Symbol) %>% slice(1) %>% ungroup() %>% mutate_if(
        is.numeric, function(x) 0
      ) %>% mutate(Date = .data$Date - 1))
  }
  new_df
}

# # TESTING
# devtools::load_all()
# library(lares)
# library(tidyverse)
# library(openxlsx)
# library(quantmod)
# library(jsonlite)
# library(lubridate)
# library(rvest)
# library(httr)
# library(rdrop2)
# data <- stocks_file()
# trans <- data$transactions
# cash <- data$cash
# tickers <- data$portfolio
# q <- stocks_obj(window = c("1M","1Y","MAX"), sectors = FALSE, cache = TRUE)
# hist <- q$quotes
# p <- q$portfolio
# s <- q$stocks
# stocks_report(q, dir = "~/Desktop")

# s %>%
#   filter(Value > 0, CumQuant > 0) %>%
#   select(Symbol, Date, CumQuant, Value) %>%
#   arrange(Date) %>%
#   group_by(Symbol) %>%
#   mutate(RelValue = Value / Value[1]) %>%
#   #filter(Symbol == "TSLA") %>%
#   ggplot(aes(x = Date, y = log(RelValue), colour = Symbol)) +
#   geom_line() +
#   theme_lares(pal = 2) +
#   scale_y_percent()
