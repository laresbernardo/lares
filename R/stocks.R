####################################################################
#' Get Personal Portfolio's Data
#' 
#' This function lets the user download his personal Excel with his 
#' Portfolio's data, locally or from Dropbox.
#' 
#' @family Investment
#' @family Credentials
#' @param filename Character. Import a local Excel file
#' @param creds Character. Dropbox's credentials (see \code{get_creds()})
#' @param auto Boolean. Automatically use my local personal file? You might want
#' to set in into your .Renviron \code{LARES_PORTFOLIO=~/dir/to/your/file.xlsx} so you
#' can leave all other parameters as \code{NA} and use it every time.
#' @param sheets Character Vector. Names of each sheet containing Portfolio summary,
#' Cash, and Transactions information
#' @param keep_old Boolean. Include sold tickers even though not currently in portfolio?
#' @examples 
#' \dontrun{
#' # Load lares dummy portfolio XLSX
#' file <- system.file("inst/docs", "dummyPortfolio.xlsx", package = "lares")
#' df <- stocks_file(filename = file, 
#'                   sheets = c("Portafolio","Fondos","Transacciones"), 
#'                   keep_old = FALSE)
#' }
#' @export
stocks_file <- function(filename = NA, 
                        creds = NA, 
                        auto = TRUE, 
                        sheets = c("Portafolio","Fondos","Transacciones"),
                        keep_old = TRUE) {
  
  processFile <- function(file, keep_old = TRUE) {
    port <- read.xlsx(file, sheet = sheets[1], skipEmptyRows = TRUE, detectDates = TRUE)
    cash <- read.xlsx(file, sheet = sheets[2], skipEmptyRows = TRUE, detectDates = TRUE)
    trans <- read.xlsx(file, sheet = sheets[3], skipEmptyRows = TRUE, detectDates = TRUE)
    if ("Value" %in% colnames(trans)) 
      trans <- rename(trans, Each = .data$Value, Invested = .data$Amount)
    if (!keep_old) port <- port[port$Stocks != 0,]
    mylist <- list("portfolio" = port, "transactions" = trans, "cash" = cash)
    return(mylist)
  }
  
  # FOR PERSONAL USE
  local <- Sys.info()
  if (auto & Sys.getenv("LARES_PORTFOLIO") != "") {
    message("Using BL's local file...")
    local <- Sys.getenv("LARES_PORTFOLIO")
    results <- processFile(local, keep_old) 
  } else {
    # FOR EVERYONE'S USE
    if (!is.na(filename)) {
      if (file.exists(filename)) results <- processFile(filename, keep_old) else
        stop("Error: that file doesn't exist or it's not in your working directory!")
    } else {
      # FOR DROPBOX'S USE
      file <- "Portfolio LC.xlsx"
      db_download(file, 
                  xlsx = FALSE, # Do not import as Excel, just download
                  newname = file,
                  token_dir = creds)
      results <- processFile(file, keep_old = keep_old)
      file.remove(file)
    } 
  }
  
  attr(results$portfolio, "type") <- "stocks_file_portfolio"
  attr(results$transactions, "type") <- "stocks_file_transactions"
  attr(results$cash, "type") <- "stocks_file_cash"
  attr(results, "type") <- "stocks_file"
  
  message("File imported succesfully!")
  return(results)   
}

####################################################################
#' Download Stocks Historical Data
#' 
#' This function lets the user download stocks historical data.
#' 
#' @family Investment
#' @family Scrapper
#' @param ticks Character Vector. Symbols/Tickers to quote in real time.
#' @examples 
#' # Multiple quotes at the same time
#' stocks_quote(c("VTI","VOO","TSLA"))
#' @export
stocks_quote <- function(ticks) {
  ret <- noret <- c()
  qRoot <- paste0(
    "https://query1.finance.yahoo.com/v7/finance/quote?fields=symbol,",
    "longName,regularMarketPrice,regularMarketChange,regularMarketTime&formatted=false&symbols=")
  for (i in 1:length(ticks)) {
    z <- fromJSON(paste(qRoot, paste(ticks[i], collapse = ","), sep = ""))
    if (length(z$quoteResponse$result) > 0) {
      cols <- c("symbol", "quoteType", "regularMarketTime", 
                "regularMarketPrice", "regularMarketChange", 
                "market", "longName")
      if (!"longName" %in% colnames(z$quoteResponse$result))
        z$quoteResponse$result$longName <- z$quoteResponse$result$symbol
      z <- select(z$quoteResponse$result, one_of(cols))
      ret <- rbind(ret, z) 
    } else noret <- rbind(noret, ticks[i])
  }
  if (length(noret) > 0) 
    message(paste("No results for", vector2text(noret)))
  if (length(ret) > 0) {
    colnames(ret) <- c("Symbol","Type","QuoteTime", "Value", "DailyChange", "Market", "SymbolName")
    ret <- data.frame(ret) %>%
      mutate(QuoteTime = as.POSIXct(.data$QuoteTime, origin = '1970-01-01 00:00:00'))
    row.names(ret) <- NULL 
    return(ret)
  }
}


####################################################################
#' Download Stocks Historical Data
#' 
#' This function lets the user download stocks historical data
#' 
#' @family Investment
#' @family Scrapper
#' @param symbols Character Vector. List of symbols to download historical data
#' @param from,to Date. Dates for range. If not set, 1 year will be downloaded.
#' Do use more than 4 days or will be over-written.
#' @param today Boolean. Do you wish to add today's live quote? This will happen
#' only if to value is the same as today's date
#' @param tax Numeric. How much [0-99] of your dividends are gone with taxes? 
#' @param parg Boolean. Personal argument. Used to personalize stuff, in this
#' case, taxes changed from A to B in given date (hard-coded)
#' @param verbose Boolean. Print results and progress while downloading?
#' @examples 
#' \dontrun{
#' df <- stocks_hist(symbols = c("VTI", "FB"), from = Sys.Date() - 7)
#' head(df)
#          Date Symbol  Value
# 1  2020-05-15    VTI 143.83
# 2  2020-05-15     FB 210.88
# 3  2020-05-14    VTI 143.03
# 4  2020-05-14     FB 206.87
# 5  2020-05-13    VTI 142.88
# 6  2020-05-13     FB 207.94
#' }
#' @export
stocks_hist <- function(symbols = c("VTI", "TSLA"), 
                        from = Sys.Date() - 365, 
                        to = Sys.Date(),
                        today = TRUE,
                        tax = 30, 
                        parg = FALSE,
                        verbose = TRUE) {
  
  try_require("quantmod")
  
  # if (!haveInternet()) stop("You currently have NO internet connection!")
  
  options("getSymbols.warning4.0" = FALSE)
  options("getSymbols.yahoo.warning" = FALSE)
  data <- divs <- c()
  
  if (!any(is.na(symbols))) {
    if (length(from) != length(symbols)) 
      from <- rep(from[1], length(symbols))
    
    for (i in 1:length(symbols)) {
      # Daily quotes (except today)
      symbol <- as.character(symbols[i])
      if (as.Date(from[i]) > (Sys.Date() - 4)) from[i] <- Sys.Date() - 4
      start_date <- as.character(from[i])
      values <- suppressWarnings(data.frame(getSymbols(
        symbol, env = NULL, from = start_date, to = to, src = "yahoo")))
      values <- cbind(row.names(values), as.character(symbol), values)
      colnames(values) <- c("Date","Symbol","Open","High","Low","Close","Volume","Adjusted")
      values <- mutate(values, Adjusted = rowMeans(select(values, .data$High, .data$Close), na.rm = TRUE))
      row.names(values) <- NULL
      # if (as.Date(from[i]) > (Sys.Date() - 4))
      #   values <- head(values, 1)
      
      # Add right now's data
      if (today & to == Sys.Date()) {
        now <- stocks_quote(symbol)
        # Append to historical data / replace most recent
        if (length(now) > 0) {
          now <- data.frame(Date = as.character(as.Date(now$QuoteTime)), 
                            Symbol = symbol,
                            Open = now$Value, High = now$Value, 
                            Low = now$Value, Close = now$Value,
                            Volume = NA, Adjusted = now$Value)
          values <- filter(values, as.Date(.data$Date) != as.Date(now$Date))
          values <- rbind(values, now)
        }
      }
      # Append to other symbols' data
      data <- rbind(data, values)
      
      # Dividends 
      d <- suppressWarnings(getDividends(
        as.character(symbol), from = start_date, split.adjust = FALSE))
      if (nrow(d) > 0) {
        div <-  data.frame(Symbol = rep(symbol, nrow(d)),
                           Date = ymd(row.names(data.frame(d))),
                           Div = as.vector(d),
                           DivReal = as.vector(d)*(100 - tax)/100)
        divs <- rbind(divs, div)
        if (parg == TRUE)
          divs <- mutate(divs, DivReal = ifelse(
            .data$Date < as.Date('2020-03-03'), 
            as.vector(d) * 0.30, 
            as.vector(d) * 0.15))
      }
      
      if (verbose & length(symbols) > 1) {
        info <- paste(symbol, "since", start_date, "   ")
        statusbar(i, length(symbols), info)  
      }
    }
  } else {
    message("You need to define which stocks to bring. Use the 'symbols=' parameter.") 
  }
  
  results <- data %>% 
    select(.data$Date, .data$Symbol, .data$Adjusted) %>% 
    rename(Value = .data$Adjusted) %>%
    filter(.data$Value > 0) %>%
    mutate(Date = as.Date(.data$Date), 
           Symbol = as.character(.data$Symbol))
  if (length(divs) > 0)
    results <- results %>% 
    left_join(mutate(divs, Symbol = as.character(.data$Symbol)), by = c("Date", "Symbol"))
  results <- results %>% 
    replace(is.na(.), 0) %>% 
    arrange(desc(.data$Date))
  
  attr(results, "type") <- "stocks_hist"
  
  return(results)
}


####################################################################
#' Daily Stocks Dataframe
#' 
#' This function creates a dataframe will all relevant metrics and values,
#' for each ticker or symbol, for every day since inception.
#' 
#' @family Investment
#' @param hist Dataframe. Result from \code{stocks_hist()}
#' @param trans Dataframe. Result from \code{stocks_file()$transactions}
#' @param tickers Dataframe. Result from \code{stocks_file()$portfolio}
#' @export
daily_stocks <- function(hist, trans, tickers = NA) {
  
  check_attr(hist, check = "stocks_hist")
  check_attr(trans, check = "stocks_file_transactions")
  if (!is.na(tickers)[1])
    check_attr(tickers, check = "stocks_file_portfolio")
  
  with_div <- "DivReal" %in% colnames(hist)
  
  # hist_structure <- c("Date", "Symbol", "Value", "Div", "DivReal")
  # trans_structure <- c("Symbol", "Date", "Quant", "Each", "Invested", "Cost")
  # if (!all(hist_structure %in% colnames(hist))) {
  #   stop(paste("The structure of the 'hist' table should be:",
  #              paste(shQuote(hist_structure), collapse = ", ")))}
  # if (!all(trans_structure %in% colnames(trans))) {
  #   stop(paste("The structure of the 'trans' table should be:",
  #              paste(shQuote(trans_structure), collapse = ", ")))}
  
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
    {if (!with_div) mutate(., DivReal = 0) else .} %>%
    group_by(.data$Symbol) %>%
    mutate(CumQuant = cumsum(.data$Quant),
           CumInvested = cumsum(.data$Invested),
           CumCost = cumsum(.data$Cost),
           CumValue = .data$CumQuant * .data$Value,
           CumROI = 100 * (.data$CumValue/.data$CumInvested - 1),
           CumROI = as.numeric(ifelse(
             .data$CumValue == 0, 100*(.data$Each*abs(.data$Quant)/lag(.data$CumInvested) - 1), 
             .data$CumROI)),
           Dividend = .data$DivReal * .data$CumQuant,
           DifUSD = .data$CumValue - .data$Invested - lag(.data$CumValue),
           CumDividend = cumsum(.data$Dividend)) %>% 
    select(.data$Date, .data$Symbol, .data$Value, .data$Quant, .data$Each, .data$Invested, 
           .data$Cost, .data$Dividend, .data$CumValue, .data$CumInvested, .data$CumROI, 
           .data$CumQuant, .data$DifUSD, .data$CumDividend, .data$CumCost) %>%
    group_by(.data$Date, .data$Symbol) %>% slice(1) %>%
    arrange(desc(.data$Date), desc(.data$CumValue)) %>% 
    ungroup()
  
  if ("Type" %in% colnames(tickers)) {
    tickers_structure <- c("Symbol", "Type")
    if (!all(tickers_structure %in% colnames(tickers))) {
      stop(paste("The structure of the 'tickers' table should be:",
                 paste(shQuote(tickers_structure), collapse = ", ")))}
    daily <- left_join(daily, select(tickers, .data$Symbol, .data$Type), "Symbol")
  } else daily$Type <- "No type"
  
  daily$Symbol <- factor(daily$Symbol, levels = unique(daily$Symbol[daily$Date == max(daily$Date)]))
  
  attr(daily, "type") <- "daily_stocks"
  return(daily)
  
}


####################################################################
#' Daily Portfolio Dataframe
#' 
#' This function creates a dataframe will all relevant metrics and values,
#' for the overall portfolio, for every day since inception.
#' 
#' @family Investment
#' @param hist Dataframe. Result from \code{stocks_hist()}
#' @param trans Dataframe. Result from \code{stocks_file()$transactions}
#' @param cash Dataframe. Result from \code{stocks_file()$cash}
#' @param cash_fix Numeric. If, for some reason, you need to fix your 
#' cash amount for all reports, set the amount here
#' @export
daily_portfolio <- function(hist, trans, cash, cash_fix = 0) {
  
  check_attr(hist, check = "stocks_hist")
  check_attr(trans, check = "stocks_file_transactions")
  check_attr(cash, check = "stocks_file_cash")
  
  temp <- expand.grid(Date = unique(hist$Date), Symbol = unique(hist$Symbol)) %>%
    left_join(daily_stocks(hist, trans), c("Date", "Symbol")) %>%
    mutate(Date = as.Date(.data$Date)) %>%
    arrange(desc(.data$Date), .data$Symbol) %>%
    group_by(.data$Symbol) %>%
    tidyr::fill(.data$Value, .data$CumInvested, .data$CumValue, 
                .data$CumDividend, .data$CumCost, .direction = "up") %>%
    group_by(.data$Date) %>% 
    summarise_if(is.numeric, list(~sum(., na.rm = TRUE))) %>%
    arrange(desc(.data$Date)) %>%
    mutate(ROI = 100 * (.data$CumValue/.data$CumInvested - 1)) %>%
    select(.data$Date, .data$CumInvested, .data$CumValue, 
           .data$ROI, .data$Invested, .data$CumCost, .data$CumDividend, .data$Dividend)
  
  days <- data.frame(Date = as.Date(min(hist$Date):Sys.Date(), origin = "1970-01-01")) %>%
    left_join(temp, c("Date")) %>%
    tidyr::fill(.data$ROI, .data$CumInvested, .data$CumValue, 
                .data$CumDividend, .data$CumCost, .direction = "up") %>%
    left_join(select(cash, .data$Date, .data$Cash), "Date") %>% 
    replace(is.na(.), 0) %>%    
    mutate(DifUSD = .data$CumValue - .data$Invested - lag(.data$CumValue),
           Cash = as.numeric(ifelse(is.na(.data$Cash), 0, .data$Cash)),
           CumCash = round(cumsum(.data$Cash) + cumsum(.data$Dividend) - .data$CumInvested - 
                             .data$CumCost + cash_fix)) %>% 
    filter(.data$CumInvested != 0) %>%
    arrange(desc(.data$Date)) %>% 
    ungroup()
  
  # One row per day only
  ret <- days %>% 
    group_by(.data$Date, .data$CumInvested, .data$CumValue, 
             .data$CumCost, .data$CumDividend, .data$ROI) %>%  
    summarise_if(is.numeric, sum) %>%
    arrange(desc(.data$Date)) %>% 
    ungroup() %>%
    mutate(Portfolio = .data$CumCash + .data$CumValue)
  
  attr(ret, "type") <- "daily_portfolio"
  return(ret)
  
}

################# PLOTTING FUNCTIONS #################

####################################################################
#' Portfolio Plots: Total Summary
#' 
#' This function plots a summary for the whole portfolio, showing
#' how much have you invested, how much has each ticker changed, etc.
#' 
#' @family Investment
#' @family Investment Plots
#' @param p Dataframe. Result from \code{daily_portfolio()}
#' @param s Dataframe. Result from \code{daily_stocks()}
#' @param save Boolean. Save plot into a local file?
#' @export
splot_summary <- function(p, s, save = FALSE) {
  
  check_attr(p, check = "daily_portfolio")
  check_attr(s, check = "daily_stocks")
  
  today <- filter(p, .data$Date == max(.data$Date))
  summary <- rbind(
    paste0("Portfolio: $", formatNum(today$Portfolio, 0)," | ", today$Date),
    paste0("Stocks: $", formatNum(today$CumValue,0)," & Cash: $", formatNum(today$CumCash,0)),
    paste0("ROI: ", formatNum(today$ROI, 2),"% ($",
           formatNum(today$CumValue - today$CumInvested, 0),")"),
    paste0("Dividends: $", formatNum(today$CumDividend, 0)," | Expenses: $",
           formatNum(today$CumCost, 0)))
  
  today <- filter(s, .data$Date == max(.data$Date)) %>% filter(.data$CumQuant > 0)
  tops <- max(rbind(today$CumInvested, today$CumValue))
  plot <- today %>%
    mutate(shapeflag = ifelse(.data$CumROI < 0, 25, 24), 
           box = -tops/5.5,
           Symbol = factor(
             paste0(.data$Symbol, " (", 
                    formatNum(100*.data$CumValue/sum(.data$CumValue)), "%)"),
             levels = paste0(.data$Symbol, " (", formatNum(100*.data$CumValue/sum(.data$CumValue)), "%)")),
           DifUSD = .data$CumValue - .data$CumInvested,
           DifPer = 100 * .data$DifUSD / .data$CumInvested) %>%
    ggplot(aes(x = reorder(.data$Symbol, .data$CumInvested))) + 
    geom_hline(yintercept = 0, colour = "black") +
    geom_col(aes(y = .data$CumInvested, fill = .data$Symbol, group = 1)) +
    geom_col(aes(y = .data$CumInvested + .data$DifUSD, fill = .data$Symbol), alpha = 0.5) +
    geom_col(aes(y = .data$box), fill = "grey", alpha = 0.5) +
    geom_point(aes(y = .data$CumInvested + .data$DifUSD, shape = .data$shapeflag), colour = "black") +
    scale_shape_identity() +
    geom_text(aes(label = paste0("$",formatNum(.data$DifUSD, 0)), 
                  y = .data$CumInvested + .data$DifUSD), 
              size = 3, hjust = -.2, vjust = -0.2) +
    geom_text(aes(label = paste0(round(.data$DifPer, 1), "%"), 
                  y = .data$CumInvested + .data$DifUSD), 
              size = 2.9, hjust = -.2, vjust = 1.2) +
    geom_text(aes(label = paste0("$", formatNum(.data$CumValue, 0)), y = .data$box), 
              size = 3, hjust = -.1, vjust = -0.2) +
    geom_text(aes(label = paste0(.data$CumQuant, " @$", formatNum(.data$CumValue/.data$CumQuant, 1)), 
                  y = .data$box), size = 2, hjust = -.1, vjust = 1.5) +
    geom_text(aes(label = paste0("$", formatNum(.data$CumInvested,1)), 
                  y = 0, colour = .data$Symbol), 
              size = 2, hjust = 0, vjust = -0.2) +
    geom_text(aes(label = paste0("@$", formatNum(.data$CumInvested/.data$CumQuant, 1)), 
                  y = 0, x = .data$Symbol, colour = .data$Symbol), 
              size = 2, hjust = 0, vjust = 1.5) +
    annotate("label", x = length(unique(today$CumQuant)) * 0.25, y = tops * 0.6, 
             label = vector2text(summary,"\n", quotes = F), size = 3.5, hjust = 0, alpha = 0.55) +
    scale_y_continuous(limits = c(NA, tops*1.12), labels = comma, expand = c(0, 0)) + 
    labs(y = NULL, x = NULL, title = "Stocks Distribution and Growth") +
    guides(fill = FALSE, colour = FALSE) + coord_flip() +
    theme_lares2(pal = 1)
  
  if (save) plot <- plot + ggsave("portf_stocks_change.png", width = 8, height = 8, dpi = 300)
  
  return(plot)
  
}


####################################################################
#' Portfolio Plots: Daily ROI
#' 
#' This function plots a portfolio's historical ROI since inception
#' or since last n days, with 2 moving average lines.
#' 
#' @family Investment
#' @family Investment Plots
#' @param p Dataframe. Result from \code{daily_portfolio()}
#' @param n_days Integer. How many days back you want to see?
#' @param historical Boolean. Historical ROI metric? If not, ROI
#' will be calculated locally for n_days parameter
#' @param ma Numeric Vector. Select 2 values for moving averages. 
#' Set to NA to turn this metric off
#' @param save Boolean. Save plot into a local file?
#' @export
splot_roi <- function(p, n_days = 365, historical = TRUE, ma = c(12, 50), save = FALSE) {
  
  check_attr(p, check = "daily_portfolio")
  
  if (n_days > nrow(p)) {
    message(sprintf("As there is less data than for %s days, changed n_days all days: %s",
                    n_days, nrow(p)))
    n_days <- nrow(p)
  }
  
  n_days <- ifelse(n_days < max(ma), max(ma) + 2, n_days)
  newp <- rbind(p, mutate(tail(p, 1), Date = .data$Date - 1, ROI = 0, CumValue = 0))
  
  caption <- ifelse(is.na(ma)[1], "", 
                    paste("Showing moving averages for", ma[1], "and", ma[2], "days."))
  
  if (!historical) newp <- mutate(newp, ROI = .data$ROI - newp$ROI[n_days])
  if (!is.na(n_days)) {
    newp <- slice(newp, 1:n_days)
    mas <- function(x, n = 30){stats::filter(x, rep(1 / n, n), sides = 2)}
    if (n_days != nrow(p))
      caption <- paste(caption, 
                       "\nShowing last", n_days, "days,",
                       ifelse(historical, "with historical ROI results.",
                              "with relative ROI results."))
  } 
  
  if (!is.na(ma)[1]) newp <- newp %>%
    mutate(ma1 = mas(.data$ROI, ma[1]), 
           ma2 = mas(.data$ROI, ma[2]))
  
  pos <- mutate(newp, ROI = ifelse(.data$ROI >= 0, .data$ROI, 0.0001))
  neg <- mutate(newp, ROI = ifelse(.data$ROI < 0, .data$ROI, -0.0001))
  
  plot <- ggplot(newp, aes(x = .data$Date)) +
    
    geom_hline(yintercept = 0, size = 0.3, color = "black") +
    geom_hline(yintercept = newp$ROI[1], alpha = 0.9, colour = "#40A4D8") +
    geom_hline(yintercept = max(newp$ROI), size = 0.2, 
               linetype = "dashed", colour = "#3DA4AB") +
    geom_hline(yintercept = min(newp$ROI), size = 0.2, 
               linetype = "dashed", colour = "tomato") +
    
    geom_area(data = pos, aes(y = .data$ROI), fill = "#3DA4AB", alpha = 0.3) +
    geom_area(data = neg, aes(y = .data$ROI), fill = "tomato", alpha = 0.3) +
    
    geom_line(aes(y = .data$ROI), size = 0.5) +
    
    scale_x_date(date_labels = "%b%y") + 
    labs(y = 'ROI [%]', x = NULL,
         title = 'Portfolio\'s Daily Growth (%)',
         subtitle = paste0(newp$Date[1], " | ROI: ",
                           formatNum(newp$ROI[1],2),"% ($",
                           formatNum(newp$CumValue[1] - newp$CumInvested[1], 0),
                           ") | Stocks Value: $", formatNum(newp$CumValue[1],0),
                           " | Portfolio: $", formatNum(newp$Portfolio[1],0)),
         caption = if (!is.na(ma)[1]) caption) +
    theme_lares2(legend = "top")
  
  if (!is.na(ma)[1]) plot <- plot + 
    geom_line(aes(y = .data$ma1), size = 0.7, 
              colour = "#071D49", alpha = 0.9, na.rm = TRUE) +
    geom_line(aes(y = .data$ma2), size = 0.7, 
              colour = "darkorange", alpha = 0.9, na.rm = TRUE) +
    
    if (save) plot <- plot + 
    ggsave("portf_daily_change.png", width = 8, height = 5, dpi = 300)
  
  return(plot)
  
}


####################################################################
#' Portfolio Plots: Daily Change
#' 
#' This function plots each stock's change through history, since
#' inception, with weighted attributions or absolute values.
#' 
#' @family Investment
#' @family Investment Plots
#' @param p Dataframe. Result from \code{daily_portfolio()}
#' @param s Dataframe. Result from \code{daily_stocks()}
#' @param weighted Boolean. Should variation values be weighted to the
#' portfolio (or simply compared with value since inception)?
#' @param group Boolean. Group stocks by stocks type?
#' @param n_days Integer. How many days back you want to see?
#' @param keep_old Boolean. Plot tickers that were already 
#' sold entirely?
#' @param save Boolean. Save plot into a local file?
#' @export
splot_change <- function(p, s, weighted = TRUE, group = FALSE, 
                         n_days = 365, keep_old = TRUE,
                         save = FALSE) {
  
  try_require("ggrepel")
  
  check_attr(p, check = "daily_portfolio")
  check_attr(s, check = "daily_stocks")
  
  current <- s[s$Date == max(s$Date),]
  current_stocks <- as.character(current$Symbol[current$CumValue > 0])
  
  d <- s %>% 
    {if (!keep_old) filter(., as.character(.data$Symbol) %in% current_stocks) else .} %>%
    arrange(.data$Date) %>% 
    group_by(.data$Symbol) %>%
    mutate(Hist = if (weighted) {100*(1 - cumsum(.data$Invested)/(.data$CumValue))} 
           else {.data$CumValue - .data$CumInvested},
           BuySell = ifelse(.data$Invested > 0, "Bought", ifelse(.data$Invested < 0, "Sold", NA))) %>%
    filter(!is.infinite(.data$Hist) & !is.na(.data$Hist)) %>%
    mutate(Hist = ifelse(.data$CumQuant > 0, .data$Hist, tail(.data$Hist, 1))) %>%
    {if (!is.na(n_days)) filter(., .data$Date >= max(s$Date) - n_days) else .} %>%
    filter(.data$CumValue > 0)
  
  d$Symbol <- factor(d$Symbol, levels = current$Symbol)
  labels <- group_by(d, .data$Symbol) %>% filter(.data$Date == max(.data$Date))
  amounts <- filter(d, .data$Invested != 0) %>%
    mutate(label = paste0(round(.data$Invested/1000,1),"K"))
  #mutate(label = formatNum(.data$Invested, abbr = TRUE))
  days <- as.integer(difftime(range(d$Date)[2], range(d$Date)[1], units = "days"))
  
  plot <- ggplot(d, aes(x = .data$Date, 
                        y = .data$Hist, 
                        colour = .data$Symbol)) +
    geom_vline(xintercept = max(s$Date), linetype = "dashed", alpha = 0.7) +
    ylab(glued('Total changed since start [{x}]', x = ifelse(weighted, "%", "$"))) +
    geom_hline(yintercept = 0, alpha = 0.8, colour = "black") +
    geom_line(alpha = 0.9, size = 0.5, na.rm = TRUE) +
    geom_point(aes(size = abs(.data$Invested), colour = .data$BuySell), alpha = 0.6, na.rm = TRUE) +
    scale_y_comma(position = "right") +
    scale_size(range = c(0, 3.2)) + guides(size = FALSE, colour = FALSE) + 
    xlim(min(d$Date), max(d$Date) + round(days*0.08)) +
    labs(title = glued(
      "Stocks Daily Change [{x}] since Start", x = ifelse(weighted, "%", "$")), 
      x = NULL, colour = NULL,
      subtitle = 'Showing absolute delta values since first purchase') +
    geom_label_repel(data = amounts, aes(label = .data$label), 
                     size = 2, na.rm = TRUE, alpha = 0.9, min.segment.length = 1.2) +
    geom_label_repel(data = labels, aes(label = .data$Symbol), 
                     size = 2.5, hjust = -0.2, alpha = 0.6, na.rm = TRUE) +
    theme_lares2(pal = 2) +
  
  if (group) plot <- plot + facet_grid(.data$Type ~ ., scales = "free", switch = "both")
  if (weighted) plot <- plot + 
    labs(subtitle = "Showing real weighted portfolio delta values")
  if (!is.na(n_days)) plot <- plot + 
    labs(caption = glued("Showing last {n_days} days"))

  if (save) plot <- plot + ggsave("portf_stocks_histchange.png", width = 8, height = 5, dpi = 300) 
  
  return(plot)
  
}


####################################################################
#' Portfolio Plots: Growth (Cash + Invested)
#' 
#' This function plots your portfolio's growth, in cash and investment,
#' since inception.
#' 
#' @family Investment
#' @family Investment Plots
#' @param p Dataframe. Result from \code{daily_portfolio()}
#' @param save Boolean. Save plot into a local file?
#' @export
splot_growth <- function(p, save = FALSE) {
  
  check_attr(p, check = "daily_portfolio")
  
  labels <- filter(p, .data$Cash != 0)
  caption <- paste0("Total Portfolio: $", formatNum(p$Portfolio[1], 0),
                    "\nInvested: $", formatNum(p$CumValue[1], 0), " (",
                    round(100*p$CumValue[1]/p$Portfolio[1],1),"%)")
  
  aux <- rbind(data.frame(Date = p$Date, Amount = p$CumCash, Type = "Cash"),
               data.frame(Date = p$Date, Amount = p$CumValue, Type = "Stocks"))
  
  plot <- ggplot(aux, aes(x = .data$Date, y = .data$Amount)) + 
    geom_area(aes(y = .data$Amount, fill = .data$Type), position = "stack") +
    labs(title = "  Daily Total Portfolio Value", y = NULL, x = NULL, fill = "") +
    scale_y_continuous(position = "right", labels = comma) +
    scale_x_date(date_labels = "%b%y", expand = c(0, 0)) +
    annotate("text", label = caption, x = max(p$Date), 
             y = 0.09*max(p$Portfolio), 
             size = 3.3, colour = "white", hjust = 1.1) +
    theme_lares2(pal = 1) +
    theme(legend.position = "top", legend.justification = c(0, 1))
  
  if (nrow(labels) > 0) {
    try_require("ggrepel")
    plot <- plot +
      geom_label_repel(data = labels, 
                       aes(x = .data$Date, y = .data$Portfolio, 
                           label = formatNum(.data$Cash, 0)), 
                       vjust = -1.3, size = 2.5)
  }
  
  if (save) plot <- plot + 
    ggsave("portf_total_hist.png", width = 8, height = 5, dpi = 300)
  
  return(plot)
  
}


####################################################################
#' Portfolio Plots: Types of Stocks
#' 
#' This function lets the user plot types or categories of tickers.
#' 
#' @family Investment
#' @family Investment Plots
#' @param s Dataframe. Result from daily_stocks()
#' @param save Boolean. Save plot into a local file?
#' @export
splot_types <- function(s, save = FALSE) {
  
  check_attr(s, check = "daily_stocks")
  
  plot <- s %>%
    filter(.data$Date == max(.data$Date), .data$CumQuant > 0) %>%
    group_by(.data$Type) %>% 
    mutate(label = paste0(.data$Type, "\n", formatNum(
      100*sum(.data$CumValue)/sum(s$CumValue[s$Date == max(s$Date)])),"%")) %>%
    ggplot() +
    geom_bar(aes(x = "", y = .data$CumValue, fill = .data$Symbol), 
             width = 1, stat = "identity") +
    facet_grid(. ~ label, scales = "free") +
    scale_y_continuous(labels = comma, expand = c(0, 0)) + 
    theme_lares2(pal = 1) +
    labs(x = NULL, y = "Total value", title = "Portfolio's Category Distribution")
  if (save) plot <- plot + ggsave("portf_distribution.png", width = 8, height = 5, dpi = 300) 
  return(plot)
}


####################################################################
#' ETF's Sectors Breakdown
#' 
#' This function scraps etf.com data for sector breakdown on ETFs.
#' Use \code{splot_etf()} for visualization.
#' 
#' @family Investment
#' @param etf Character Vector. Which ETFs you wish to scrap?
#' @param quiet Boolean. Print results and progress while downloading?
#' @export
etf_sector <- function(etf = "VTI", quiet = FALSE) {
  ret <- data.frame()
  for (i in 1:length(etf)) {
    info <- toupper(etf[i])
    url <- paste0("https://www.etf.com/", info)
    exists <- tryCatch({!httr::http_error(url)}, error = function(err) {FALSE})
    if (exists) {
      sector <- read_html(url)
      txt <- sector %>% 
        html_nodes(".pl0") %>% 
        html_text(trim = TRUE) %>% 
        .[grepl("Sector/Industry Breakdown", .)] %>%
        str_replace_all("[\r\n]" , " ") %>%
        gsub(".*Benchmark  ", "", .) %>%
        gsub("%", "", .) %>%
        str_split("  ") %>% 
        unlist() %>% .[. != ""]
      if (!is.null(txt[1])) {
        sector <- as.data.frame(matrix(txt, ncol = 3, byrow = TRUE))[,c(1:2)]
        colnames(sector) <- c("Sector", "Percentage")
        sector$Percentage <- as.numeric(as.character(sector$Percentage))
        sector$ETF <- info
        ret <- rbind(ret, sector)
        check <- TRUE 
      }
    } else {
      check <- FALSE
      Sys.sleep(1)
    }
    if (!quiet & length(etf) > 1)
      statusbar(i, length(etf), info)   
  }
  
  if (nrow(ret) == 0) {
    message("No data found for given Tickers!")
    invisible(return(NULL))
  } else {
    return(ret) 
  }
}


####################################################################
#' Portfolio's Sector Distribution (ETFs)
#' 
#' This function lets the user plot his portfolio's distribution, 
#' specifically ETF's sectors
#' 
#' @family Investment
#' @family Investment Plots
#' @family Scrapper
#' @param s Dataframe. Result from \code{daily_stocks()}.
#' @param keep_all Boolean. Keep "Not Known / Not ETF"?
#' @param save Boolean. Save plot into a local file?
#' @export
splot_etf <- function(s, keep_all = FALSE, save = FALSE) {
  
  check_attr(s, check = "daily_stocks")
  
  if (!"Date" %in% colnames(s)) s$Date <- Sys.Date()
  
  message(">>> Downloading ETF's sectors...")
  etfs <- etf_sector(unique(s$Symbol[s$Date == max(s$Date)]))
  
  if (nrow(etfs) > 0) {
    df <- etfs %>% 
      right_join(select(s, .data$Symbol, .data$CumValue, .data$Date) %>% 
                   filter(.data$Date == max(.data$Date)) %>%
                   mutate(Symbol = as.character(.data$Symbol)), 
                 by = c("ETF" = "Symbol")) %>%
      mutate(ETF = factor(.data$ETF, levels = s$Symbol[s$Date == max(s$Date)]),
             Sector = ifelse(is.na(.data$Sector), 
                             "Not Known / Not ETF", 
                             as.character(.data$Sector))) %>%
      replace(., is.na(.), 100) %>%
      mutate(Value = .data$CumValue * .data$Percentage / 100) %>%
      group_by(.data$Sector) %>% 
      mutate(ValueSector = sum(.data$Value)) %>% 
      ungroup() %>% 
      group_by(.data$Sector) %>% 
      mutate(label = paste0(
        .data$Sector, " (", 
        formatNum(100*.data$ValueSector/sum(s$CumValue[s$Date == max(s$Date)]), 1), "%)"))
    if (keep_all == FALSE)
      df <- filter(df, .data$ETF != "Not Known / Not ETF")
    
    plot <- ggplot(df, aes(x = reorder(.data$label, .data$ValueSector), 
                           y = .data$Value, fill = .data$ETF)) +
      geom_bar(width = 1, stat = "identity") +
      coord_flip() +
      scale_y_continuous(labels = comma, expand = c(0, 0)) + 
      theme_lares2(pal = 1) +
      labs(x = NULL, y = "Total value", fill = NULL,
           title = "Portfolio's Sector Distribution (ETFs)")
    
    if (save) plot <- plot + ggsave("portf_distribution_etfs.png", width = 8, height = 5, dpi = 300) 
    return(plot) 
  } else return(noPlot("No data here!"))
}


####################################################################
#' Portfolio's Calculations and Plots
#' 
#' This function lets the user create his portfolio's calculations and
#' plots for further study.
#' 
#' @family Investment
#' @param data List. Containing the following dataframes: portfolio,
#' transactions, cash. They have to follow the original xlsx format
#' @param cash_fix Numeric. If you wish to algebraically sum a value 
#' to your cash balance
#' @param tax Numeric. How much [0-99] of your dividends are gone with taxes? 
#' @param sectors Boolean. Return sectors segmentation for ETFs?
#' @param parg Boolean. Personal argument. Used to personalize stuff, in this
#' case, taxes changed from A to B in given date (hard-coded)
#' @export
stocks_obj <- function(data = stocks_file(), 
                       cash_fix = 0, 
                       tax = 30, 
                       sectors = FALSE,
                       parg = FALSE) {
  
  check_attr(data, check = "stocks_file")
  
  ret <- list()
  trans <- data$transactions
  cash <- data$cash
  tickers <- data$portfolio
  
  message(">>> Downloading historical data for each stock...")
  ret[["quotes"]] <- hist <- stocks_hist(
    symbols = tickers$Symbol, 
    from = tickers$StartDate, 
    tax = tax,
    parg = parg)
  
  # Objects needed for plots
  ret[["stocks"]] <- s <- daily_stocks(hist, trans, tickers)
  ret[["portfolio"]] <- p <- daily_portfolio(hist, trans, cash, cash_fix = cash_fix)
  message("Calculations ready...")
  
  # Visualizatoins
  plots <- list()
  plots[["roi1"]] <- splot_roi(p)
  plots[["roi2"]] <- splot_roi(p, 365, FALSE)
  plots[["growth"]] <- splot_growth(p)
  plots[["summary"]] <- splot_summary(p, s)
  plots[["change1"]] <- splot_change(p, s, TRUE)
  plots[["change2"]] <- splot_change(p, s, FALSE)
  plots[["types"]] <- splot_types(s)
  if (sectors) {
    message(">>> Downloading sectors for each ETF...")
    plots[["etfs"]] <- splot_etf(s)
  } 
  ret[["plots"]] <- plots
  message("Visualizations ready...")
  
  attr(ret, "type") <- "stocks_obj"
  return(ret)
}
#x <- stocks_obj()


####################################################################
#' Portfolio's Full Report and Email
#' 
#' This function lets the user create his portfolio's full report with 
#' plots and send it to an email with the HTML report attached
#' 
#' @family Investment
#' @family Credentials
#' @param data Character. \code{stocks_obj()} output. If NA, automatic 
#' parameters and \code{stocks_file()} defaults will be used.
#' @param dir Character. Directory for HTML report output. If set to NA, 
#' current working directory will be used. If mail sent, file will be erased
#' @param mail Boolean. Do you want to send an email with the report attached? 
#' If not, an HTML file will be created in dir
#' @param to Character. Email to send the report to
#' @param sectors Boolean. Return sectors segmentation for ETFs?
#' @param keep Boolean. Keep HTML file when sended by email?
#' @param creds Character. Credential's user (see \code{get_creds()}) for 
#' sending mail and Dropbox interaction
#' @examples
#' \dontrun{
#' list <- stocks_obj()
#' stocks_report(list, dir = "~/Desktop")
#' }
#' @export
stocks_report <- function(data = NA,
                          dir = NA,
                          mail = FALSE, 
                          to = "laresbernardo@gmail.com",
                          sectors = FALSE,
                          keep = FALSE,
                          creds = NA) {
  
  try_require("rmarkdown")
  
  if (is.na(data)[1]) {
    df <- stocks_file(creds = creds) 
    data <- stocks_obj(df, sectors = sectors, parg = is.na(creds))
  }
  
  check_attr(data, check = c("stocks_obj"))
  
  pandoc <- Sys.getenv("RSTUDIO_PANDOC")
  Sys.setenv(RSTUDIO_PANDOC = pandoc)
  if (is.na(dir)) dir <- getwd()
  
  # Can be more accurate with names but works for me!
  params <- list(
    portf_daily_change = data$plots[["roi1"]],
    portf_stocks_change = data$plots[["summary"]],
    portf_stocks_histchange_weighted = data$plots[["change1"]],
    portf_stocks_histchange_absolute = data$plots[["change2"]],
    portf_distribution = data$plots[["growth"]],
    portf_daily = data$plots[["roi2"]],
    portfolio_perf = data$plots[["types"]])
  if ("etfs" %in% names(data$plots)) 
    params[["portf_distribution_sectors"]] <- data$plots[["etfs"]]
  
  message(">>> Rendering HTML report...")
  html_file <- "stocksReport.html"
  render(system.file("docs", "stocksReport.Rmd", package = "lares"), 
         output_file = html_file,
         output_dir = dir,
         params = params,
         envir = new.env(parent = globalenv()),
         quiet = TRUE)  
  message("HTML report created succesfully!")
  
  if (mail) {
    # Set token for mail and dropbox credentials
    creds <- case_when(
      is.na(creds) ~ "~/Dropbox (Personal)/Documentos/Docs/Data",
      creds %in% c("matrix","server") ~ "~/creds",
      TRUE ~ as.character(creds))
    
    message(">>> Sending email...")
    mailSend(to = to, text = " \n", 
             subject = paste("Portfolio:", max(data$portfolio$Date)),
             attachment = paste0(getwd(), html_file),
             creds = creds,
             quiet = FALSE) 
    if (!keep) invisible(file.remove(paste0(getwd(), html_file)))
  }
}

# # TESTING
# library(lares)
# library(tidyverse)
# library(openxlsx)
# library(quantmod)
# library(jsonlite)
# library(lubridate)
# library(rvest)
# library(scales)
# library(httr)
# library(rdrop2)
# x <- stocks_obj()
# stocks_report(x, dir = "~/Desktop")
