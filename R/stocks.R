####################################################################
#' Get Personal Portfolio's Data
#' 
#' This function lets me download my personal Excel with my Portfolio 
#' data, locally or from Dropbox. You can also use it for local files
#' 
#' @family Investment
#' @param filename Characeter. Import a local Excel file
#' @param creds Character. Where is my personal token for Dropbox connection?
#' @param auto Boolean. Automatically user my local personal file? 
#' @param sheets Character Vector. Names of each sheet containing Portfolio summary,
#' Cash, and Transactions information
#' @param keep_old Boolean. Include sold tickers eventhough not currently in portfolio?
#' @export
stocks_file <- function(filename = NA, creds = "~/Dropbox (Personal)/Documentos/Docs/Data", 
                        auto = TRUE, sheets = c("Portafolio","Fondos","Transacciones"),
                        keep_old = TRUE) {
  
  processFile <- function(file, keep_old = TRUE) {
    port <- read.xlsx(file, sheet = sheets[1], skipEmptyRows = TRUE, detectDates = TRUE)
    cash <- read.xlsx(file, sheet = sheets[2], skipEmptyRows = TRUE, detectDates = TRUE)
    trans <- read.xlsx(file, sheet = sheets[3], skipEmptyRows = TRUE, detectDates = TRUE)
    if ("Value" %in% colnames(trans)) trans <- rename(trans, Each = Value, Invested = Amount)
    if (!keep_old) port <- port[port$Stocks != 0,]
    mylist <- list("portfolio" = port, "transactions" = trans, "cash" = cash)
    return(mylist)
  }
  
  # FOR PERSONAL USE
  local <- Sys.info()
  if (local[["nodename"]] == "MacBook-Pro-de-Bernardo.local" & auto == TRUE) {
    message("Using BL's local file...")
    local <- "~/Dropbox (Personal)/Documentos/Interactive Brokers/Portfolio/Portfolio LC.xlsx"
    results <- processFile(local, keep_old) 
  } else {
    # FOR EVERYONE'S USE
    if (!is.na(filename)) {
      if (file.exists(filename)) results <- processFile(filename, keep_old) else
        stop("Error: that file doesn't exist or it's not in your working directory!")
    } else {
      # FOR DROPBOX'S USE
      creds <- creds
      load(paste0(creds, "/token_pers.rds"))
      x <- drop_search("Portfolio LC.xlsx", dtoken = token)
      file <- "temp.xlsx"
      invisible(
        drop_download(x$matches[[1]]$metadata$path_lower,
                      local_path = file,
                      overwrite = TRUE,
                      dtoken = token))
      results <- processFile(file, keep_old)
      file.remove(file)
    } 
  }
  message("File imported succesfully!")
  return(results)   
}


####################################################################
#' Download Stocks Historical Data
#' 
#' This function lets the user download stocks historical data
#' 
#' @family Investment
#' @param symbols Character Vector. List of symbols to download historical data
#' @param from Date. Since when do you wish to download historical data? 
#' If not set, will return since 365 days ago
#' @param tax Numeric. How much [0-99] of your dividends are gone with taxes? 
#' @param verbose Boolean. Print results and progress while downloading?
#' @export
stocks_hist <- function(symbols = c("VTI","IEMG"), 
                        from = Sys.Date() - 365, 
                        tax = 30, 
                        verbose = TRUE) {
  
  if (!haveInternet()) stop("You currently have NO internet connection!")
  
  options("getSymbols.warning4.0" = FALSE)
  options("getSymbols.yahoo.warning" = FALSE)
  data <- divs <- c()
  
  if (!any(is.na(symbols))) {
    if (length(from) != length(symbols)) from <- rep(from[1], length(symbols))
    
    for (i in 1:length(symbols)) {
      # Daily quotes
      symbol <- as.character(symbols[i])
      start_date <- as.character(from[i])
      values <- getSymbols(symbol, env = NULL, from = start_date, src = "yahoo") %>% data.frame()
      values <- cbind(row.names(values), as.character(symbol), values)
      colnames(values) <- c("Date","Symbol","Open","High","Low","Close","Volume","Adjusted")
      values <- mutate(values, Adjusted = rowMeans(select(values, High, Close), na.rm = TRUE))
      row.names(values) <- NULL
      data <- rbind(data, values)
      
      # Dividends
      d <- getDividends(as.character(symbol), from = start_date)
      if (nrow(d) > 0) {
        div <-  data.frame(Symbol = rep(symbol, nrow(d)),
                           Date = ymd(row.names(data.frame(d))),
                           Div = as.vector(d),
                           DivReal = as.vector(d)*(100 - tax)/100)
        divs <- rbind(divs, div)
      }
      if (verbose) {
        info <- paste(symbol, "since", start_date, "   ")
        statusbar(i, length(symbols), info)  
      }
    }
  } else {message("You need to define which stocks to bring. Use the 'symbols=' parameter.") }
  
  results <- data %>% 
    select(Date, Symbol, Adjusted) %>% rename(Value = Adjusted) %>%
    mutate(Date = as.Date(Date), Symbol = as.character(Symbol)) %>%
    left_join(mutate(divs, Symbol = as.character(Symbol)), by = c("Date", "Symbol")) %>% 
    replace(is.na(.), 0) %>%
    arrange(desc(Date))
  return(results)
}


####################################################################
#' Daily Stocks Dataframe
#' 
#' This function creates a dataframe will all relevant metrics and values,
#' for each ticker or symbol, for every day since inception.
#' 
#' @family Investment
#' @param hist Dataframe. Result from stocks_hist()
#' @param trans Dataframe. Result from stocks_file()$transactions
#' @param tickers Dataframe. Result from stocks_file()$portfolio
#' @export
daily_stocks <- function(hist, trans, tickers = NA) {
  
  hist_structure <- c("Date", "Symbol", "Value", "Div", "DivReal")
  trans_structure <- c("Symbol", "Date", "Quant", "Each", "Invested", "Cost")
  if (!all(hist_structure %in% colnames(hist))) {
    stop(paste("The structure of the 'hist' table should be:",
               paste(shQuote(hist_structure), collapse = ", ")))}
  if (!all(trans_structure %in% colnames(trans))) {
    stop(paste("The structure of the 'trans' table should be:",
               paste(shQuote(trans_structure), collapse = ", ")))}
  
  daily <- hist %>%
    left_join(trans, by = c("Date", "Symbol")) %>%
    replace(is.na(.), 0) %>%
    arrange(Date) %>%
    group_by(Symbol) %>%
    mutate(CumQuant = cumsum(Quant),
           CumInvested = cumsum(Invested),
           CumCost = cumsum(Cost),
           CumValue = CumQuant * Value,
           CumROI = 100 * (CumValue/CumInvested - 1),
           CumROI = as.numeric(ifelse(
             CumValue == 0, 100*(Each*abs(Quant)/lag(CumInvested) - 1), CumROI)),
           Dividend = DivReal * CumQuant,
           DifUSD = CumValue - Invested - lag(CumValue),
           CumDividend = cumsum(Dividend)) %>% 
    select(Date, Symbol, Value, Quant, Each, Invested, Cost, Dividend, 
           CumValue, CumInvested, CumROI, CumQuant, DifUSD, CumDividend, CumCost) %>%
    group_by(Date) %>% mutate(Weight = 100*CumValue/sum(CumValue)) %>% 
    group_by(Date, Symbol) %>% slice(1) %>%
    arrange(desc(Date), desc(CumValue)) %>% ungroup()
  
  if ("Type" %in% colnames(tickers)) {
    tickers_structure <- c("Symbol", "Type")
    if (!all(tickers_structure %in% colnames(tickers))) {
      stop(paste("The structure of the 'tickers' table should be:",
                 paste(shQuote(tickers_structure), collapse = ", ")))}
    daily <- left_join(daily, select(tickers, Symbol, Type), "Symbol")
  } else daily$Type <- "No type"
  
  daily$Symbol <- factor(daily$Symbol, levels = unique(daily$Symbol[daily$Date == max(daily$Date)]))
  
  return(daily)
  
}


####################################################################
#' Daily Portfolio Dataframe
#' 
#' This function creates a dataframe will all relevant metrics and values,
#' for the overall portfolio, for every day since inception.
#' 
#' @family Investment
#' @param hist Dataframe. Result from stocks_hist()
#' @param trans Dataframe. Result from stocks_file()$transactions
#' @param cash Dataframe. Result from stocks_file()$cash
#' @param cash_fix Numeric. If, for some reason, you need to fix your 
#' cash amount for all reports, set the amount here
#' @export
daily_portfolio <- function(hist, trans, cash, cash_fix = 0) {
  
  cash_structure <- c("Date", "Cash")
  if (!all(cash_structure %in% colnames(cash))) {
    stop(paste("The structure of the 'cash' table should be:",
               paste(shQuote(cash_structure), collapse = ", ")))}
  
  daily <- daily_stocks(hist, trans) %>%
    arrange(Date) %>% group_by(Date) %>% 
    summarise_if(is.numeric, sum) %>%
    mutate(ROI = 100 * (CumValue/CumInvested - 1)) %>%
    select(Date, CumInvested, CumValue, ROI, Invested, CumCost, CumDividend, Dividend) 
  
  days <- data.frame(Date = as.Date(min(hist$Date):Sys.Date(), origin = "1970-01-01")) %>%
    left_join(daily, "Date") %>%
    tidyr::fill(ROI, CumInvested, CumValue, CumDividend, CumCost, Invested, .direction = "up") %>%
    left_join(select(cash, Date, Cash), "Date") %>% 
    replace(is.na(.), 0) %>%    
    mutate(DifUSD = CumValue - Invested - lag(CumValue),
           Cash = as.numeric(ifelse(is.na(Cash), 0, Cash)),
           CumCash = round(cumsum(Cash) + cumsum(Dividend) - CumInvested - CumCost + cash_fix),
           Portfolio = CumCash + CumValue) %>% 
    filter(CumInvested != 0) %>%
    arrange(desc(Date)) %>% ungroup()
  
  return(days)
  
}

################# PLOTTING FUNCTIONS #################

####################################################################
#' Portfolio Plots: Total Summary
#' 
#' This function plots a summary for the whole portfolio, showing
#' how much have you invested, how much has each ticker changed, etc.
#' 
#' @family Investment
#' @param p Dataframe. Result from daily_portfolio()
#' @param s Dataframe. Result from daily_stocks()
#' @param save Boolean. Save plot into a local file?
#' @export
splot_summary <- function(p, s, save = FALSE) {
  
  today <- filter(p, Date == max(Date))
  summary <- rbind(
    paste0("Portfolio: $", formatNum(today$Portfolio, 0)," | ", today$Date),
    paste0("Stocks: $", formatNum(today$CumValue,0)," & Cash: $", formatNum(today$CumCash,0)),
    paste0("ROI: ", formatNum(today$ROI, 2),"% ($",
           formatNum(today$CumValue - today$CumInvested, 0),")"),
    paste0("Dividends: $", formatNum(today$CumDividend, 0)," | Expenses: $",
           formatNum(today$CumCost, 0)))
  
  today <- filter(s, Date == max(Date)) %>% filter(CumQuant > 0)
  tops <- max(rbind(today$CumInvested, today$CumValue))
  plot <- today %>%
    mutate(shapeflag = ifelse(CumROI < 0, 25, 24), 
           box = -tops/5.5,
           Symbol = factor(paste0(Symbol, " (", formatNum(100*CumValue/sum(CumValue)), "%)"),
                           levels = paste0(Symbol, " (", formatNum(100*CumValue/sum(CumValue)), "%)")),
           DifUSD = CumValue - CumInvested,
           DifPer = 100 * DifUSD / CumInvested) %>%
    ggplot(aes(x = reorder(Symbol, CumInvested))) + 
    geom_hline(yintercept = 0, colour = "black") +
    geom_col(aes(y = CumInvested, fill = Symbol, group = 1)) +
    geom_col(aes(y = CumInvested + DifUSD, fill = Symbol), alpha = 0.5) +
    geom_col(aes(y = box), fill = "grey", alpha = 0.5) +
    geom_point(aes(y = CumInvested + DifUSD, shape = shapeflag), colour = "black") +
    scale_shape_identity() +
    geom_text(aes(label = paste0("$",formatNum(DifUSD,0)), y = CumInvested + DifUSD), 
              size = 3, hjust = -.2, vjust = -0.2) +
    geom_text(aes(label = paste0(round(DifPer, 1), "%"), y = CumInvested + DifUSD), 
              size = 2.9, hjust = -.2, vjust = 1.2) +
    geom_text(aes(label = paste0("$", formatNum(CumValue, 0)), y = box), 
              size = 3, hjust = -.1, vjust = -0.2) +
    geom_text(aes(label = paste0(CumQuant, " @$", formatNum(CumValue/CumQuant, 1)), 
                  y = box), size = 2, hjust = -.1, vjust = 1.5) +
    geom_text(aes(label = paste0("$", formatNum(CumInvested,1)), y = 0, 
                  colour = Symbol), size = 2, hjust = 0, vjust = -0.2) +
    geom_text(aes(label = paste0("@$", formatNum(CumInvested/CumQuant, 1)), 
                  y = 0, x = Symbol, colour = Symbol), 
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
#' @param p Dataframe. Result from daily_portfolio()
#' @param n_days Integer. How many days back you want to see?
#' @param historical Boolean. Historical ROI metric? If not, ROI
#' will be calculated locally for n_days parameter
#' @param ma Numeric Vector. Select 2 values for moving averages. 
#' Set to NA to turn this metric off
#' @param save Boolean. Save plot into a local file?
#' @export
splot_roi <- function(p, n_days = 365, historical = TRUE, ma = c(12, 50), save = FALSE) {
  
  n_days <- ifelse(n_days < max(ma), max(ma) + 2, n_days)
  newp <- rbind(p, mutate(tail(p, 1), Date = Date - 1, ROI = 0, CumValue = 0))
  
  caption <- ifelse(is.na(ma)[1], "No caption needed", 
                    paste("Showing moving averages for", ma[1], "and", ma[2], "days."))
  
  if (!historical) newp <- mutate(newp, ROI = ROI - newp$ROI[n_days])
  if (!is.na(n_days)) {
    newp <- slice(newp, 1:n_days)
    mas <- function(x, n = 30){stats::filter(x, rep(1 / n, n), sides = 2)}
    caption <- paste(caption, 
                     "\nShowing last", n_days, "days,",
                     ifelse(historical, "with historical ROI results.",
                            "with relative ROI results."))
  } 
  
  if (!is.na(ma)[1]) newp <- mutate(newp, ma1 = mas(ROI, ma[1]), ma2 = mas(ROI, ma[2]))
  
  pos <- mutate(newp, ROI = ifelse(ROI >= 0, ROI, 0.0001))
  neg <- mutate(newp, ROI = ifelse(ROI < 0, ROI, -0.0001))
  
  plot <- ggplot(newp, aes(x = Date)) +
    
    geom_hline(yintercept = 0, size = 0.3, color = "black") +
    geom_hline(yintercept = newp$ROI[1], alpha = 0.9, colour = "#40A4D8") +
    geom_hline(yintercept = max(newp$ROI), size = 0.2, 
               linetype = "dashed", colour = "#3DA4AB") +
    geom_hline(yintercept = min(newp$ROI), size = 0.2, 
               linetype = "dashed", colour = "tomato") +
    
    geom_area(data = pos, aes(y = ROI), fill = "#3DA4AB", alpha = 0.3) +
    geom_area(data = neg, aes(y = ROI), fill = "tomato", alpha = 0.3) +
    
    geom_line(aes(y = ROI), size = 0.5) +
    
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
    geom_line(aes(y = ma1), size = 0.7, 
              colour = "#071D49", alpha = 0.9, na.rm = TRUE) +
    geom_line(aes(y = ma2), size = 0.7, 
              colour = "darkorange", alpha = 0.9, na.rm = TRUE) +
    
    if (save) plot <- plot + 
    ggsave("portf_daily_change.png", width = 8, height = 5, dpi = 300)
  
  return(plot)
  
}


####################################################################
#' Portfolio Plots: Daily Change
#' 
#' This function plots each stock's change throught history, since
#' inception, with weighted attributions or absolute values.
#' 
#' @family Investment
#' @param p Dataframe. Result from daily_portfolio()
#' @param s Dataframe. Result from daily_stocks()
#' @param weighted Boolean. Should variation values be weighted to the
#' portfolio (or simply compared with value since inception)?
#' @param group Boolean. Group stocks by stocks type?
#' @param save Boolean. Save plot into a local file?
#' @export
splot_change <- function(p, s, weighted = TRUE, group = TRUE, save = FALSE) {
  
  try_require("ggrepel")
  
  d <- s %>% 
    arrange(Date) %>% group_by(Symbol) %>%
    mutate(Hist = if (weighted) {100*(1 - cumsum(Invested)/(CumValue))} 
           else {CumValue - CumInvested},
           BuySell = ifelse(Invested > 0, "Bought", ifelse(Invested < 0, "Sold", NA))) %>%
    mutate(Hist = ifelse(CumQuant > 0, Hist, NA))
  d$Symbol <- factor(d$Symbol, levels = rev(unique(s$Symbol)))
  labels <- d %>% filter(Date == max(Date))
  amounts <- d %>% filter(Invested != 0) %>%
    mutate(label = paste0(round(Invested/1000,1),"K"))
  days <- as.integer(difftime(range(d$Date)[2], range(d$Date)[1], units = "days"))
  
  plot <- ggplot(d, aes(x = Date, y = Hist, colour = Symbol)) +
    ylab('% Change since Start') +
    geom_hline(yintercept = 0, alpha = 0.8, colour = "black") +
    geom_line(alpha = 0.9, size = 0.5, na.rm = TRUE) +
    geom_point(aes(size = abs(Invested), colour = BuySell), alpha = 0.6, na.rm = TRUE) +
    scale_y_continuous(position = "right") +
    scale_size(range = c(0, 3.2)) + guides(size = FALSE, colour = FALSE) + 
    xlim(min(d$Date), max(d$Date) + round(days*0.08)) +
    labs(title = 'Daily Portfolio\'s Stocks Change (%) since Start', x = '',
         subtitle = 'Showing absolute delta values since first purchase', colour = '') +
    geom_label_repel(data = amounts, aes(label = label), 
                     size = 2, na.rm = TRUE, alpha = 0.9, min.segment.length = 1.2) +
    geom_label(data = labels, aes(label = Symbol), 
               size = 2.5, hjust = -0.2, alpha = 0.6, na.rm = TRUE) +
    theme_lares2(pal = 2)
  
  if (group) plot <- plot + facet_grid(Type ~ ., scales = "free", switch = "both")
  if (weighted) plot <- plot + 
    labs(subtitle = "Showing real weighted portfolio delta values")
  if (save) plot <- plot + 
    ggsave("portf_stocks_histchange.png", width = 8, height = 5, dpi = 300) 
  
  return(plot)
  
}


####################################################################
#' Portfolio Plots: Growth (Cash + Invested)
#' 
#' This function plots your portfolio's growth, in cash and investment,
#' since inception.
#' 
#' @family Investment
#' @param p Dataframe. Result from daily_portfolio()
#' @param save Boolean. Save plot into a local file?
#' @export
splot_growth <- function(p, save = FALSE) {
  
  try_require("ggrepel")
  
  labels <- filter(p, Cash != 0)
  caption <- paste0("Total Portfolio: $", formatNum(p$Portfolio[1], 0),
                    "\nInvested: $", formatNum(p$CumValue[1], 0), " (",
                    round(100*p$CumValue[1]/p$Portfolio[1],1),"%)")
  
  aux <- rbind(data.frame(Date = p$Date, Amount = p$CumCash, Type = "Cash"),
               data.frame(Date = p$Date, Amount = p$CumValue, Type = "Stocks"))
  
  plot <- ggplot(aux, aes(x = Date, y = Amount)) + 
    geom_col(aes(y = Amount, fill = Type), position = "stack", width = 1) +
    labs(title = "  Daily Total Portfolio Value", y = NULL, x = NULL, fill = "") +
    geom_label_repel(data = labels, 
                     aes(x = Date, y = Portfolio, label = formatNum(Cash, 0)), 
                     vjust = -1.3, size = 2.5) +
    scale_y_continuous(position = "right", labels = comma) +
    scale_x_date(date_labels = "%b%y", expand = c(0, 0)) +
    annotate("text", label = caption, x = max(p$Date), 
             y = 0.09*max(p$Portfolio), 
             size = 3.3, colour = "white", hjust = 1.1) +
    theme_lares2(pal = 1) +
    theme(legend.position = "top", legend.justification = c(0, 1))
  
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
#' @param s Dataframe. Result from daily_stocks()
#' @param save Boolean. Save plot into a local file?
#' @export
splot_types <- function(s, save = FALSE) {
  
  plot <- s %>%
    filter(Date == max(Date), CumQuant > 0) %>%
    group_by(Type) %>% 
    mutate(label = paste0(Type, "\n", formatNum(
      100*sum(CumValue)/sum(s$CumValue[s$Date == max(s$Date)])),"%")) %>%
    ggplot() +
    geom_bar(aes(x = "", y = CumValue, fill = Symbol), width = 1, stat = "identity") +
    facet_grid(. ~ label, scales = "free") +
    scale_y_continuous(labels = comma, expand = c(0, 0)) + 
    theme_lares2(pal = 1) +
    labs(x = NULL, y = "Total value", title = "Portfolio's Category Distribution")
  if (save) plot <- plot + ggsave("portf_distribution.png", 
                                  width = 8, height = 5, dpi = 300) 
  return(plot)
}


####################################################################
#' ETF's Sectors Breakdown
#' 
#' This function scraps etf.com data for sector breakdown on ETFs.
#' Use splot_etf() for visualization.
#' 
#' @family Investment
#' @param etf Character Vector. Which ETFs you wish to scrap?
#' @param verbose Boolean. Print results and progress while downloading?
#' @export
etf_sector <- function(etf = "VTI", verbose = TRUE) {
  ret <- data.frame()
  for (i in 1:length(etf)) {
    url <- paste0("https://etfdb.com/etf/", toupper(etf[i]))
    if (RCurl::url.exists(url)) {
      sector <- read_html(url) %>% html_nodes(".col-md-6") %>% 
        html_text() %>% .[grepl("Sector Breakdown",.)] %>% .[1]
      sector <- data.frame(matrix(unlist(strsplit(sector, split = "\n"))[-c(1:5)], 
                                  ncol = 2, byrow = TRUE))
      colnames(sector) <- c("Sector", "Percentage")
      sector$Percentage <- as.integer(cleanText(sector$Percentage))/100 
      sector$ETF <- toupper(etf[i])
      sector <- sector %>% select(ETF, Sector, Percentage)
      ret <- rbind(ret, sector)
      check <- TRUE
    } else {
      check <- FALSE
      Sys.sleep(1)
    }
    if (verbose & length(etf) > 1) {
      info <- paste(toupper(etf[i]), ifelse(check, "", "X"))
      statusbar(i, length(etf), info)   
    }
  }
  
  if (nrow(ret) == 0) {
    message("No data found for given Tickers!")
    invisible(return())
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
#' @param s Dataframe. Result from daily_stocks()
#' @param save Boolean. Save plot into a local file?
#' @export
splot_etf <- function(s, save = FALSE) {
  
  structure <- c("Symbol", "CumValue")
  if (!all(structure %in% colnames(s))) {
    stop(paste("s should contain all of the following:",
               paste(shQuote(s), collapse = ", ")))}
  if (!"Date" %in% colnames(s)) s$Date <- Sys.Date()
  
  message(">>> Downloading ETF's sectors...")
  etfs <- etf_sector(unique(s$Symbol[s$Date == max(s$Date)]))
  
  if (nrow(etfs) > 0) {
    df <- etfs %>% 
      right_join(select(s, Symbol, CumValue, Date) %>% 
                   filter(Date == max(Date)) %>%
                   mutate(Symbol = as.character(Symbol)), 
                 by = c("ETF" = "Symbol")) %>%
      mutate(ETF = factor(ETF, levels = s$Symbol[s$Date == max(s$Date)]),
             Sector = ifelse(is.na(Sector), "Not Known / Not ETF", as.character(Sector))) %>%
      replace(., is.na(.), 100) %>%
      mutate(Value = CumValue * Percentage / 100) %>%
      group_by(Sector) %>% mutate(ValueSector = sum(Value)) %>% ungroup() %>% 
      group_by(Sector) %>% mutate(label = paste0(
        Sector, " (", formatNum(100*ValueSector/sum(s$CumValue[s$Date == max(s$Date)]), 1), "%)"))
    
    plot <- ggplot(df, aes(x = reorder(label, ValueSector), y = Value, fill = ETF)) +
      geom_bar(width = 1, stat = "identity") +
      coord_flip() +
      scale_y_continuous(labels = comma, expand = c(0, 0)) + 
      theme_lares2(pal = 1) +
      labs(x = NULL, y = "Total value", fill = NULL,
           title = "Portfolio's Sector Distribution (ETFs)")
    
    if (save) plot <- plot + ggsave("portf_distribution_etfs.png", 
                                    width = 8, height = 5, dpi = 300) 
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
#' @export
stocks_obj <- function(data = stocks_file(), 
                       cash_fix = 0, tax = 30, 
                       sectors = TRUE) {

  ret <- list()
  trans <- data$transactions
  cash <- data$cash
  tickers <- data$portfolio
  
  message(">>> Downloading historical data for each stock...")
  ret[["quotes"]] <- hist <- stocks_hist(
    symbols = tickers$Symbol, from = tickers$StartDate, tax = tax)
  
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
  if (sectors == TRUE) plots[["etfs"]] <- splot_etf(s)
  ret[["plots"]] <- plots
  message("Visualizations ready...")
  
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
#' @param data Character. stocks_file() output. If NA, automatic report
#' parameters will be used
#' @param dir Character. Directory for HTML report output. If set to NA, 
#' current working directory will be used. If mail sent, file will be erased
#' @param mail Boolean. Do you want to send an email with the report attached? 
#' If not, an HTML file will be created in dir
#' @param to Character. Email to send the report to
#' @param creds Character. Credential's user (see get_credentials) for 
#' sending mail and Dropbox interaction
#' @export
stocks_report <- function(data = NA,
                          dir = NA,
                          mail = FALSE, 
                          to = "laresbernardo@gmail.com",
                          creds = NA) {
  if (is.na(data))
    data <- stocks_file(creds = creds)
  
  data <- stocks_obj(data)
  
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
  
  invisible(file.copy(
    from = system.file("docs", "stocksReport.Rmd", package = "lares"),
    to = getwd(), overwrite = TRUE, recursive = FALSE, copy.mode = TRUE))
  
  message(">>> Rendering HTML report...")
  rmarkdown::render("stocksReport.Rmd", 
                    output_file = "stocksReport.html",
                    output_dir = dir,
                    params = params,
                    envir = new.env(parent = globalenv()),
                    quiet = TRUE)  
  
  invisible(file.remove(paste0(getwd(), "/stocksReport.Rmd")))
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
             attachment = paste0(getwd(), "/stocksReport.html"),
             creds = creds,
             quiet = FALSE) 
    invisible(file.remove(paste0(getwd(), "/stocksReport.html")))
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
# x <- stocks_obj(sectors = FALSE)
# s <- x$stocks
# p <- x$portfolio
# stocks_report(x, dir = "~/Desktop")
