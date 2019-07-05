####################################################################
#' Get Personal Portfolio's Data
#' 
#' This function lets me download my personal Excel with my Portfolio data
#' 
#' @family Investment
#' @param filename Characeter. Import a local Excel file
#' @param token_dir Character. Where is my personal token for Dropbox connection?
#' @param auto Boolean. Automatically user my local personal file? 
#' @export
get_stocks <- function(filename = NA, token_dir = "~/Dropbox (Personal)/Documentos/Docs/Data", auto = TRUE) {
  
  processFile <- function(file) {
    cash <- read.xlsx(file, sheet = 'Fondos', skipEmptyRows = TRUE, detectDates = TRUE)
    trans <- read.xlsx(file, sheet = 'Transacciones', skipEmptyRows = TRUE, detectDates = TRUE)
    port <- read.xlsx(file, sheet = 'Portafolio', skipEmptyRows = TRUE, detectDates = TRUE)
    port <- port[port$Stocks != 0,]
    mylist <- list("portfolio" = port, "transactions" = trans, "cash" = cash)
    return(mylist)
  }
  
  # FOR PERSONAL USE
  local <- Sys.info()
  if (local[["nodename"]] == "MacBook-Pro-de-Bernardo.local" & auto == TRUE) {
    message("Using BL's local file...")
    local <- "~/Dropbox (Personal)/Documentos/Interactive Brokers/Portfolio/Portfolio LC.xlsx"
    results <- processFile(local) 
  } else {
    # FOR EVERYONE'S USE
    if (!is.na(filename)) {
      if (file.exists(filename)) {
        results <- processFile(filename)
      } else {
        stop("Error: that file doesn't exist or it's not in your working directory!")
      }
    } else {
      # FOR DROPBOX'S USE
      token_dir <- token_dir
      load(paste0(token_dir, "/token_pers.rds"))
      x <- drop_search("Portfolio LC.xlsx", dtoken = token)
      file <- "temp.xlsx"
      invisible(
        drop_download(x$matches[[1]]$metadata$path_lower,
                      local_path = file,
                      overwrite = TRUE,
                      dtoken = token))
      results <- processFile(file)
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
#' @param symbols Character Vector. List of symbols to download historical data. 
#' Example: c('VTI','TSLA')
#' @param from Date. Since when do you wish to download historical data
#' @param today Boolean. Do you wish to additionaly download today's quote?
#' @param tax Numeric. Percentage for dividends real return. Range from 0 to 99
#' @param verbose Boolean. Print results and progress while downloading?
#' @export
get_stocks_hist <- function(symbols = NA, 
                            from = NA, 
                            today = TRUE, 
                            tax = 30, 
                            verbose = TRUE) {
  
  if (!haveInternet()) {
    stop("You currently have NO internet connection!")
  }
  
  options("getSymbols.warning4.0" = FALSE)
  options("getSymbols.yahoo.warning" = FALSE)
  data <- divs <- c()
  
  if (!any(is.na(symbols))) {
    from[is.na(from)] <- Sys.Date() - 365
    if (length(from) == length(symbols)) {
      for (i in 1:length(symbols)) {
        symbol <- as.character(symbols[i])
        start_date <- as.character(from[i])
        values <- getSymbols(symbol, env = NULL, from = start_date, src = "yahoo") %>% data.frame()
        values <- cbind(row.names(values), as.character(symbol), values)
        colnames(values) <- c("Date","Symbol","Open","High","Low","Close","Volume","Adjusted")
        values <- mutate(values, Adjusted = rowMeans(dplyr::select(values, High, Close), na.rm = TRUE))
        row.names(values) <- NULL
        
        # Add right now's data
        if (today == TRUE) {
          quote <- function(ticks) {
            qRoot <- "https://query1.finance.yahoo.com/v7/finance/quote?fields=symbol,longName,regularMarketPrice,regularMarketChange,regularMarketTime&formatted=false&symbols="
            z <- fromJSON(paste(qRoot, paste(ticks, collapse = ","), sep = ""))
            z <- z$quoteResponse$result[,c("symbol", "regularMarketTime", "regularMarketPrice", "regularMarketChange", "longName")]
            row.names(z) <- z$symbol
            z$symbol <- NULL
            names(z) <- c("Time", "Price", "Change", "Name")
            z$Time <- as.POSIXct(z$Time, origin = '1970-01-01 00:00:00')
            return(z)
          }
          now <- quote(symbol)
          now <- data.frame(Date = as.character(as.Date(now$Time)), Symbol = symbol,
                            Open = now$Price, High = now$Price, Low = now$Price, Close = now$Price,
                            Volume = 0, Adjusted = now$Price)
          values <- rbind(values, now)
        }
        
        data <- rbind(data, values)
        
        # Dividends if case
        d <- quantmod::getDividends(as.character(symbol), from = start_date)
        if (nrow(d) > 0) {
          div <-  data.frame(Symbol = rep(symbol, nrow(d)),
                             Date = ymd(row.names(data.frame(d))),
                             Div = as.vector(d),
                             DivReal = as.vector(d)*(100 - tax)/100)
          divs <- rbind(divs, div)
        }
        if (verbose == TRUE) {
          info <- paste(symbol, "since", start_date, "   ")
          statusbar(i, length(symbols), info)  
        }
      }
    } else {message("The parameters 'symbols' and 'from' should be the same length.") }
  } else {message("You need to define which stocks to bring. Use the 'symbols=' parameter.") }
  results <- list("values" = data, "dividends" = divs)
  return(results)
}


####################################################################
#' Fix Historical Data on Stocks
#' 
#' This function lets the user fix downloaded stock data into a usefull 
#' format output
#' 
#' @family Investment
#' @param dailys Dataframe. Daily values. Structure: "Date", "Symbol", 
#' "Open", "High", "Low", "Close", "Volume", "Adjusted"
#' @param dividends Dataframe. Dividends. Structure: "Symbol", "Date", 
#' "Div", "DivReal"
#' @param transactions Dataframe. Transactions. Structure: "ID", "Inv", 
#' "CODE", "Symbol", "Date", "Quant", "Value", "Amount", "Description"
#' @param expenses Numeric. How much does that bank or broker charges per
#' transaction? Absolute value.
#' @export
stocks_hist_fix <- function(dailys, dividends, transactions, expenses = 7) {
  
  dailys_structure <- c("Date", "Symbol", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  dividends_structure <- c("Symbol", "Date", "Div", "DivReal")
  trans_structure <- c("ID", "Inv", "CODE", "Symbol", "Date", "Quant", "Value", "Amount", "Description")
  
  if (!all(dailys_structure %in% colnames(dailys))) {
    stop(paste("The structure of the 'dailys' table should be:",
               paste(shQuote(dailys_structure), collapse = ", ")))
  }
  
  if (!all(dividends_structure %in% colnames(dividends))) {
    stop(paste("The structure of the 'dividends' table should be:",
               paste(shQuote(dividends_structure), collapse = ", ")))
  }
  
  if (!all(trans_structure %in% colnames(transactions))) {
    stop(paste("The structure of the 'transactions' table should be:",
               paste(shQuote(trans_structure), collapse = ", ")))
  }
  
  df <- dailys %>%
    mutate(Date = as.Date(as.character(Date)), Symbol = as.character(Symbol)) %>%
    arrange(Date, Symbol) %>%
    # Add transactions daily data
    left_join(transactions %>%
                mutate(Date = as.Date(as.character(Date)), Symbol = as.character(Symbol)) %>%
                dplyr::select(Symbol, Date, Quant, Value, Amount),
              by = c('Symbol','Date')) %>%
    mutate(Expenses = ifelse(is.na(Quant), 0, expenses)) %>%
    group_by(Symbol) %>%
    mutate(Quant = ifelse(is.na(Quant), 0, Quant),
           Stocks = cumsum(Quant)) %>%
    # Add dividends daily data
    left_join(dividends %>%
                mutate(Date = as.Date(as.character(Date)), Symbol = as.character(Symbol)),
              by = c('Symbol','Date')) %>%
    mutate(DailyDiv = ifelse(is.na(DivReal), 0, Stocks * DivReal)) %>% ungroup() %>%
    # If sold everything and then restarted...
    group_by(Symbol) %>% 
    mutate(group = ifelse(lead(Stocks) > 0 & Stocks == 0, 1, 0)) %>%
    mutate(groupi = cumsum(group)) %>% ungroup() %>% 
    mutate(Ticker = ifelse(groupi > 0, paste0(Symbol, groupi + 1), Symbol)) %>% 
    select(-group, -groupi) %>% 
    # Some other cumulative calculations
    arrange(Date) %>% group_by(Stocks) %>%
    mutate(DailyValue = Close * Stocks) %>%
    arrange(desc(Date), desc(DailyValue)) %>%
    arrange(Date) %>% group_by(Ticker) %>%
    mutate(StartUSD = Value[Date == min(Date)],
           RelChangeP = 100 - (100 * lag(Close) / Close),
           RelChangeUSD = Stocks * (Close - lag(Close)) - Expenses,
           RelChangePHist = ifelse(Date == min(Date), 0, 
                                   100 - (100 * StartUSD / Close)),
           RelChangeUSDHist = Stocks * (Close - StartUSD) - sum(Expenses)) %>%
    arrange(desc(Date)) %>% 
    #mutate_if(is.numeric, funs(round(., 2))) %>% 
    ungroup() %>%
    mutate_at(vars(-contains("Date")), funs(replace(., is.na(.), 0))) %>%
    group_by(Date, Ticker) %>% arrange(desc(Volume)) %>% slice(1) %>% ungroup()
  
  return(df)
  
}


####################################################################
#' Stocks Overall Performance
#' 
#' This function lets the user calculate stocks performance
#' 
#' @family Investment
#' @param dailys Dataframe. Daily values. Structure: "Date", "Symbol", 
#' "Open", "High", "Low", "Close", "Volume", "Adjusted", "Quant", 
#' "Value", "Amount", "Expenses", "Stocks", "Div", "DivReal", "DailyDiv", 
#' "DailyValue", "RelChangeP", 
#' "RelChangeUSD"
#' @param cash_in Dataframe. Deposits and withdrawals. Structure: "ID", "Date", "Cash"
#' @param cash_fix Numeric. If you wish to algebraically sum a value 
#' to your cash balance
#' @export
stocks_performance <- function(dailys, cash_in, cash_fix = 0)  {
  
  dailys_structure <- c("Date", "Symbol", "Open", "High", "Low", "Close", "Volume", "Adjusted",
                        "Quant", "Value", "Amount", "Expenses", "Stocks", "Div", "DivReal",
                        "DailyDiv", "DailyValue", "RelChangeP", "RelChangeUSD", 
                        "RelChangePHist", "RelChangeUSDHist")
  
  cash_structure <- c("ID", "Date", "Cash")
  
  if (!all(dailys_structure %in% colnames(dailys))) {
    stop(paste("The structure of the 'dailys' table should be:",
               paste(shQuote(dailys_structure), collapse = ", ")))
  }
  
  if (!all(cash_structure %in% colnames(cash_in))) {
    stop(paste("The structure of the 'cash_in' table should be:",
               paste(shQuote(cash_structure), collapse = ", ")))
  }
  
  result <- dailys %>% group_by(Date) %>%
    summarise(Stocks = n(),
                     DailyStocks = sum(DailyValue),
                     DailyTrans = sum(Amount),
                     DailyExpen = sum(Expenses),
                     DailyDiv = sum(DailyDiv),
                     RelUSD = sum(RelChangeUSD)) %>%
    mutate(RelPer = round(100 * RelUSD / DailyStocks, 2),
           CumDiv = cumsum(DailyDiv),
           CumExpen = cumsum(DailyExpen)) %>%
    left_join(cash_in %>% dplyr::select(Date, Cash), by = c('Date')) %>%
    mutate(DailyCash = ifelse(is.na(Cash), 0, Cash),
           CumCash = cumsum(DailyCash) - cumsum(DailyTrans) + cumsum(DailyDiv) + cash_fix,
           CumPortfolio = CumCash + DailyStocks,
           TotalUSD = DailyStocks - cumsum(DailyTrans),
           TotalPer = round(100 * DailyStocks / (cumsum(DailyTrans)), 2) - 100) %>%
    select(Date,CumPortfolio,TotalUSD,TotalPer,RelUSD,RelPer,DailyStocks,
                  DailyTrans,DailyDiv,CumDiv,DailyCash,CumCash) %>% arrange(desc(Date)) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    distinct()
  
  return(result)
  
}


####################################################################
#' Portfolio Overall Performance
#' 
#' This function lets the user calculate portfolio performance
#' 
#' @family Investment
#' @param portfolio Dataframe. Structure: "Symbol", "Stocks", "StockIniValue", "InvPerc", "Type", "Trans", "StartDate"
#' @param daily Dataframe. Daily data
#' @export
portfolio_performance <- function(portfolio, daily) {
  
  portf_structure <- c("Symbol", "Stocks", "StockIniValue", "InvPerc", "Type", "Trans", "StartDate")
  
  if (!all(portf_structure %in% colnames(portfolio)))
    stop(paste("The structure of the 'portfolio' table should be:", vector2text(portf_structure)))
  
  divIn <- daily %>% group_by(Symbol) %>%
    summarise(DivIncome = sum(DailyDiv),
              DivPerc = round(100 * DivIncome / sum(Amount), 2)) %>%
    arrange(desc(DivPerc))
  
  result <- left_join(portfolio %>% mutate(Symbol = as.character(Symbol)), 
                      daily %>% filter(Date == max(Date)) %>% select(Symbol,Ticker,DailyValue), 
                      by = c('Symbol')) %>%
    mutate(DifUSD = DailyValue - Invested, DifPer = round(100 * DifUSD / Invested,2),
           StockValue = DailyValue / Stocks,
           InvPerc = 100 * InvPerc,
           RealPerc = round(100 * DailyValue/sum(DailyValue), 2)) %>%
    left_join(divIn, by = c('Symbol')) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    select(Symbol:StockIniValue, Ticker, StockValue, InvPerc, RealPerc, everything())
  
  return(result)
  
}


####################################################################
#' Portfolio Daily Performance
#' 
#' This function lets the user calculate daily portfolio performance
#' 
#' @family Investment
#' @param data Dataframe. Result from get_stocks()
#' @param dailys Dataframe. Result from get_stocks_hist()
#' @param cash_fix Numeric. If you wish to algebraically sum a value 
#' to your cash balance
#' @export
portfolio_daily <- function(data, dailys, cash_fix = 0) {
  
  daily_fixed <- stocks_hist_fix(dailys = dailys$values, 
                                 dividends = dailys$dividends, 
                                 transactions = data$transactions)
  
  mindate <- as.Date(min(as.Date(daily_fixed$Date), origin = "1970-01-01"), na.rm = TRUE)

  result <- data.frame(Date = as.Date(mindate:Sys.Date(), origin = "1970-01-01")) %>%
    left_join(data$cash %>% select(Date, Cash) %>% rename(Deposit = Cash), "Date") %>%
    left_join(data$transactions %>% group_by(Date) %>% summarise(Amount = sum(Amount)) %>%
                select(Date, Amount) %>% rename(Invest = Amount), "Date") %>%
    left_join(daily_fixed %>% group_by(Date) %>% 
                summarise(StocksValue = sum(DailyValue), Dividend = sum(DailyDiv),
                          Expense = sum(Expenses)) %>% 
                select(Date, StocksValue, Dividend, Expense), "Date") %>%
    arrange(Date) %>% replace(., is.na(.), 0) %>%
    mutate(Deposited = cumsum(Deposit)) %>%
    mutate(Invested = cumsum(Invest)) %>%
    mutate(StocksValue = ifelse(StocksValue == 0, lag(StocksValue), StocksValue)) %>%
    mutate(StocksValue = ifelse(StocksValue == 0, lag(StocksValue), StocksValue)) %>%
    mutate(StocksValue = ifelse(StocksValue == 0, lag(StocksValue), StocksValue)) %>%
    mutate(Dividends = cumsum(Dividend)) %>%
    mutate(Expenses = cumsum(Expense)) %>%
    mutate(Portfolio = (Deposited - Invested) + StocksValue + Dividends - Expenses) %>%
    mutate(Cash = Portfolio - StocksValue - Expenses + cash_fix) %>%
    mutate(Performance = round(100 * (1 - Invested/StocksValue), 2)) %>%
    select(Date,Deposit,Invest,Dividend,Expense,Deposited,
           Invested,Dividends,Expenses,StocksValue,everything())
  
  return(result)
  
}


################# PLOTTING FUNCTIONS #################

####################################################################
#' Portfolio Daily Plot
#' 
#' This function lets the user plot his portfolio daily change
#' 
#' @family Investment
#' @param stocks_perf Dataframe. Output of the stocks_performance function
#' @param save Boolean. Export plot as an image?
#' @export
portfolio_daily_plot <- function(stocks_perf, save = FALSE) {
  
  stocks_perf <- stocks_perf %>% 
    # Get rid of super picks
    filter(abs(RelPer) < 70) %>% 
    # Add day before first date with zero data
    rbind(tail(stocks_perf, 1) %>% mutate(Date = Date - 1, TotalPer = 0))
  
  plot <- stocks_perf %>%
    filter(abs(RelPer) < 70) %>%
    mutate(color = ifelse(RelPer > 0, "Pos", "Neg")) %>%
    ggplot() +
    geom_area(aes(x = Date, y = TotalPer/(0.5*max(stocks_perf$TotalPer))), alpha = 0.15) +
    geom_bar(aes(x = Date, y = RelPer, fill = color), stat = 'identity', width = 1) +
    geom_line(aes(x = Date, y = TotalPer/(0.5*max(stocks_perf$TotalPer))), alpha = 0.9, colour = "black") +
    geom_hline(yintercept = 0, alpha = 0.5, color = "black") +
    guides(fill = FALSE) + 
    scale_x_date(date_labels = "%b%y") +
    scale_y_continuous(
      labels = comma,
      sec.axis = sec_axis(~.*(0.5 * max(stocks_perf$TotalPer)), 
                          name = "% Portfolio Var", labels = comma)) +
    labs(y = '% Daily Var', x = '',
         title = 'Daily Portfolio\'s Growth (%) since Start',
         subtitle = paste(stocks_perf$Date[1]," (Includes Expenses): ",
                          formatNum(stocks_perf$TotalPer[1],2),"% ($",
                          formatNum(stocks_perf$TotalUSD[1], 0),") | $",
                          formatNum(stocks_perf$CumPortfolio[1]), sep = "")) +
    theme_lares2()
  
  if (save) plot <- plot + ggsave("portf_daily_change.png", width = 8, height = 5, dpi = 300)

  return(plot)
  
}


####################################################################
#' Stocks Total Performance Plot
#' 
#' This function lets the user plot his stocks total performance
#' 
#' @family Investment
#' @param stocks_perf Dataframe. Output of the stocks_performance function
#' @param portfolio_perf Dataframe. Output of the portfolio_performance function
#' @param daily Dataframe. Daily data
#' @param trans Dataframe. Transactions data
#' @param cash Dataframe. Cash data
#' @param save Boolean. Export plot as an image?
#' @export
stocks_total_plot <- function(stocks_perf, portfolio_perf, daily, trans, cash, save = FALSE) {
  
  tops <- max(rbind(portfolio_perf$Invested, portfolio_perf$DailyValue))
  summary <- rbind(
    paste0("Portfolio: $", formatNum(stocks_perf$CumPortfolio[1])," | ", max(daily$Date)),
    paste0("Stocks: $", formatNum(sum(stocks_perf$DailyStocks[1]),1)," & Cash: $", 
           formatNum(stocks_perf$CumCash[1],1)),
    paste0("ROI: ", formatNum(stocks_perf$TotalPer[1], 2),"% ($",
           formatNum(stocks_perf$TotalUSD[1],0),")"),
    paste0("Dividends: $", formatNum(sum(daily$DailyDiv),0)," & Expenses: $", 
           formatNum(sum(daily$Expenses),0)))
  
  plot <- portfolio_perf %>%
    mutate(shapeflag = ifelse(DifUSD < 0, 25, 24), box = -tops/5.5) %>% ungroup() %>%
    mutate(Symbol = paste0(Symbol, " (", formatNum(100*DailyValue/sum(DailyValue)), "%)")) %>%
    ggplot() + 
    geom_hline(yintercept = 0, colour = "black") +
    geom_col(aes(x = reorder(Symbol, Invested), y = Invested, fill = Symbol, group = 1)) +
    geom_col(aes(x = Symbol, y = Invested + DifUSD, fill = Symbol), alpha = 0.5) +
    geom_col(aes(x = Symbol, y = box), fill = "grey", alpha = 0.5) +
    geom_point(aes(x = Symbol, y = Invested + DifUSD, shape = shapeflag), colour = "black") +
    scale_shape_identity() +
    geom_text(aes(label = paste0("$",formatNum(DifUSD,1)), y = Invested + DifUSD, x = Symbol), 
              size = 2.9, hjust = -.2, vjust = -0.2) +
    geom_text(aes(label = paste0(DifPer, "%"), y = Invested + DifUSD, x = Symbol), 
              size = 2.9, hjust = -.2, vjust = 1.2) +
    geom_text(aes(label = paste0("$", formatNum(DailyValue, 1)), y = box, x = Symbol), 
              size = 3, hjust = -.1, vjust = -0.2) +
    geom_text(aes(label = paste0(Stocks, " @$", formatNum(DailyValue/Stocks, 2)), y = box, x = Symbol), 
              size = 2, hjust = -.1, vjust = 1.5) +
    geom_text(aes(label = paste0("$", formatNum(Invested,1)), y = 0, x = Symbol, colour = Symbol), 
              size = 2, hjust = 0, vjust = -0.2) +
    geom_text(aes(label = paste0("@$", formatNum(Invested/Stocks, 2)), y = 0, x = Symbol, colour = Symbol), 
              size = 2, hjust = 0, vjust = 1.5) +
    annotate("label", x = length(unique(portfolio_perf$Stocks)) * 0.25, y = tops * 0.6, 
             label = vector2text(summary,"\n",quotes = F), size = 3.5, hjust = 0, alpha = 0.55) +
    scale_y_continuous(limits = c(NA, tops*1.1), labels = comma, expand = c(0, 0)) + 
    labs(y = '', x = '', title = "Stocks Distribution and Growth") +
    guides(fill = FALSE, colour = FALSE) + coord_flip() +
    theme_lares2(pal = 1)
  
  if (save) plot <- plot + ggsave("portf_stocks_change.png", width = 8, height = 8, dpi = 300)
  
  return(plot)
  
}


####################################################################
#' Stocks Daily Plot
#' 
#' This function lets the user plot stocks daily change
#' 
#' @family Investment
#' @param portfolio Dataframe. Output of the portfolio_perf function
#' @param daily Dataframe. Daily data
#' @param weighted Boolean. Should variation values be weighted to the
#' portfolio (or simply compared with initial value)?
#' @param group Boolean. Group stocks by stocks type?
#' @param save Boolean. Export plot as an image?
#' @export
stocks_daily_plot <- function(portfolio, daily, weighted = TRUE, group = TRUE, save = FALSE) {
  
  try_require("ggrepel")
  
  d <- daily %>%
    left_join(portfolio %>% select(Symbol,Type), by = 'Symbol') %>%
    arrange(Date) %>% group_by(Symbol) %>%
    mutate(Hist = if (weighted) {100*(1 - cumsum(Amount)/(Stocks*Adjusted))} else {RelChangePHist},
           BuySell = ifelse(Amount > 0, "Bought", ifelse(Amount < 0, "Sold", NA)))
  labels <- d %>% filter(Date == max(Date))
  amounts <- d %>% filter(Amount != 0) %>%
    mutate(label = paste0(round(Amount/1000,1),"K"))
  days <- as.integer(difftime(range(d$Date)[2], range(d$Date)[1], units = "days"))
  plot <- ggplot(d) + ylab('% Change since Start') +
    geom_hline(yintercept = 0, alpha = 0.8, color = "black") +
    geom_line(aes(x = Date, y = Hist, color = Symbol), alpha = 0.9, size = 0.5) +
    geom_point(aes(x = Date, y = Hist, size = abs(Amount), colour = BuySell), alpha = 0.6) +
    scale_y_continuous(position = "right") +
    scale_size(range = c(0, 3.2)) + guides(size = FALSE, colour = FALSE) + 
    xlim(min(d$Date), max(d$Date) + round(days*0.08)) +
    labs(title = 'Daily Portfolio\'s Stocks Change (%) since Start', x = '',
         subtitle = 'Showing absolute delta values since first purchase', colour = '') +
    geom_label_repel(data = amounts, aes(x = Date, y = Hist, label = label), size = 2) +
    geom_label(data = labels, aes(x = Date, y = Hist, label = Symbol), size = 2.5, hjust = -0.2, alpha = 0.6) +
    theme_lares2(pal = 2)
  
  if (group) plot <- plot + facet_grid(Type ~ ., scales = "free", switch = "both")
  if (weighted) plot <- plot + labs(subtitle = "Showing real weighted portfolio delta values")
  if (save) plot <- plot + ggsave("portf_stocks_histchange.png", width = 8, height = 5, dpi = 300) 
  
  return(plot)
  
}


####################################################################
#' Portfolio's Category Distribution
#' 
#' This function lets the user plot his portfolio's distribution
#' 
#' @family Investment
#' @param portfolio_perf Dataframe. Output of the portfolio_performance function
#' @param save Boolean. Export plot as an image?
#' @export
portfolio_distr_plot <- function(portfolio_perf, save = FALSE) {
  
  p <- portfolio_perf %>%
    group_by(Type) %>% 
    mutate(label = paste0(Type, "\n", formatNum(
      100*sum(DailyValue)/sum(portfolio_perf$DailyValue)),"%")) %>%
    ggplot() +
    geom_bar(aes(x = "", y = DailyValue, fill = Symbol), width = 1, stat = "identity") +
    facet_grid(. ~ label, scales = "free") +
    scale_y_continuous(labels = scales::comma, expand = c(0, 0)) + 
    theme_lares2(pal = 1) +
    labs(x = NULL, y = "Total value", title = "Portfolio's Category Distribution")
  if (save) p <- p + ggsave("portf_distribution.png", width = 8, height = 5, dpi = 300) 
  return(p)
}


####################################################################
#' ETF's Sectors Breakdown
#' 
#' This function scraps etf.com data for sector breakdown on ETFs.
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
      sector <- data.frame(matrix(unlist(strsplit(sector, split = "\n"))[-c(1:5)], ncol = 2, byrow = TRUE))
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
    if (verbose == TRUE & length(etf) > 1) {
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
#' @param portfolio_perf Dataframe. Output of the portfolio_performance function
#' @param save Boolean. Export plot as an image?
#' @export
etf_sector_plot <- function(portfolio_perf, save = FALSE) {
  
  structure <- c("Symbol", "DailyValue")
  
  if (!all(structure %in% colnames(portfolio_perf))) {
    stop(paste("portfolio_perf should contain all of the following:",
               paste(shQuote(structure), collapse = ", ")))
  }
  
  message(">>> Downloading ETF's sectors...")
  etfs <- etf_sector(portfolio_perf$Symbol)
  
  if (nrow(etfs) > 0) {
    df <- etfs %>% 
      right_join(select(portfolio_perf, Symbol, DailyValue), 
                by = c("ETF" = "Symbol")) %>%
      mutate(Sector = ifelse(is.na(Sector), "Not Known", as.character(Sector))) %>%
      replace(., is.na(.), 100) %>%
      mutate(Value = DailyValue * Percentage / 100) %>%
      group_by(Sector) %>% mutate(total = sum(Value)) %>% ungroup() %>% 
      group_by(Sector) %>% mutate(label = paste0(
        Sector, " (", formatNum(100*total/sum(portfolio_perf$DailyValue), 1), "%)"))
    
    p <- ggplot(df, aes(x = reorder(label,total), y = Value, fill = ETF)) +
      geom_bar(width = 1, stat = "identity") +
      coord_flip() +
      scale_y_continuous(labels = comma, expand = c(0, 0)) + 
      theme_lares2(pal = 1) +
      labs(x = NULL, y = "Total value", title = "Portfolio's Sector Distribution (ETFs)", fill = NULL)
    if (save) p <- p + ggsave("portf_distribution_etfs.png", width = 8, height = 5, dpi = 300) 
    return(p) 
  } else {
    return(noPlot("No data here!"))
  }
}


####################################################################
#' Portfolio's Daily Cumulative
#' 
#' This function lets the user plot his portfolio's daily cumulative
#' 
#' @family Investment
#' @param portfolio Dataframe. Results from portfolio_daily()
#' @param save Boolean. Export plot as an image?
#' @export
portfolio_total_plot <- function(portfolio, save = FALSE) {
  
  try_require("ggrepel")
  
  labels <- portfolio %>% filter(Deposit != 0)
  caption <- paste0("Portfolio: $", formatNum(portfolio$Portfolio[nrow(portfolio)]),
                    "\nInvested: $", formatNum(portfolio$StocksValue[nrow(portfolio)]))
  
  plot <- data.frame(Date = rep(portfolio$Date, 2),
                  type = c(rep("Invested", nrow(portfolio)), 
                           rep("Cash", nrow(portfolio))),
                  values = c(portfolio$StocksValue, portfolio$Cash)) %>%
    ggplot() + 
    geom_area(aes(x = Date, y = values, fill = type, group = type), 
              colour = "black", size = 0.2, alpha = 0.95) + 
    labs(title = "  Daily Total Portfolio Value", y = NULL, x = NULL, fill = "") +
    geom_label_repel(data = labels, 
                     aes(x = Date, y = Portfolio, label = formatNum(Deposit, 0)), 
                     vjust = -1.3, size = 2.5) +
    scale_y_continuous(position = "right", labels = comma) +
    annotate("text", label = caption, x = max(portfolio$Date), 
             y = 0.09*max(portfolio$Portfolio), 
             size = 3.3, colour = "white", hjust = 1.1) +
    theme_lares2(pal = 1) +
    theme(legend.position = "top", legend.justification = c(0, 1))
  
  if (save) plot <- plot + 
    ggsave("portf_total_hist.png", width = 8, height = 5, dpi = 300)
  
  return(plot)
  
}


################# REPORTING FUNCTIONS #################

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
#' @param tax Numeric. How much does of your dividends does the taxes take? 
#' Range from 0 to 99
#' @param expenses Numeric. How much does that bank or broker charges per
#' transaction? Absolute value.
#' @param sectors Boolean. Return sectors segmentation for ETFs?
#' @export
stocks_objects <- function(data, cash_fix = 0, tax = 30, expenses = 7, sectors = TRUE) {
  
  tabs <- c('portfolio','transactions','cash')
  if (sum(names(data) %in% tabs) != 3) {
    not <- names(data)[!names(data) %in% tabs]
    stop(paste("The following objects are obligatory too:", vector2text(not)))
  }
  
  tempdir <- tempdir()
  on.exit(setwd(tempdir))
  
  # Data wrangling and calculations
  message(">>> Downloading historical data for each stock...")
  hist <- get_stocks_hist(symbols = data$portfolio$Symbol, 
                          from = data$portfolio$StartDate, 
                          tax = tax)
  daily <- stocks_hist_fix(dailys = hist$values, 
                           dividends = hist$dividends, 
                           transactions = data$transactions, 
                           expenses = expenses)
  stocks_perf <- stocks_performance(daily, 
                                    cash_in = data$cash, 
                                    cash_fix = cash_fix)
  portfolio_perf <- portfolio_performance(portfolio = data$portfolio, 
                                          daily = daily)
  pf_daily <- portfolio_daily(data = data, dailys = hist, cash_fix = cash_fix)
  
  message("Calculations ready...")
  
  # Visualizations
  p1 <- portfolio_daily_plot(stocks_perf)
  p2 <- stocks_total_plot(stocks_perf, portfolio_perf, daily, 
                          trans = data$transactions, 
                          cash = data$cash)
  p3 <- stocks_daily_plot(portfolio = data$portfolio, daily, weighted = FALSE)
  p5 <- stocks_daily_plot(portfolio = data$portfolio, daily, weighted = TRUE)
  p6 <- portfolio_total_plot(pf_daily)
  p4 <- portfolio_distr_plot(portfolio_perf)
  if (sectors) p7 <- etf_sector_plot(portfolio_perf)
  
  message("Graphics ready...")
  
  # Consolidation
  results <- list(p_portf_daily_change = p1,
                  p_portf_stocks_change = p2,
                  p_portf_stocks_histchange_weighted = p5,
                  p_portf_stocks_histchange_absolute = p3,
                  p_portf_distribution = p4,
                  p_portfolio_daily = p6,
                  df_portfolio_perf = portfolio_perf,
                  df_portfolio_daily = pf_daily,
                  df_stocks_perf = stocks_perf,
                  df_daily = daily,
                  df_hist = hist)
  if (sectors) results[["p_sectors"]] <- p7
  
  unlink(tempdir, recursive = FALSE)
  
  message("All results ready to export!")
  return(results)
}

####################################################################
#' Portfolio's Full Report in HTML
#' 
#' This function lets the user create his portfolio's full report in HTML using
#' the library's results
#' 
#' @family Investment
#' @param results List. Containing the following objects: portf_daily_change, 
#' portf_stocks_change, portf_stocks_histchange, portf_distribution & portfolio_perf.
#' You can use simply use the stocks_objects(data) if you didn't mess with the order!
#' @export
stocks_html <- function(results) {
  
  dir <- getwd()
  pandoc <- Sys.getenv("RSTUDIO_PANDOC")
  Sys.setenv(RSTUDIO_PANDOC = pandoc)
  
  # Can be more accurate with names but works for me!
  params <- list(portf_daily_change = results[["p_portf_daily_change"]],
                 portf_stocks_change = results[["p_portf_stocks_change"]],
                 portf_stocks_histchange_weighted = results[["p_portf_stocks_histchange_weighted"]],
                 portf_stocks_histchange_absolute = results[["p_portf_stocks_histchange_absolute"]],
                 portf_distribution = results[["p_portf_distribution"]],
                 portf_daily = results[["p_portfolio_daily"]],
                 portfolio_perf = results[["df_portfolio_perf"]])
  if ("p_sectors" %in% names(results)) 
    params[["portf_distribution_sectors"]] <- results[["p_sectors"]]
  
  invisible(file.copy(
    from = system.file("docs", "stocksReport.Rmd", package = "lares"),
    to = dir, 
    overwrite = TRUE, 
    recursive = FALSE, 
    copy.mode = TRUE))
  
  rmarkdown::render("stocksReport.Rmd", 
                    output_file = "stocksReport.html",
                    params = params,
                    envir = new.env(parent = globalenv()),
                    quiet = TRUE)  
  
  invisible(file.remove(paste0(dir, "/stocksReport.Rmd")))
  message("HTML report created succesfully!")
}


####################################################################
#' Portfolio's Full Report and Email
#' 
#' This function lets the user create his portfolio's full report with plots and email sent
#' 
#' @family Investment
#' @param wd Character. Where do you wish to save the results (plots and report)?
#' @param cash_fix Numeric. If you wish to algebraically sum a value to your cash balance
#' @param sectors Boolean. Return sectors segmentation for ETFs?
#' @param mail Boolean. Do you wish to send the email? Set to NA to not send email
#' @param creds Character. Credential's user (see get_credentials) for sending mail
#' @export
stocks_report <- function(wd = "personal", cash_fix = 0, 
                          sectors = TRUE,
                          mail = "laresbernardo@gmail.com", 
                          creds = NA) {
  
  # Setting up working directory
  if (dir.exists(wd)) {
    temp <- wd
  } else {
    temp <- tempdir() 
  }
  on.exit(setwd(temp))
  
  # Set token for Sendgrid credentials:
  token_dir <- case_when(
    wd == "personal" ~ "~/Dropbox (Personal)/Documentos/Docs/Data",
    wd %in% c("matrix","server") ~ "~/creds",
    TRUE ~ as.character(creds))
  
  # Data extraction and processing
  data <- get_stocks(token_dir = token_dir)
  results <- stocks_objects(data, sectors = sectors)
  
  # HTML report
  message(">>> Creating HTML report...")
  stocks_html(results)
  
  if (!is.na(mail)) {
    message(">>> Sending email...")
    mailSend(to = mail, 
             subject = paste("Portfolio:", max(results$df_daily$Date)),
             text = " \n", 
             attachment = paste0(getwd(), "/stocksReport.html"),
             creds = token_dir,
             quiet = FALSE)
  }
}

######################### SHORT #####################################
# df <- get_stocks() # Get data from my Dropbox
# dfp <- stocks_objects(df) # Get historical data, make calculations and plots
# stocks_html(dfp) # Create HTML report
# stocks_report() # Create and send report to my mail

######################### LONG #####################################
# df <- get_stocks() # Get data from my Dropbox
# hist <- get_stocks_hist(symbols = df$portfolio$Symbol, from = df$portfolio$StartDate)
# daily <- stocks_hist_fix(dailys = hist$values, dividends = hist$dividends, transactions = df$transactions)

