####################################################################
#' Get Personal Portfolio's Data
#' 
#' This function lets me download my personal Excel with my Portfolio data
#' 
#' @param token_dir Character. Directory containing personal Dropbox token file
#' @export
get_stocks <- function(token_dir="~/Dropbox (Personal)/Documentos/Interactive Brokers/Portfolio") {

  suppressMessages(require(rdrop2))
  suppressMessages(require(xlsx))
  suppressMessages(require(dplyr))
  options("getSymbols.yahoo.warning"=FALSE)
  options("getSymbols.warning4.0"=FALSE)

  valid <- Sys.info()

  if (valid[["user"]] %in% c("bernardo", "rstudio")) {
    
    load(paste0(token_dir, "/token_pers.rds"))

    x <- drop_search("Portfolio LC.xlsx", dtoken = token)
    file <- "temp.xlsx"
    drop_download(x$matches[[1]]$metadata$path_lower,
                  local_path = file,
                  overwrite = TRUE,
                  dtoken = token)

    # Transaccions historical data
    cash <- read.xlsx(file, sheetName = 'Fondos', header=TRUE, colClasses=NA)
    trans <- read.xlsx(file, sheetName = 'Transacciones', header=TRUE, colClasses=NA)
    port <- read.xlsx(file, sheetName = 'Portafolio', header=TRUE, colClasses=NA)
    results <- list("portfolio" = port, "transactions" = trans, "cash" = cash)

    file.remove(file)

  } else { message("User is not authorized to run this function in this device :(") }

  return(results)

}


####################################################################
#' Download Stocks Historical Data
#' 
#' This function lets the user download stocks historical data
#' 
#' @param symbols Character Vector. List of symbols to download historical data. Example: c('VTI','TSLA')
#' @param from Date. Since when do you wish to download historical data
#' @param today Boolean. Do you wish to additionaly download today's quote?
#' @param verbose Boolean. Print results and progress while downloading?
#' @export
get_stocks_hist <- function (symbols = NA, from = Sys.Date() - 365, today = TRUE, verbose = TRUE) {

  suppressMessages(require(quantmod))
  suppressMessages(require(dplyr))
  suppressMessages(require(lubridate))
  suppressMessages(require(jsonlite))

  options(warn=-1)

  if (!is.na(symbols)) {

    if (from == Sys.Date() - 365) {
      from <- rep(from, each = length(symbols))
    }

    if (length(from) == length(symbols)) {

      data <- c()
      divs <- c()

      for(i in 1:length(symbols)) {

        symbol <- as.character(symbols[i])
        start_date <- as.character(from[i])

        values <- getSymbols(symbol, env=NULL, from=start_date, src="yahoo") %>% data.frame()
        values <- cbind(row.names(values), as.character(symbol), values)
        colnames(values) <- c("Date","Symbol","Open","High","Low","Close","Volume","Adjusted")
        values <- mutate(values, Adjusted = rowMeans(select(values, High, Close), na.rm = TRUE))
        row.names(values) <- NULL

        # Add right now's data
        if (today == TRUE) {
          getQuote <- function(ticks) {
            qRoot <- "https://query1.finance.yahoo.com/v7/finance/quote?fields=symbol,longName,regularMarketPrice,regularMarketChange,regularMarketTime&formatted=false&symbols="
            z <- fromJSON(paste(qRoot, paste(ticks, collapse=","), sep=""))
            z <- z$quoteResponse$result[,c("symbol", "regularMarketTime", "regularMarketPrice", "regularMarketChange", "longName")]
            row.names(z) <- z$symbol
            z$symbol <- NULL
            names(z) <- c("Time", "Price", "Change", "Name")
            z$Time <- as.POSIXct(z$Time, origin = '1970-01-01 00:00:00')
            return(z)
          }
          now <- getQuote(symbol)
          now <- data.frame(Date = as.character(as.Date(now$Time)), Symbol = symbol,
                            Open = now$Price, High = now$Price, Low = now$Price, Close = now$Price,
                            Volume = 0, Adjusted = now$Price)
          values <- rbind(values, now)
        }

        data <- rbind(data, values)

        # Dividends if case
        d <- getDividends(as.character(symbol), from = start_date)
        if (nrow(d) > 0) {
          div <-  data.frame(Symbol = rep(symbol, nrow(d)),
                             Date = ymd(row.names(data.frame(d))),
                             Div = as.vector(d),
                             DivReal = as.vector(d)*0.7)
          divs <- rbind(divs, div)
        }
        if (verbose == TRUE) {
          message(paste(symbol," since ",start_date,": done ",lares::formatNum(100*i/length(symbols),1),"% (",i,"/",length(symbols),")",sep=""))
        }
      }
    } else { message("The parameters 'symbols' and 'start_dates' should be the same length.") }
  } else { message("You need to define which stocks to bring. Use the 'stocks=' parameter.") }

  results <- list("values" = data, "dividends" = divs)
  message(paste("Until", max(as.Date(as.character(results$values$Date)))[1]))
  return(results)

}


####################################################################
#' Fix Historical Data on Stocks
#' 
#' This function lets the user fix downloaded stock data into a usefull format
#' 
#' @param dailys Dataframe. Daily values. Structure: "Date", "Symbol", "Open", "High", "Low", "Close", "Volume", "Adjusted"
#' @param dividends Dataframe. Dividends. Structure: "Symbol", "Date", "Div", "DivReal"
#' @param transactions Dataframe. Transactions. Structure: "ID", "Inv", "Symbol", "Date", "Quant", "Value", "Amount", "Description"
#' @export
stocks_hist_fix <- function (dailys, dividends, transactions) {

  require(dplyr)
  require(lubridate)
  
  dailys_structure <- c("Date", "Symbol", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  dividends_structure <- c("Symbol", "Date", "Div", "DivReal")
  trans_structure <- c("ID", "Inv", "Symbol", "Date", "Quant", "Value", "Amount", "Description")

  if (colnames(dailys) != dailys_structure) {
    stop(paste("The structure of the 'dailys' table should be:",
               paste(shQuote(dailys_structure), collapse=", ")))
  }

  if (colnames(dividends) != dividends_structure) {
    stop(paste("The structure of the 'dividends' table should be:",
               paste(shQuote(dividends_structure), collapse=", ")))
  }

  if (colnames(transactions) != trans_structure) {
    stop(paste("The structure of the 'transactions' table should be:",
               paste(shQuote(trans_structure), collapse=", ")))
  }

  df <- dailys %>%
    mutate(Date = as.Date(Date), Symbol = as.character(Symbol)) %>%
    arrange(Date, Symbol) %>%
    # Add transactions daily data
    left_join(transactions %>%
                mutate(Date = as.Date(Date), Symbol = as.character(Symbol)) %>%
                select(Symbol, Date, Quant, Value, Amount),
              by = c('Symbol','Date')) %>%
    mutate(Expenses = ifelse(is.na(Quant), 0, 7)) %>%
    dplyr::group_by(Symbol) %>%
    mutate(Quant = ifelse(is.na(Quant), 0, Quant),
           Stocks = cumsum(Quant)) %>%
    # Add dividends daily data
    left_join(dividends %>%
                mutate(Date = as.Date(Date), Symbol = as.character(Symbol)),
              by = c('Symbol','Date')) %>%
    mutate(DailyDiv = ifelse(is.na(DivReal), 0, Stocks * DivReal)) %>%
    # Some other cumulative calculations
    arrange(Date) %>% group_by(Stocks) %>%
    mutate(DailyValue = Close * Stocks) %>%
    arrange(desc(Date), desc(DailyValue)) %>%
    arrange(Date) %>% group_by(Symbol) %>%
    mutate(StartUSD = Value[Date == min(Date)],
           RelChangeP = 100 - (100 * lag(Close) / Close),
           RelChangeUSD = Stocks * (Close - lag(Close)) - Expenses,
           RelChangePHist = ifelse(Date == min(Date), 0, 
                                   100 - (100 * StartUSD / Close)),
           RelChangeUSDHist = Stocks * (Close - StartUSD) - sum(Expenses)) %>%
    arrange(desc(Date)) %>%
    mutate_if(is.numeric, funs(round(., 2)))
  df[is.na(df)] <- 0

  return(df)

}


####################################################################
#' Stocks Overall Performance
#' 
#' This function lets the user calculate stocks performance
#' 
#' @param dailys Dataframe. Daily values. Structure: "Date", "Symbol", "Open", "High", "Low", "Close", "Volume", "Adjusted", "Quant", "Value", "Amount", "Expenses", "Stocks", "Div", "DivReal", "DailyDiv", "DailyValue", "RelChangeP", "RelChangeUSD"
#' @param cash_in Dataframe. Deposits and withdrawals. Structure: "ID", "Date", "Cash"
#' @param cash_fix Numeric. If you wish to algebraically sum a value to your cash balance
#' @export
stocks_performance <- function(dailys, cash_in, cash_fix = 0)  {

  suppressMessages(require(dplyr))

  dailys_structure <- c("Date", "Symbol", "Open", "High", "Low", "Close", "Volume", "Adjusted",
                        "Quant", "Value", "Amount", "Expenses", "Stocks", "Div", "DivReal",
                        "DailyDiv", "DailyValue", "RelChangeP", "RelChangeUSD", 
                        "RelChangePHist", "RelChangeUSDHist")

  cash_structure <- c("ID", "Date", "Cash")

  if (colnames(dailys) != dailys_structure) {
    stop(paste("The structure of the 'dailys' table should be:",
               paste(shQuote(dailys_structure), collapse=", ")))
  }

  if (colnames(cash_in) != cash_structure) {
    stop(paste("The structure of the 'cash_in' table should be:",
               paste(shQuote(cash_structure), collapse=", ")))
  }

  result <- dailys %>% group_by(Date) %>%
    dplyr::summarise(Stocks = n(),
                     DailyStocks = sum(DailyValue),
                     DailyTrans = sum(Amount),
                     DailyExpen = sum(Expenses),
                     DailyDiv = sum(DailyDiv),
                     RelUSD = sum(RelChangeUSD)) %>%
    mutate(RelPer = round(100 * RelUSD / DailyStocks, 2),
           CumDiv = cumsum(DailyDiv),
           CumExpen = cumsum(DailyExpen)) %>%
    left_join(cash_in %>% select(Date, Cash), by = c('Date')) %>%
    mutate(DailyCash = ifelse(is.na(Cash), 0, Cash),
           CumCash = cumsum(DailyCash) - cumsum(DailyTrans) + cumsum(DailyDiv) + cash_fix,
           CumPortfolio = CumCash + DailyStocks,
           TotalPer = round(100 * DailyStocks / (cumsum(DailyTrans)), 2) - 100) %>%
    select(Date,CumPortfolio,TotalPer,RelUSD,RelPer,DailyStocks,
           DailyTrans,DailyDiv,CumDiv,DailyCash,CumCash) %>% arrange(desc(Date)) %>%
    mutate_if(is.numeric, funs(round(., 2)))

  return(result)

}


####################################################################
#' Portfolio Overall Performance
#' 
#' This function lets the user calculate portfolio performance
#' 
#' @param portfolio Dataframe. Structure: "Symbol", "Stocks", "StockIniValue", "InvPerc", "Type", "Trans", "StartDate"
#' @param daily Dataframe. Daily data
#' @export
portfolio_performance <- function(portfolio, daily) {

  suppressMessages(require(dplyr))


  portf_structure <- c("Symbol", "Stocks", "StockIniValue", "InvPerc", "Type", "Trans", "StartDate")

  if (colnames(portfolio) != portf_structure) {
    stop(paste("The structure of the 'portfolio' table should be:",
               paste(shQuote(portf_structure), collapse=", ")))
  }

  divIn <- daily %>% group_by(Symbol) %>%
    dplyr::summarise(DivIncome = sum(DailyDiv),
                     DivPerc = round(100 * DivIncome / sum(Amount), 2)) %>%
    arrange(desc(DivPerc))

  result <- left_join(portfolio %>% mutate(Symbol = as.character(Symbol)), daily[1:nrow(portfolio),] %>%
                        select(Symbol,DailyValue), by = c('Symbol')) %>%
    mutate(DifUSD = DailyValue - Invested, DifPer = round(100 * DifUSD / Invested,2),
           StockValue = DailyValue / Stocks,
           InvPerc = 100 * InvPerc,
           RealPerc = round(100 * DailyValue/sum(DailyValue), 2)) %>%
    left_join(divIn, by = c('Symbol')) %>%
    mutate_if(is.numeric, funs(round(., 2))) %>%
    select(Symbol:StockIniValue, StockValue, InvPerc, RealPerc, everything())

  return(result)

}


################# PLOTTING FUNCTIONS #################

####################################################################
#' Portfolio Daily Plot
#' 
#' This function lets the user plot his portfolio daily change
#' 
#' @param stocks_perf Dataframe. Output of the stocks_performance function
#' @export
portfolio_daily_plot <- function(stocks_perf) {

  require(dplyr)
  require(ggplot2)

  plot <- stocks_perf %>%
    dplyr::mutate(color = ifelse(RelPer > 0, "Pos", "Neg")) %>%
    ggplot() +
    geom_area(aes(x=Date, y=TotalPer/(1.05*max(stocks_perf$TotalPer))), alpha=0.2) +
    geom_bar(aes(x=Date, y=RelPer, fill=color), stat='identity', width=1) +
    geom_line(aes(x=Date, y=TotalPer/(1.05*max(stocks_perf$TotalPer))), alpha=0.5) +
    geom_hline(yintercept = 0, alpha=0.5, color="black") +
    guides(fill=FALSE) + theme_minimal() +
    scale_x_date(date_minor_breaks = "1 month", date_labels = "%b%y") +
    scale_y_continuous(breaks=seq(-100, 100, 0.5),
                       sec.axis = sec_axis(~.*(1.05*max(stocks_perf$TotalPer)), name = "% Portfolio Var", breaks=seq(-100, 100, 2))) +
    labs(y = '% Daily Var', x = '',
         title = 'Daily Portfolio\'s Stocks Change (%) since Start',
         subtitle = paste(stocks_perf$Date[1]," (Includes Expenses): ",
                          stocks_perf$TotalPer[1],"% ($",lares::formatNum(stocks_perf$DailyStocks[1] - sum(stocks_perf$DailyTrans)),") | $",
                          lares::formatNum(stocks_perf$CumPortfolio[1]), sep="")) +
    ggsave("portf_daily_change.png", width = 10, height = 6, dpi = 300)

  return(plot)

}


####################################################################
#' Stocks Total Performance Plot
#' 
#' This function lets the user plot his stocks total performance
#' 
#' @param stocks_perf Dataframe. Output of the stocks_performance function
#' @param portfolio_perf Dataframe. Output of the portfolio_performance function
#' @param daily Dataframe. Daily data
#' @param trans Dataframe. Transactions data
#' @param cash Dataframe. Cash data
#' @export
stocks_total_plot <- function(stocks_perf, portfolio_perf, daily, trans, cash) {

  require(dplyr)
  require(ggplot2)
  require(scales)
  
  tops <- max(rbind(portfolio_perf$Invested, portfolio_perf$DailyValue))
  summary <- rbind(
    paste0("Portfolio: $", lares::formatNum(stocks_perf$CumPortfolio[1])," | ", max(daily$Date)),
    paste0("Stocks: $", lares::formatNum(sum(stocks_perf$DailyStocks[1]))," & Cash: $", stocks_perf$CumCash[1]),
    paste0("Stocks Investment: $", lares::formatNum(sum(trans$Amount,na.rm=T))),
    paste0("ROI: ", lares::formatNum(stocks_perf$TotalPer[1], 2),"% ($",
           lares::formatNum(stocks_perf$DailyStocks[1]),")"),
    paste0("Dividends: $", lares::formatNum(sum(daily$DailyDiv),0)," & Expenses: $", 
           lares::formatNum(sum(daily$Expenses),0)))
  
  plot <- portfolio_perf %>%
    mutate(shapeflag = ifelse(DifUSD < 0, 25, 24),
           box = -tops/5.5) %>%
    ggplot() + theme_minimal() +
    geom_hline(yintercept = 0, colour = "black") +
    geom_col(aes(x = reorder(Symbol, Invested), y = Invested, fill = Symbol, group = 1)) +
    geom_col(aes(x = Symbol, y = Invested + DifUSD, fill=Symbol), alpha=0.5) +
    geom_col(aes(x = Symbol, y = box), fill="grey", alpha=0.5) +
    geom_point(aes(x = Symbol, y = Invested + DifUSD, shape = shapeflag)) +
    scale_shape_identity() +
    geom_text(aes(label = paste0("$",lares::formatNum(DifUSD,1)), y = Invested + DifUSD, x = Symbol), 
              size = 2.9, hjust = -.2, vjust = -0.2) +
    geom_text(aes(label = paste0(DifPer, "%"), y = Invested + DifUSD, x = Symbol), 
              size = 2.9, hjust = -.2, vjust = 1.2) +
    geom_text(aes(label = paste0("$", lares::formatNum(DailyValue, 1)), y = box, x = Symbol), 
              size = 3, hjust = -.1, vjust = -0.2) +
    geom_text(aes(label = paste0(Stocks, " @$", lares::formatNum(DailyValue/Stocks, 2)), y = box, x = Symbol), 
              size = 2, hjust = -.1, vjust = 1.5) +
    geom_text(aes(label = paste0("$", lares::formatNum(Invested,1)), y = 0, x = Symbol), 
              size = 2, hjust = 0, vjust = -0.2) +
    geom_text(aes(label = paste0("@$", lares::formatNum(Invested/Stocks, 2)), 
                  y = 0, x = Symbol), size = 2, hjust = 0, vjust = 1.5) +
    annotate("label", x = length(unique(portfolio_perf$Stocks))*0.25, y = tops*0.5, 
             label = lares::vector2text(summary,"\n",quotes = F), size = 3.5, hjust = 0, alpha=0.55) +
    scale_y_continuous(limits = c(NA, tops*1.1)) + 
    labs(y='', x='', title="Stocks Distribution and Growth") +
    guides(fill=FALSE) + coord_flip() +
    ggsave("portf_stocks_change.png", width = 8, height = 8, dpi = 300)

  return(plot)

}


####################################################################
#' Stocks Daily Plot
#' 
#' This function lets the user plot stocks daily change
#' 
#' @param portfolio Dataframe. Output of the stocks_performance function
#' @param daily Dataframe. Daily data
#' @export
stocks_daily_plot <- function (portfolio, daily) {

  require(dplyr)
  require(ggplot2)

  plot <- daily %>%
    left_join(portfolio %>% select(Symbol,Type), by='Symbol') %>%
    arrange(Date) %>% group_by(Symbol) %>%
    mutate(Hist = RelChangePHist,
           BuySell = ifelse(Expenses > 0, TRUE, FALSE)) %>%
    ggplot() + theme_minimal() + ylab('% Change since Start') +
    geom_hline(yintercept = 0, alpha=0.8, color="black") +
    geom_line(aes(x=Date, y=Hist, color=Symbol), alpha=0.5, size=1) +
    facet_grid(Type ~ ., scales = "free", switch = "both") +
    scale_y_continuous(position = "right") +
    geom_point(aes(x=Date, y=Hist, fill=BuySell, alpha=BuySell, size=Amount)) +
    scale_size(range = c(0, 3.2)) +
    labs(title = 'Daily Portfolio\'s Stocks Change (%) since Start',
         subtitle = 'Note that the real weighted change is not shown') +
    ggsave("portf_stocks_histchange.png", width = 10, height = 8, dpi = 300)

  return(plot)

}


####################################################################
#' Portfolio's Distribution
#' 
#' This function lets the user plot his portfolio's distribution
#' 
#' @param portfolio_perf Dataframe. Output of the portfolio_performance function
#' @param daily Dataframe. Daily data
#' @export
portfolio_distr_plot <- function (portfolio_perf, daily) {

  require(dplyr)
  require(ggplot2)
  require(gridExtra)

  plot_stocks <- ggplot(portfolio_perf) + theme_minimal() +
    geom_bar(aes(x="",y=DailyValue, fill=Symbol), width=1, stat="identity") +
    coord_polar("y", start=0)
  plot_areas <- ggplot(portfolio_perf) + theme_minimal() +
    geom_bar(aes(x="",y=100*DailyValue/sum(DailyValue), fill=Type), width=1, stat="identity") +
    coord_polar("y", start=0)
  t1 <- tableGrob(portfolio_perf %>% mutate(Perc = lares::formatNum(100*DailyValue/sum(portfolio_perf$DailyValue),2)) %>%
                    select(Symbol, Type, DailyValue, Perc, DifPer), cols = NULL, rows=NULL)
  t2 <- tableGrob(portfolio_perf %>% group_by(Type) %>%
                    dplyr::summarise(DailyValue = sum(DailyValue),
                                     Perc = lares::formatNum(100*DailyValue/sum(portfolio_perf$DailyValue),2),
                                     DifPer = lares::formatNum(100*sum(DailyValue)/sum(Invested)-100,2)) %>%
                    select(Type, DailyValue, Perc, DifPer) %>% arrange(desc(Perc)), cols = NULL, rows=NULL)
  png("portf_distribution.png",width=700,height=500)
  grid.arrange(plot_stocks, plot_areas, t1, t2, nrow=2, heights=c(3,3))
  dev.off()
  return(grid.arrange(plot_stocks, plot_areas, t1, t2, nrow=2, heights=c(3,3)))
}


####################################################################
#' Portfolio's Full Report with Plots and Email
#' 
#' This function lets the user create his portfolio's full report with plots and email sent
#' 
#' @param wd Character. Where do you wish to save the plots?
#' @param cash_fix Numeric. If you wish to algebraically sum a value to your cash balance
#' @param html Boolean. HTML output for the report?
#' @param mail Boolean Do you wish to send the email?
#' @param creds Character. Credential's user (see get_credentials)
#' @export
stocks_report <- function(wd = "personal", cash_fix = 0, html = FALSE, mail = TRUE, creds = NA) {
  
  options("getSymbols.warning4.0"=FALSE)
  options(warn=-1)

  current_wd <- getwd()

  if (wd == "personal") {
    token_dir <- "~/Dropbox (Personal)/Documentos/Docs/Data"
    setwd("~/Dropbox (Personal)/Documentos/Interactive Brokers/Portfolio")
  }
  
  if (wd == "matrix") {
    token_dir = "~/creds"
    setwd("~/personal/IB")
  }

  if (!wd %in% c("personal", "matrix")) {
    wd <- readline(prompt="Set the working directory where your YML file is: ")
    token_dir <- wd
    setwd(wd)
  }
  
  setwd(current_wd)
  tempdir <- tempdir()
  
  # Data extraction
  data <- lares::get_stocks(token_dir = token_dir)
  message("1. Data downloaded...")
  # Data wrangling and calculations
  hist <- lares::get_stocks_hist(symbols = data$portfolio$Symbol, from = data$portfolio$StartDate)
  daily <- lares::stocks_hist_fix(dailys = hist$values, dividends = hist$dividends, transactions = data$transactions)
  stocks_perf <- lares::stocks_performance(daily, cash_in = data$cash, cash_fix = cash_fix)
  portfolio_perf <- lares::portfolio_performance(portfolio = data$portfolio, daily = daily)
  message("2. Calculations ready...")
  # Visualizations
  setwd(tempdir)
  p1 <- lares::portfolio_daily_plot(stocks_perf)
  p2 <- lares::stocks_total_plot(stocks_perf, portfolio_perf, daily, trans = data$transactions, cash = data$cash)
  p3 <- lares::stocks_daily_plot(portfolio = data$portfolio, daily)
  p4 <- lares::portfolio_distr_plot(portfolio_perf, daily)
  message("3. All visuals plotted...")
  # Export and save data
  write.csv(stocks_perf,"mydaily.csv",row.names = F)
  write.csv(portfolio_perf,"myportfolio.csv",row.names = F)
  message("4. CSVs exported...")
  # HTML report
  if (html == TRUE) {
    require(rmarkdown)
    htmlreport <- "stocksReport.html"
    htmlreportready <- paste0("docs/", htmlreport)
    params <- list(portf_daily_change = p1,
                   portf_stocks_change = p2,
                   portf_stocks_histchange = p3,
                   portf_distribution = p4,
                   portfolio_perf = portfolio_perf)
    setwd(current_wd)
    invisible(
      file.copy(
        from = paste0(current_wd,"/docs/stocksReport.Rmd"), 
        to = tempdir, 
        overwrite = TRUE, recursive = FALSE, copy.mode = TRUE))
    setwd(tempdir)
    rmarkdown::render("stocksReport.Rmd", 
                      output_file = htmlreport,
                      params = params,
                      envir = new.env(parent = globalenv()),
                      quiet = TRUE)
  }
  if (mail == TRUE) {
    if (html == TRUE) {
      files <- "stocksReport.html"
    } else {
      files <- c("portf_daily_change.png",
                 "portf_stocks_change.png",
                 "portf_stocks_histchange.png",
                 "portf_distribution.png",
                 "myportfolio.csv",
                 "mydaily.csv")
    }
    lares::mailSend(body = max(daily$Date), 
                    subject = paste("Portfolio:", max(daily$Date)),
                    attachment = files,
                    to = "laresbernardo@gmail.com", 
                    from = 'RServer <bernardo.lares@comparamejor.com>', creds = wd)
    message("5. Email sent...") 
  }
  # Clean everything up and delete files created
  setwd(current_wd)
  if (file.exists(htmlreportready)) file.remove(htmlreportready)
  if (wd != "personal") { file.remove(files) }
  unlink(tempdir, recursive = FALSE)
  graphics.off()
  rm(list = ls())
  message("All's clean and done!")
}
