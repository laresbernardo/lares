# PostgreSQL queries for differente servers and databases

# DUMMY READ-ONLY
queryDummy = function(query, creds = NA) {

  options(warn=-1)

  suppressMessages(require(dplyr))
  suppressMessages(require(RPostgreSQL))
  suppressMessages(require(config))

  dw <- lares::get_credentials(from="dummy", dir = creds)

  drv <- dbDriver(dw$driver)
  con <- dbConnect(drv, host=dw$server, dbname=dw$database,
                   port=dw$port, user=dw$uid, password=dw$pwd)
  start <- Sys.time()
  q <- dbSendQuery(con, query)
  q <- fetch(q, n = -1)
  dbDisconnect(con)
  message(paste("Query duration:", round(difftime(Sys.time(), start, units="secs"), 2), "s"))

  return(q)
  setwd(wd)
}

# REDSHIFT READ-WRITE
queryDW = function(query, which=c("seguros"), creds = NA) {

  # Possible Whichs:
    # seguros
    # tabunga

  options(warn=-1)

  require(dplyr)
  require(RPostgreSQL)
  require(config)

  dw <- lares::get_credentials(from = "warehouse", dir = creds)

  dbname <- ifelse(which == "seguros", dw$database_seguros,
                   ifelse(which == "tabunga", dw$database_tabunga, NA))

  drv <- dbDriver(dw$driver)
  con <- dbConnect(drv, host=dw$server, dbname=dbname,
                   port=dw$port, user=dw$uid, password=dw$pwd)
  start <- Sys.time()
  q <- dbSendQuery(con, query)
  q <- fetch(q, n = -1)
  dbDisconnect(con)
  message(paste("Query duration:", round(difftime(Sys.time(), start, units="secs"), 2), "s"))

  return(q)
  setwd(wd)
}

# PRODUCTION READ-WRITE
queryProduc = function(query, creds = NA) {

  options(warn=-1)

  suppressMessages(require(dplyr))
  suppressMessages(require(RPostgreSQL))
  suppressMessages(require(config))

  dw <- lares::get_credentials(from = "production", dir = creds)

  drv <- dbDriver(dw$driver)
  con <- dbConnect(drv, host=dw$server, dbname=dw$database,
                   port=dw$port, user=dw$uid, password=dw$pwd)
  start <- Sys.time()
  q <- dbSendQuery(con, query)
  q <- fetch(q, n = -1)
  dbDisconnect(con)
  message(paste("Query duration:", round(difftime(Sys.time(), start, units="secs"), 2), "s"))

  return(q)
  setwd(wd)
}
