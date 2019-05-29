####################################################################
#' PostgreSQL Queries on Dummy Database (read only)
#' 
#' This function lets the user query our Dummy Database
#' 
#' @family Connections
#' @param query Character. SQL Query
#' @param creds Character. Credential's user (see get_credentials)
#' @export
queryDummy = function(query, creds = NA) {

  options(warn=-1)
  
  dw <- get_credentials(from="dummy", dir = creds)

  drv <- RPostgreSQL::PostgreSQL()
  con <- dbConnect(drv, host=dw$server, dbname=dw$database,
                   port=dw$port, user=dw$uid, password=dw$pwd)
  start <- Sys.time()
  q <- dbSendQuery(con, query)
  q <- fetch(q, n = -1)
  dbDisconnect(con)
  message(paste("Query duration:", round(difftime(Sys.time(), start, units="secs"), 2), "s"))
  return(q)
}


####################################################################
#' PostgreSQL Queries on Redshift Database (read-write)
#' 
#' This function lets the user query our Data Warehouse Database
#' 
#' @family Connections
#' @param query Character. SQL Query
#' @param which Character. Which database do you wish to connect to: seguros, tabunga...
#' @param creds Character. Credential's user (see get_credentials)
#' @export
queryDW = function(query, which = "soat", creds = NA) {

  options(warn=-1)
  
  dw <- get_credentials(from = "warehouse", dir = creds)

  dbname <- ifelse(which == "soat", dw$database_soat,
                   ifelse(which == "creditos", dw$database_creditos, NA))

  drv <- RPostgreSQL::PostgreSQL()
  con <- dbConnect(drv, host=dw$server, dbname=dbname,
                   port=dw$port, user=dw$uid, password=dw$pwd)
  start <- Sys.time()
  q <- dbSendQuery(con, query)
  q <- fetch(q, n = -1)
  dbDisconnect(con)
  message(paste("Query duration:", round(difftime(Sys.time(), start, units="secs"), 2), "s"))
  return(q)
}


####################################################################
#' PostgreSQL Queries on Production Database (read-write)
#' 
#' This function lets the user query our Production Database
#' 
#' @family Connections
#' @param query Character. SQL Query
#' @param creds Character. Credential's user (see get_credentials)
#' @export
queryProduc = function(query, creds = NA) {

  options(warn=-1)

  dw <- get_credentials(from = "production", dir = creds)

  drv <- RPostgreSQL::PostgreSQL()
  con <- dbConnect(drv, host=dw$server, dbname=dw$database,
                   port=dw$port, user=dw$uid, password=dw$pwd)
  start <- Sys.time()
  q <- dbSendQuery(con, query)
  q <- fetch(q, n = -1)
  dbDisconnect(con)
  message(paste("Query duration:", round(difftime(Sys.time(), start, units="secs"), 2), "s"))
  return(q)
}
