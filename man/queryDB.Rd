% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/querieSQL.R
\name{queryDB}
\alias{queryDB}
\title{PostgreSQL Queries on Database (Read)}
\usage{
queryDB(query, from, creds = NA)
}
\arguments{
\item{query}{Character. SQL Query}

\item{from}{Character. Credential's user (see \code{get_creds()})}

\item{creds}{Character. Credential's directory (see \code{get_creds()})}
}
\value{
data.frame. Result of fetching the \code{query} data.
}
\description{
This function lets the user query a PostgreSQL database. Previously
was called \code{queryDummy} but was replaced and deprecated for a
more general function by using the \code{from} parameter.
}
\seealso{
Other Credentials: 
\code{\link{db_download}()},
\code{\link{db_upload}()},
\code{\link{encrypt_file}()},
\code{\link{get_credentials}()},
\code{\link{get_tweets}()},
\code{\link{mail_send}()},
\code{\link{queryGA}()},
\code{\link{slackSend}()},
\code{\link{stocks_file}()}
}
\concept{Credentials}
\concept{Database}
