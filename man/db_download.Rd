% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dropbox.R
\name{db_download}
\alias{db_download}
\title{Download/Import Dropbox File by File's Name}
\usage{
db_download(
  query,
  local_path = NULL,
  xlsx = TRUE,
  token_dir = NA,
  token_name = "token_pers.rds",
  quiet = FALSE
)
}
\arguments{
\item{query}{Search string. This string is split (on spaces) into
individual words. Files will be used if they contain
all words in the search string.}

\item{local_path}{Character. Path to save file to. If \code{NULL}
(the default), saves file to working directory with same name.
If not, but a valid folder, file will be saved in this folder with
same basename as path. If not \code{NULL} and not a folder,
file will be saved to this path exactly.}

\item{xlsx}{Boolean. Is it an Excel file? Can be returned as a list
for each tab and not as a file if needed. Will delete downloaded file.}

\item{token_dir}{Character. RDS with token local directory. You may set to
NA if you already set your credentials (see \code{get_creds()})}

\item{token_name}{Character. RDS file name with your token's data.}

\item{quiet}{Boolean. Keep quiet? If not, informative messages will be shown.}
}
\value{
If \code{query} returns a .xlsx file and \code{xlsx=TRUE}, will
return a data.frame. Else, \code{local_path} string.
}
\description{
This function lets the user download a file from Dropbox, specifying
its name, using a previously created token or with interactive window.
}
\examples{
\dontrun{
# Download a specific file
db_download("stocksReport.Rmd", local_path = "~/Desktop/generic.Rmd")
# Import an Excel file from Dropbox into a data.frame
df <- db_download("Portfolio LC.xlsx", xlsx = FALSE)
}
}
\seealso{
Other Tools: 
\code{\link{autoline}()},
\code{\link{bind_files}()},
\code{\link{bring_api}()},
\code{\link{chr2num}()},
\code{\link{db_upload}()},
\code{\link{dont_sleep}()},
\code{\link{export_plot}()},
\code{\link{export_results}()},
\code{\link{files_functions}()},
\code{\link{font_exists}()},
\code{\link{formatColoured}()},
\code{\link{formatHTML}()},
\code{\link{glued}()},
\code{\link{grepm}()},
\code{\link{h2o_selectmodel}()},
\code{\link{haveInternet}()},
\code{\link{image_metadata}()},
\code{\link{importxlsx}()},
\code{\link{ip_data}()},
\code{\link{json2vector}()},
\code{\link{list_cats}()},
\code{\link{listfiles}()},
\code{\link{mail_send}()},
\code{\link{markdown2df}()},
\code{\link{move_files}()},
\code{\link{msplit}()},
\code{\link{myip}()},
\code{\link{quiet}()},
\code{\link{read.file}()},
\code{\link{statusbar}()},
\code{\link{tic}()},
\code{\link{try_require}()},
\code{\link{updateLares}()},
\code{\link{warnifnot}()},
\code{\link{what_size}()}

Other Credentials: 
\code{\link{db_upload}()},
\code{\link{encrypt_file}()},
\code{\link{get_credentials}()},
\code{\link{get_tweets}()},
\code{\link{mail_send}()},
\code{\link{queryDB}()},
\code{\link{queryGA}()},
\code{\link{slackSend}()},
\code{\link{stocks_file}()}

Other Dropbox: 
\code{\link{db_upload}()}
}
\concept{Credentials}
\concept{Dropbox}
\concept{Tools}
