% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/other_functions.R
\name{json2vector}
\alias{json2vector}
\title{Convert Python JSON string to R vector (data.frame with 1 row)}
\usage{
json2vector(json)
}
\arguments{
\item{json}{Character. JSON string.}
}
\value{
List, data.frame, or vector. Depends on the \code{json} string.
}
\description{
This function lets the user transform a JSON string into vector
(data.frame with 1 row). You can also pass a Python's dictionary.
For any other JSON transformation, \code{jsonlite} is recommended.
}
\examples{
json2vector('{"id": 1, "nodata": null, "gender": "M"}')
}
\seealso{
Other Tools: 
\code{\link{autoline}()},
\code{\link{bind_files}()},
\code{\link{bring_api}()},
\code{\link{chr2num}()},
\code{\link{db_download}()},
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
}
\concept{Tools}
