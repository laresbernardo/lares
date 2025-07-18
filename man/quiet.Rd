% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/other_functions.R
\name{quiet}
\alias{quiet}
\title{Quiet prints and verbose noise}
\usage{
quiet(fx, quiet = TRUE)
}
\arguments{
\item{fx}{Function to quiet}

\item{quiet}{Quiet outputs? If not, skip quietness.}
}
\value{
Same as \code{fx} but with no messages or prints.
}
\description{
This function silences (verbose) output prints. Thanks to Hadley Wickham
for bringing the idea.
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
\code{\link{json2vector}()},
\code{\link{list_cats}()},
\code{\link{listfiles}()},
\code{\link{mail_send}()},
\code{\link{markdown2df}()},
\code{\link{move_files}()},
\code{\link{msplit}()},
\code{\link{myip}()},
\code{\link{read.file}()},
\code{\link{statusbar}()},
\code{\link{tic}()},
\code{\link{try_require}()},
\code{\link{updateLares}()},
\code{\link{warnifnot}()},
\code{\link{what_size}()}
}
\concept{Tools}
