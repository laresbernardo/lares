% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/other_functions.R
\name{date_cuts}
\alias{date_cuts}
\title{Convert Date into Year-Month (YYYY-MM)}
\usage{
date_cuts(date, type = "Q")
}
\arguments{
\item{date}{Date. Date we wish to transform}

\item{type}{Character. Any of the following: B (2 months),
Q (3 months), T (4 months), H (6 months)}
}
\description{
This function returns categorical values for any date(s) using year
cuts such as bimonths, quarters, terms, and halves.
}
\seealso{
Other Data Wrangling: 
\code{\link{balance_data}()},
\code{\link{calibrate}()},
\code{\link{categ_reducer}()},
\code{\link{cleanText}()},
\code{\link{date_feats}()},
\code{\link{dateformat}()},
\code{\link{formatNum}()},
\code{\link{formatTime}()},
\code{\link{holidays}()},
\code{\link{impute}()},
\code{\link{left}()},
\code{\link{normalize}()},
\code{\link{numericalonly}()},
\code{\link{ohse}()},
\code{\link{one_hot_encoding_commas}()},
\code{\link{rbind_full}()},
\code{\link{removenacols}()},
\code{\link{removenarows}()},
\code{\link{replaceall}()},
\code{\link{right}()},
\code{\link{textFeats}()},
\code{\link{textTokenizer}()},
\code{\link{vector2text}()},
\code{\link{year_month}()},
\code{\link{year_week}()}
}
\concept{Data Wrangling}