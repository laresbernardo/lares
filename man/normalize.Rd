% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangling.R
\name{normalize}
\alias{normalize}
\title{Normalize Vector}
\usage{
normalize(x, range = c(0, 1), ...)
}
\arguments{
\item{x}{Numeric vector. The numbers to be normalized.}

\item{range}{A numeric vector of length 2 specifying the desired range
for normalization. Default is c(0, 1).}

\item{...}{Additional parameters.}
}
\value{
A numeric vector with normalized \code{x} values.
}
\description{
This function normalizes numerical values into a specified range,
defaulting to the 0 to 1 range.
}
\examples{
x <- c(0, 1, 4, 7.5, 10)
normalize(x)
normalize(x, range = c(-1, 1))
}
\seealso{
Other Data Wrangling: 
\code{\link{balance_data}()},
\code{\link{categ_reducer}()},
\code{\link{cleanText}()},
\code{\link{date_cuts}()},
\code{\link{date_feats}()},
\code{\link{file_name}()},
\code{\link{formatHTML}()},
\code{\link{holidays}()},
\code{\link{impute}()},
\code{\link{left}()},
\code{\link{num_abbr}()},
\code{\link{ohe_commas}()},
\code{\link{ohse}()},
\code{\link{quants}()},
\code{\link{removenacols}()},
\code{\link{replaceall}()},
\code{\link{replacefactor}()},
\code{\link{textFeats}()},
\code{\link{textTokenizer}()},
\code{\link{vector2text}()},
\code{\link{year_month}()},
\code{\link{zerovar}()}
}
\concept{Data Wrangling}
