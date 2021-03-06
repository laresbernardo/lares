% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/other_functions.R
\name{normalize}
\alias{normalize}
\title{Normalize Vector}
\usage{
normalize(x)
}
\arguments{
\item{x}{Numeric Vector. Numbers to be transformed into
normalized vector}
}
\value{
Vector with normalized \code{x} values
}
\description{
This function lets the user normalize numerical values into
the 0 to 1 range
}
\examples{
x <- c(0, 1, 4, 7.5, 10)
normalize(x)
}
\seealso{
Other Data Wrangling: 
\code{\link{balance_data}()},
\code{\link{categ_reducer}()},
\code{\link{cleanText}()},
\code{\link{date_cuts}()},
\code{\link{date_feats}()},
\code{\link{formatNum}()},
\code{\link{holidays}()},
\code{\link{impute}()},
\code{\link{left}()},
\code{\link{numericalonly}()},
\code{\link{ohe_commas}()},
\code{\link{ohse}()},
\code{\link{removenacols}()},
\code{\link{removenarows}()},
\code{\link{replaceall}()},
\code{\link{textFeats}()},
\code{\link{textTokenizer}()},
\code{\link{vector2text}()},
\code{\link{year_month}()},
\code{\link{year_week}()}
}
\concept{Data Wrangling}
