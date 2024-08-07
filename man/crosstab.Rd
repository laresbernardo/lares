% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crosstab.R
\name{crosstab}
\alias{crosstab}
\title{Weighted Cross Tabulation}
\usage{
crosstab(
  df,
  ...,
  wt = NULL,
  prow = FALSE,
  pcol = FALSE,
  pall = FALSE,
  decimals = 2,
  rm.na = FALSE,
  total = TRUE,
  order = TRUE
)
}
\arguments{
\item{df}{Data.frame.}

\item{...}{Variables. Dependent and independent variables.}

\item{wt}{Variable, numeric. Weights.}

\item{prow, pcol, pall}{Boolean. Calculate percent values for rows, columns,
or the whole table, respectively.}

\item{decimals}{Integer. How many decimals should be returned?}

\item{rm.na}{Boolean. Remove NA values?}

\item{total}{Boolean. Return total values column?}

\item{order}{Boolean. Sort columns and rows by frequencies? Else, will
be sorted alphabetically}
}
\value{
data.frame. Result of crossing the variables provided in \code{...} and
counting how many observations (rows) fall into each criteria.
}
\description{
A cross-tabulation function with output similar to STATA, tidy
friendly, with weighting possibility.
}
\examples{
data(dft) # Titanic dataset
crosstab(dft, Survived, Pclass, total = FALSE)
# Show values in percentages
crosstab(dft, Pclass, Survived, prow = TRUE)
crosstab(dft, Pclass, Survived, pall = TRUE)
# Weighted by another variable
crosstab(dft, Survived, Pclass, wt = Fare, prow = TRUE)
}
\seealso{
Other Exploratory: 
\code{\link{corr_cross}()},
\code{\link{corr_var}()},
\code{\link{df_str}()},
\code{\link{distr}()},
\code{\link{freqs}()},
\code{\link{freqs_df}()},
\code{\link{freqs_list}()},
\code{\link{freqs_plot}()},
\code{\link{lasso_vars}()},
\code{\link{missingness}()},
\code{\link{plot_cats}()},
\code{\link{plot_df}()},
\code{\link{plot_nums}()},
\code{\link{tree_var}()}
}
\concept{Exploratory}
