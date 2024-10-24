% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crossbrand.R
\name{robyn_crossmmm}
\alias{robyn_crossmmm}
\alias{print.robyn_crossmmm}
\title{Cross-MMM Budget Optimization}
\usage{
robyn_crossmmm(
  models,
  initial_budgets = NULL,
  start_dates = NULL,
  end_dates = NULL,
  budget_constr_low = 0.5,
  budget_constr_up = 1.5,
  channel_constr_low = budget_constr_low,
  channel_constr_up = budget_constr_up,
  cores = NULL,
  quiet = FALSE,
  ...
)

\method{print}{robyn_crossmmm}(x, ...)
}
\arguments{
\item{models}{Lists. Recreated Robyn models with \code{robyn_recreate()}.}

\item{initial_budgets}{Numeric vector. Default will use the total spends
per model for the specified or total date range.
Must be length 1 or same as \code{models}.}

\item{start_dates, end_dates}{Character vector. Start and end dates for each
specific model. You can specify a single date and will be used in all models.
Default empty value will assume you want all available data and date range.
Must be length 1 or same as \code{models}.}

\item{budget_constr_low, budget_constr_up}{Numeric vector. Relative minimum
and maximum budgets to consider based on \code{initial_budgets}.
By default it'll consider 50% and 150% budget constraints.
Must be length 1 or same as \code{models}.}

\item{channel_constr_low, channel_constr_up}{Numeric vector. Relative minimum
and maximum budgets to consider per channel.
By default it'll consider 50% and 150% channel constraints.
Must be length 1 or same as \code{models}.}

\item{cores}{Integer. How many cores to use for parallel computations?
Set to 1 to not use this option.
Default will the minimum between 10 cores and all available cores - 1.}

\item{quiet}{Boolean. Keep quiet? If not,
informative messages will be printed.}

\item{...}{Additional parameters to be passed to internal functions.}

\item{x}{Object to print: robyn_crossmmm() output.}
}
\value{
Invisible vector with results by letter.
}
\description{
Given a list of recreated Robyn models, this function optimizes budget
allocation across MMMs with respective constraints by maximizing
incremental revenue/conversions. This method
assumes each model is independent and can be compared given its
spends were cleanly and properly split.
}
\examples{
# You must have Robyn installed and some models stored as JSON files
\dontrun{
# Import and recreate the models
files <- c("BrandA.json", "BrandB.json", "BrandC.json", "BrandN.json")
models <- lapply(files, function(x) Robyn::robyn_recreate(x))
names(models) <- gsub("\\\\.json", "", files)

# Calculate cross-brand optimal allocation
res <- robyn_crossmmm(
  models,
  cores = 10,
  start_dates = "2023-01-01",
  end_dates = "2023-12-01"
)
print(res)
res$summary
}
}
