% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_plots.R
\name{mplot_importance}
\alias{mplot_importance}
\title{Variables Importances Plot}
\usage{
mplot_importance(
  var,
  imp,
  colours = NA,
  limit = 15,
  model_name = NA,
  subtitle = NA,
  save = FALSE,
  subdir = NA,
  file_name = "viz_importance.png"
)
}
\arguments{
\item{var}{Vector. Variable or column's names}

\item{imp}{Vector. Importance of said variables. Must have same length as var}

\item{colours}{If positive and negative contribution is known}

\item{limit}{Integer. Limit how many variables you wish to plot}

\item{model_name}{Character. Model's name}

\item{subtitle}{Character. Subtitle to show in plot}

\item{save}{Boolean. Save output plot into working directory}

\item{subdir}{Character. Sub directory on which you wish to save the plot}

\item{file_name}{Character. File name as you wish to save the plot}
}
\value{
Plot with ranked importance variables results.
}
\description{
This function plots Variable Importances
}
\examples{
Sys.unsetenv("LARES_FONT") # Temporal
df <- data.frame(
  variable = LETTERS[1:6],
  importance = c(4, 6, 6.7, 3, 4.8, 6.2) / 100,
  positive = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)
)
head(df)

mplot_importance(
  var = df$variable,
  imp = df$importance,
  model_name = "Random values model"
)

# Add a colour for categories
mplot_importance(
  var = df$variable,
  imp = df$importance,
  colours = df$positive,
  limit = 4
)
}
\seealso{
Other ML Visualization: 
\code{\link{mplot_conf}()},
\code{\link{mplot_cuts}()},
\code{\link{mplot_cuts_error}()},
\code{\link{mplot_density}()},
\code{\link{mplot_full}()},
\code{\link{mplot_gain}()},
\code{\link{mplot_lineal}()},
\code{\link{mplot_metrics}()},
\code{\link{mplot_response}()},
\code{\link{mplot_roc}()},
\code{\link{mplot_splits}()},
\code{\link{mplot_topcats}()}
}
\concept{ML Visualization}
