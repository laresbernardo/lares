% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_functions.R
\name{export_results}
\alias{export_results}
\title{Export h2o_automl's Results}
\usage{
export_results(
  results,
  thresh = 10,
  which = c("txt", "csv", "rds", "binary", "mojo", "plots", "dev", "production"),
  note = NA,
  subdir = NA,
  save = TRUE,
  seed = 0
)
}
\arguments{
\item{results}{\code{h2o_automl} or \code{h2o} model}

\item{thresh}{Integer. Threshold for selecting binary or regression
models: this number is the threshold of unique values we should
have in 'tag' (more than: regression; less than: classification)}

\item{which}{Character vector. Select which file format to export:
Possible values: txt, csv, rds, binary, mojo, plots. You might also
use dev (txt, csv, rds) or production (binary, mojo) or simply don't use
parameter to export everything}

\item{note}{Character. Add a note to the txt file. Useful when lots of
models are trained and saved to remember which one is which one}

\item{subdir}{Character. In which directory do you wish to save
the results?}

\item{save}{Boolean. Do you wish to save/export results?}

\item{seed}{Numeric. For reproducible results and random splits.}
}
\value{
No return value, called for side effects.
}
\description{
Export RDS, TXT, POJO, MOJO and all results from \code{h2o_automl()}.
}
\seealso{
Other Machine Learning: 
\code{\link{ROC}()},
\code{\link{conf_mat}()},
\code{\link{gain_lift}()},
\code{\link{h2o_automl}()},
\code{\link{h2o_predict_MOJO}()},
\code{\link{h2o_selectmodel}()},
\code{\link{impute}()},
\code{\link{iter_seeds}()},
\code{\link{lasso_vars}()},
\code{\link{model_metrics}()},
\code{\link{model_preprocess}()},
\code{\link{msplit}()}

Other Tools: 
\code{\link{autoline}()},
\code{\link{bind_files}()},
\code{\link{bring_api}()},
\code{\link{chr2num}()},
\code{\link{db_download}()},
\code{\link{db_upload}()},
\code{\link{dont_sleep}()},
\code{\link{export_plot}()},
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
}
\concept{Machine Learning}
\concept{Tools}
