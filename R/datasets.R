####################################################################
#' Titanic Dataset
#'
#' The sinking of the Titanic is one of the most infamous shipwrecks in history.
#' On April 15, 1912, during her maiden voyage, the widely considered "unsinkable"
#' RMS Titanic sank after colliding with an iceberg. Unfortunately, there weren’t
#' enough lifeboats for everyone onboard, resulting in the death of 1502 out of
#' 2224 passengers and crew. While there was some element of luck involved in
#' surviving, it seems some groups of people were more likely to survive than others.
#' This dataset contains the details of a subset of the passengers on board
#' (891 to be exact) taken from Kaggle's Titanic
#' \href{https://www.kaggle.com/c/titanic/overview}{Train.csv}.
#'
#' @family Dataset
#' @docType data
#' @usage data(dft)
#' @return data.frame
#' @format An object of class \code{"data.frame"}
#' \describe{
#'   \item{PassengerId}{Unique ID for each passenger (1-891)}
#'   \item{Survived}{Did the passenger survive? (TRUE, FALSE)}
#'   \item{Pclass}{Ticket class, from first to third (1, 2, 3)}
#'   \item{Sex}{Gender (female, male)}
#'   \item{Age}{Age for each passenger in years (0.42-80)}
#'   \item{SibSp}{Amount of siblings / spouses aboard the Titanic (0-8)}
#'   \item{Parch}{Amount of parents / children aboard the Titanic (0-6)}
#'   \item{Ticket}{Ticket IDs}
#'   \item{Fare}{Amount paid for passenger's ticket (0-512.3292)}
#'   \item{Cabin}{width of top of diamond relative to widest point (43-95)}
#'   \item{Embarked}{Port of Embarkation (43-95)}
#' }
#' @examples
#' data(dft)
#' head(dft)
"dft"

# dft <- read.csv("~/Dropbox (Personal)/Documentos/R/R Titanic/train.csv") %>%
#   mutate(Pclass = factor(Pclass),
#          Sex = factor(Sex),
#          Survived = as.logical(Survived),
#          Ticket = as.character(Ticket)) %>%
#   select(-Name)
# save(dft, file = "data/dft.RData")


####################################################################
#' Results for AutoML Predictions
#'
#' List with categorical (2 and 3 classes) and continuous predictions,
#' generated with \code{h2o_automl()} and the \code{dft}. Note that
#' the models per se won't work to predict.
#'
#' @family Dataset
#' @docType data
#' @usage data(dfr)
#' @return List
#' @format An object of class \code{"list"} with 3 \code{"data.frame"}
#' \describe{
#'   \item{class2}{Predictions for a Binomial Classification Model}
#'   \item{class3}{Predictions for a Multi-Categorical Classification Model}
#'   \item{regr}{Predictions for a Continuous Regression Model}
#' }
#' @examples
#' data(dfr)
#' lapply(dfr, head)
"dfr"

# data(dft)
# df <- subset(dft, select = -c(Ticket, PassengerId, Cabin))
# # Classification: 2 classes
# class2 <- h2o_automl(df, y = Survived, max_models = 1, target = "TRUE", start_clean = TRUE)
# # Classification: 3 classes
# class3 <- h2o_automl(df, Pclass, ignore = "Fare", impute = TRUE, start_clean = FALSE)
# # Regression (Continuous Values)
# regr <- h2o_automl(df, y = "Fare", ignore = c("Pclass","Cabin"), exclude_algos = NULL, start_clean = FALSE)
# # Save results
# dfr <- list(class2 = class2$scores_test,
#             class3 = class3$scores_test,
#             regr = regr$scores_test)
# save(dfr, file = "data/dfr.RData", version = 2)
