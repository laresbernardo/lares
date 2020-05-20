####################################################################
#' Dataset: Titanic Sub-dataset por Examples
#'
#' Data generated from a Titanic dataset.
#'
#' @family Dataset
#' @docType data
#' @usage data(dft)
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
#' Dataset: Random Transactional Data
#'
#' Data randomly generated for examples.
#'
#' @family Dataset
#' @docType data
#' @usage data(dfl)
#' @format An object of class \code{"data.frame"}
#' @examples
#' data(dfl)
#' head(dfl)
"dfl"

# library(lares)
# library(dplyr)
# dfl <- queryDummy(
#   "SELECT random() as rand,
#   opp_id, opp_date, funnel_kind, motorcycle, 
#   policy_price, total_discount as promo, issued
#   FROM super_opps 
#   WHERE policy_price IS NOT NULL
#   AND date(opp_date) >= CURRENT_DATE - INTERVAL '30 days'
#   ORDER BY rand
#   LIMIT 1000") %>%
#   mutate(novar = "soat") %>%
#   mutate(kind = factor(ifelse(right(opp_id, 1) %in% c(1,5), NA, funnel_kind))) %>%
#   rename(values = policy_price, type = motorcycle) %>%
#   select(-rand, -funnel_kind) %>% 
# save(dfl, file="data/dfl.RData")

####################################################################
#' Dataset: Results for AutoML Predictions
#'
#' List with categorical (2 and 3 classes) and continuous predictions, 
#' generated with \code{h2o_automl()} and the \code{dft} 
#' Titanic dataset. Data.frames contain the actual values and model's 
#' predictions over the test set observations.
#'
#' @family Dataset
#' @docType data
#' @usage data(dfr)
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
# dft <- subset(dft, select = -c(Ticket, PassengerId, Cabin))
# # Classification: 2 classes
# class2 <- h2o_automl(dft, y = Survived, max_models = 1, target = "TRUE")
# # Classification: 3 classes
# class3 <- h2o_automl(dft, Pclass, ignore = "Fare", impute = TRUE)
# # Regression (Continuous Values)
# regr <- h2o_automl(dft, y = "Fare", ignore = c("Pclass","Cabin"), exclude_algos = NULL)
# # Save results
# dfr <- list(class2 = class2$scores_test,
#             class3 = class3$scores_test,
#             regr = regr$scores_test)
# save(dfr, file = "data/dfr.RData", version = 2)
