####################################################################
#' Dataset: Titanic Sub-dataset por Examples
#'
#' Data generated from a Titanic dataset.
#'
#' @family Dataset
#' @docType data
#' @usage data(dft)
#' @format An object of class \code{"data.frame"}
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
#' @format An object of class \code{"list"} and length = 3
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
# dfr <- list(class2 = r$class2$scores_test,
#             class3 = r$class3$scores_test,
#             regr = r$regr$scores_test)
# save(dfr, file="data/dfr.RData")
