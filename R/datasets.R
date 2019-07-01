####################################################################
#' Dataset: Titanic Sub-dataset por Examples
#'
#' Data generated from a Titanic dataset.
#'
#' @docType data
#' @usage data(dft)
#' @format An object of class \code{"data.frame"}
#' @keywords datasets titanic
#' @examples
#' data(dft)
"dft"

# dft <- read.csv("~/Dropbox (Personal)/Documentos/R/R Titanic/train.csv") %>%
#   mutate(Pclass = factor(Pclass),
#          Sex = factor(Sex),
#          Survived = as.logical(Survived),
#          Ticket = as.character(Ticket)) %>%
#   select(-Name)
# save(dft, file = "data/dft.RData")



####################################################################
#' Dataset: Random Data for Examples
#'
#' Data randomly generated for examples.
#'
#' @docType data
#' @usage data(dfl)
#' @format An object of class \code{"data.frame"}
#' @keywords datasets random
#' @examples
#' data(dfl)
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
