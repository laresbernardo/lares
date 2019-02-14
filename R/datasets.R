#' Random Lares Data for Examples
#'
#' Data randomly generated for examples.
#'
#' @docType data
#' @usage data(dfl)
#' @format An object of class \code{"data.frame"}
#' @keywords datasets
#' @examples
#' data(dfl)
#' df_str(dfl)
"dfl"

# library(lares)
# library(dplyr)
# 
# df <- queryDummy(
#   "SELECT random() as rand,
#   opp_id, opp_date, funnel_kind, motorcycle, 
#   policy_price, total_discount as promo, issued
#   FROM super_opps 
#   WHERE policy_price IS NOT NULL
#   AND date(opp_date) >= CURRENT_DATE - INTERVAL '30 days'
#   ORDER BY rand
#   LIMIT 1000")
# dfl <- df %>% 
#   mutate(novar = "soat") %>%
#   mutate(kind = factor(ifelse(right(opp_id, 1) %in% c(1,5), NA, funnel_kind))) %>%
#   rename(values = policy_price, type = motorcycle) %>%
#   select(-rand, -funnel_kind)
# df_str(dfl, return = "plot")
# head(dfl)
# 
# dfl %>% corr_var(issued, top = 10)
# dfl %>% distr(issued, promo)
# dfl %>% distr(issued, kind)
# dfl %>% distr(issued, values, abc=T)
# dfl %>% distr(issued, type, abc=T)
# 
# save(dfl, file="data/dfl.RData")
