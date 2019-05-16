library(lares)
library(dplyr)

data(dft)
dft <- dft %>% select(-Ticket, -PassengerId)

# Correlations
dft %>% corr_var(Survived)
dft %>% distr(Survived, Age, na.rm = T)
dft %>% distr(Survived, Pclass, abc=T)

# Classification: 2 class
r <- dft %>% h2o_automl(y = "Survived", max_models = 3)
r$metrics

# Classification: 3 classes
r <- dft %>% select(-Fare) %>% h2o_automl(y = "Pclass", max_models = 3)
r$metrics

# Regression
r <- dft %>% h2o_automl(y = "Fare", max_models = 3)
r$metrics

# Full Results plots
mplot_full(r$scores_test$tag, r$scores_test$score)
mplot_full(r$scores_test$tag, r$scores_test$score, select(r$scores_test, -tag, -score))
mplot_importance(r$importance$variable, r$importance$importance)

# OTHER EXAMPLE
data(dfl)
r <- dfl %>% ohse(dates = T) %>% 
  select(-opp_date, -opp_id) %>% 
  h2o_automl("issued", max_models = 3)
