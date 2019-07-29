library(lares)
library(dplyr)

data(dft)
dft <- dft %>% select(-Ticket, -PassengerId)

# Correlations
dft %>% corr_cross()
dft %>% corr_var(Survived)
dft %>% distr(Survived, Age, na.rm = TRUE)
dft %>% distr(Survived, Pclass, abc = TRUE)

# Classification: 2 class
r <- dft %>% h2o_automl(y = "Survived", max_models = 4, impute = TRUE)
plot(r$plots$dashboard)
r$metrics

# Classification: 3 classes
r <- dft %>% select(-Fare) %>% h2o_automl(y = "Pclass", max_models = 3)
plot(r$plot$dashboard)
r$metrics

# Regression
r <- dft %>% h2o_automl(y = "Fare", max_models = 3)
plot(r$plot$dashboard)
r$metrics

# Variables importances for any model
r$plots$importance

####### OTHER EXAMPLE
data(dfl)
r <- dfl %>% ohse(dates = T) %>% 
  select(-opp_date, -opp_id) %>% 
  h2o_automl("issued", max_models = 5, balance = TRUE)
r$metrics
