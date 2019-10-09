library(lares)
library(dplyr)

data(dft)
dft <- dft %>% select(-Ticket, -PassengerId, -Cabin)

# Classification: 2 class
r <- dft %>% h2o_automl(y = "Survived", max_models = 2, exclude_algos = NULL)
plot(r$plots$dashboard)
r$metrics

# Classification: 3 classes
r <- dft %>% select(-Fare) %>% h2o_automl(y = "Pclass", impute = TRUE)
plot(r$plot$dashboard)
r$metrics

# Regression
r <- dft %>% h2o_automl(y = "Fare", ignore = c("Pclass","Cabin"))
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

####### WITH PRE-DEFINED TRAIN/TEST
splits <- msplit(dft)
df <- rbind(mutate(splits$train, train_test = "train"),
            mutate(splits$test, train_test = "test"))
r <- h2o_automl(df, "Survived", max_models = 3, train_test = "train_test")
