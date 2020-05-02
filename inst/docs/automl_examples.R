library(lares)

data(dft)
dft <- subset(dft, select = -c(Ticket, PassengerId, Cabin))

# Classification: 2 class
r <- h2o_automl(dft, y = "Survived", max_models = 1, target = "TRUE")
r$plots$dashboard
r$metrics

# Classification: 3 classes
r <- h2o_automl(dft, y = "Pclass", ignore = "Fare", impute = TRUE)
r$plot$dashboard
r$metrics

# Regression (Continuous Values)
r <- h2o_automl(dft, y = "Fare", ignore = c("Pclass","Cabin"), exclude_algos = NULL)
r$plot$dashboard
r$metrics

# Variables importances for any model
r$plots$importance

# WITH PRE-DEFINED TRAIN/TEST DATAFRAMES
splits <- msplit(dft, size = 0.8)
splits$train$split <- "train"
splits$test$split <- "test"
df <- rbind(splits$train, splits$test)
r <- h2o_automl(df, "Survived", max_models = 3, train_test = "split")
