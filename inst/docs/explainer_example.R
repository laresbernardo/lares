library(lares)
library(tidyverse)

data(dft)

# TRAIN MODEL
dfm <- dft %>%
  select(-Ticket, -PassengerId, -Cabin) %>%
  rename("tag" = "Survived") %>%
  h2o_automl(max_time = 60)

# EXPLAINER
explainer <- dalex_explainer(df = dfm$datasets$test, 
                             model = dfm$model)
explainer$data <- na.omit(explainer$data)

# CATEGORICAL
class <- dalex_variable(explainer, y = "Pclass")
class$plot

# NUMERICAL
class <- dalex_variable(explainer, y = "Fare")
class$plot
# x <- ceteris_paribus(explainer, explainer$data, variables = "Fare")
# aux <- aggregate_profiles(x)
# plot(aux)

# LOCAL EXAMPLE
explainer$data[1,]
#local <- dalex_local(explainer, row = 1, plot = TRUE)
local <- dalex_local(explainer, observation = explainer$data[1,])
local$plot
