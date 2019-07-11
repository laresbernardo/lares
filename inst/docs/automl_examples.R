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
mplot_full(r$scores_test$tag, r$scores_test$score)
r$metrics

# Classification: 3 classes
r <- dft %>% select(-Fare) %>% h2o_automl(y = "Pclass", max_models = 3)
mplot_full(r$scores_test$tag, r$scores_test$score, select(r$scores_test, -tag, -score))
r$metrics

# Regression
r <- dft %>% h2o_automl(y = "Fare", max_models = 3)
mplot_full(r$scores_test$tag, r$scores_test$score)
r$metrics

# Variables importances for any model
mplot_importance(r$importance$variable, r$importance$importance)


# OTHER EXAMPLE
data(dfl)
r <- dfl %>% ohse(dates = T) %>% 
  select(-opp_date, -opp_id) %>% 
  h2o_automl("issued", max_models = 5, balance = TRUE)
r$metrics

library(ggplot2)
splits <- 10
quiet = FALSE
gains <- gain_lift(r$scores_test$tag, r$scores_test$scores, "auto", splits, quiet = quiet) 

gains %>%
  mutate(percentile = as.numeric(percentile)) %>%
  ggplot(aes(x = percentile)) + 
  # Random line
  geom_line(aes(y = random, linetype = "Random"), colour = "black", alpha = 0.6) +
  # Optimal line
  geom_line(aes(y = optimal, linetype = "Optimal"), colour = "black", alpha = 0.6) +
  # Model line
  geom_line(aes(y = gain), colour = "darkorange", size = 1.2) +
  geom_label(aes(y = gain, label = ifelse(gain == 100, NA, round(gain))), alpha = 0.9) +
  scale_y_continuous(breaks = seq(0, 100, 10)) + guides(colour = FALSE) +
  scale_x_continuous(minor_breaks = NULL, breaks = seq(0, splits, 1)) +
  labs(title = "Cumulative Gains Plot", linetype = NULL,
       y = "Cumulative gains [%]", 
       x = paste0("Percentiles [",splits,"]")) +
  theme_lares2(pal = 2) + theme(legend.position = c(0.88, 0.2))
