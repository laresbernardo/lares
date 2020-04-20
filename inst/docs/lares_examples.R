# Code Shared: https://codeshare.io/5Zk8Dg

library(tidyverse)
library(lares) # devtools::install_github("laresbernardo/lares")

data(dft) # Titanic subset 
df_str(dft)
head(dft)

# FREQUENCIES (freqs): group, count, calculate percentages, cumulatives (and plot)
?freqs
# How many survived?
dft %>% freqs(Survived)
# How many survived and see plot?
dft %>% freqs(Survived, plot = T)
# How many survived per class?
dft %>% freqs(Survived, Pclass, plot = T)
# Per class, how many survived?
dft %>% freqs(Pclass, Survived, plot = T)
# Per sex and class, how many survived?
dft %>% freqs(Sex, Pclass, Survived, plot = T)
# Per number of siblings, sex, and class, how many survived?
dft %>% freqs(SibSp, Sex, Pclass, Survived, plot = T)
dft %>% freqs(SibSp, Sex, Pclass, Survived, plot = F)
# Frequency of tickets
dft %>% freqs(Ticket, plot = T)
# Frequency of tickets: show me more
dft %>% freqs(Ticket, plot = T, top = 10)
# Let's customize the plot a bit....
dft %>% 
  mutate(Survived = ifelse(Survived == 1, "Did survive", "Did not survive")) %>%
  freqs(Pclass, Survived, plot = T, 
        title = "People who survived the Titanic by Class",
        subtitle = paste("MEG | Keeping up con los datos:", Sys.Date()))

# There's also freqs_plot function
freqs_plot(dft, Pclass)
freqs_plot(dft, Pclass, Survived)
freqs_plot(dft, Pclass, Survived, Sex)
freqs_plot(dft, Pclass, Survived, Sex, Embarked)
freqs_plot(dft, Pclass, Survived, Sex, Embarked, top = 15)

# DISTRIBUTIONS (distr): compare the distribution of a target variable vs another variable
?distr
# Relation for survived vs sex
dft %>% distr(Survived, Sex)
# Relation for sex vs survived
dft %>% distr(Sex, Survived)
# Relation for survived vs embark gate (categorical)
dft %>% distr(Survived, Embarked)
dft %>% distr(Embarked, Survived)
# Relation for survived vs embark gate
dft %>% distr(Survived, Fare)
# Relation for survived vs fare with custom colours for Survived
dft %>% distr(Survived, Fare, custom_colours = T)
# Relation for survived vs fare with ascending order
dft %>% distr(Survived, Fare, abc = T)
# Relation for survived vs fare with only 5 splits
dft %>% distr(Survived, Fare, abc = T, breaks = 5)
dft %>% distr(Survived, Fare, abc = T, top = 5)
dft %>% distr(Survived, Embarked, top = 2)
# Relation for survived vs age (notice NA values)
dft %>% distr(Survived, Age)
dft %>% distr(Survived, Age, na.rm = T, abc = T)
dft %>% distr(Survived, Ticket, top = 10)
# Distribution of fares payed
dft %>% distr(Fare)
dft %>% filter(Fare < 200) %>% distr(Fare)
# Distribution (frequency) of survivors
dft %>% distr(Survived, force = "char")
# Distribution of log(Fare) vs Fare
dft %>% mutate(logFare = log(Fare)) %>% distr(Fare, logFare)
dft %>% mutate(logFare = log(Fare)) %>% 
  filter(Fare < 100) %>% distr(Fare, logFare) + 
  geom_point(colour = "yellow")
# Only one plot or the other
dft %>% distr(Survived, Age, type = 2)
dft %>% distr(Survived, Age, type = 3)

# BONUS!!!
# CORRELATION VARIABLE (corr_var): correlates a whole dataframe with a single feature
?corr_var
# Correlate Survived with everything else
dft %>% corr_var(Survived)
# Filter out variables with more than 50% of correlation
dft %>% corr_var(Survived_TRUE, ceiling = 50)
# Show only 10 values
dft %>% corr_var(Survived_TRUE, top = 10)
# Also calculate log(values)
dft %>% corr_var(Survived_TRUE, logs = T)

# ALSO:
# You can save and export a PNG file for each generated plot using:
# 'save = TRUE' and in any folder, using 'subdir = "~/"'
