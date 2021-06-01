# lares <img src='man/figures/lares_logo.png' align="right" height="140px" />
### R Package for Analytics and Machine Learning
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/lares)](https://cran.r-project.org/package=lares) [![R-CMD-check](https://github.com/laresbernardo/lares/workflows/R-CMD-check/badge.svg?branch=master)](https://github.com/laresbernardo/lares/actions?query=workflow%3AR-CMD-check) [![documentation](https://github.com/laresbernardo/lares/workflows/documentation/badge.svg)](https://laresbernardo.github.io/lares/reference/index.html) [![CodeFactor](https://www.codefactor.io/repository/github/laresbernardo/lares/badge)](https://www.codefactor.io/repository/github/laresbernardo/lares) [![saythanks](https://img.shields.io/badge/say-hi-blue.svg)](https://www.linkedin.com/in/laresbernardo)
----

R library built to automate, improve, and speed everyday **Analysis and Machine Learning** tasks. With a wide variety of family functions like Machine Learning, data cleaning and procesing, EDA, Investment, NLP, Queries, Scrappers, API interactions, `lares` helps the analyst or data scientist get quick, reproducible, robust results, without the need of repetitive coding nor extensive programming skills. Feel free to install, use, and/or comment on any of the code and functionalities. And if you are also colourblind, be sure to check the the colour palettes!

Don't hesitate to contact me, and please when you do, let me know where did you first hear from the library and which family of functions you are most interested in.

## Installation

```{r}
## CRAN VERSION
install.packages("lares")

## DEV VERSION
# If you don't have remotes yet, run: install.packages('remotes')
remotes::install_github("laresbernardo/lares")
# Full installation with recommended dependencies (takes more time)
remotes::install_github("laresbernardo/lares", dependencies = TRUE)
```

**Windows users**: you MAY have to install `RTools` before installing the dev version. Download it [here](https://cran.r-project.org/bin/windows/Rtools/).

## See the library in action!

- DataScience+: [Visualizations for Classification Models Results](https://datascienceplus.com/machine-learning-results-one-plot-to-rule-them-all/)

- DataScience+: [Visualizations for Regression Models Results](https://datascienceplus.com/machine-learning-results-in-r-one-plot-to-rule-them-all-part-2-regression-models)

- DataScience+: [AutoML and DALEX for Dataset Understanding](https://datascienceplus.com/understanding-titanic-dataset-with-h2os-automl-dalex-and-lares-library/)

- DataScience+: [Find Insights with Ranked Cross-Correlations](https://datascienceplus.com/find-insights-with-ranked-cross-correlations/)

- DataScience+: [Portfolio's Performance and Reporting](https://datascienceplus.com/visualize-your-portfolios-performance-and-generate-a-nice-report-with-r)

- DataScience+: [Manage Credentials Safely in R](https://datascienceplus.com/how-to-manage-credentials-and-secrets-safely-in-r/)

- DataScience+: [Plot Timelines with Gantt Charts](https://datascienceplus.com/visualize-your-cvs-timeline-with-r-gantt-style/)

- RPubs: [freqs(), distr(), and corr_var() Examples](http://rpubs.com/laresbernardo/freqs-distr-corr)

### AutoML Simplified Map from [`h2o_automl()`](https://laresbernardo.github.io/lares/reference/h2o_automl.html)
![AutoML Map (lares)](man/figures/automl_map.png?raw=true)

### Insights While Understanding
To get insights and value out of your dataset, first you need to understand its structure, types of data, empty values, interactions between variables... `corr_cross()` and `freqs()` are here to give you just that! They show a wide perspective of your dataset content, correlations, and frequencies. Additionally, with the `missingness()` function to detect all missing values and `df_str()` to break down you data frame's structure, you will be ready to squeeze valuable insights out of your data. If you'd like to go deeper, try the `x2y()`, `lasso_vars()` and `tree_var()` to use Machine Learning to detect patterns, predictive powers, and variables importance.

![Cross-Correlations and Frequencies (lares)](man/figures/titanic_df.png?raw=true)

### Kings of Data Mining
My favourite and most used functions are `freqs()`, `distr()`, and `corr_var()`. In this [RMarkdown](http://rpubs.com/laresbernardo/freqs-distr-corr) you can see them in action. Basically, they group and count values within variables, show distributions of one variable vs another one (numerical or categorical), and calculate/plot correlations of one variables vs all others, no matter what type of data you insert. 

If there is space for one more, I would add `ohse()` (One Hot Smart Encoding), which has made my life much easier and my work much valuable. It converts a whole data frame into numerical values by making dummy variables (categoricals turned into new columns with 1s and 0s, ordered by frequencies and grouping less frequent into a single column) and dates into new features (such as month, year, week of the year, minutes if time is present, holidays given a country, currency exchange rates, etc).

## What else is there? 

You can check all active functions and documentations [here](https://laresbernardo.github.io/lares/reference/index.html) or type `lares::` in **RStudio** and you will get a pop-up with all the functions that are currently available within the package. You might also want to check the whole documentation by running `help(package = "lares")` in your RStudio or in the [Online Official Documentation](https://laresbernardo.github.io/lares/reference/index.html). Remember to check the families and similar functions on the **See Also** sections as well.

### Getting further help

If you need help with any of the functions when using RStudio, use the `?` function (i.e. `?lares::function`) and the **Help** tab will display a short explanation on each function and its parameters. You might also be interested in the [online documentation](https://laresbernardo.github.io/lares/reference/index.html) to check all functions and parameters.

If you encounter a bug, please share with me a reproducible example on [Github issues](https://github.com/laresbernardo/lares/issues) and I'll take care of it. For inquiries, and other matters, you can [LinkedIn me](https://www.linkedin.com/in/laresbernardo/) anytime!
