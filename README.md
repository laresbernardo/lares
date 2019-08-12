# lares <img src='man/figures/lares_logo.png' align="right" height="140px" />
### R Package for Analytics and Machine Learning
[![Build Status](https://travis-ci.com/laresbernardo/lares.svg?branch=master)](https://travis-ci.com/laresbernardo/lares) [![Rdoc](http://www.rdocumentation.org/badges/version/lares)](http://www.rdocumentation.org/packages/lares) [![saythanks](https://img.shields.io/badge/say-thanks-1EAEDB.svg)](https://saythanks.io/to/laresbernardo)
----

`lares` is a library designed to automate, improve, and speed everyday **Analysis and Machine Learning** tasks. With a wide variety of family functions within Machine Learning, Data Wrangling, EDA, and Scrappers, `lares` helps the analyst or data scientist to get quick, reproducible, and robust results, without the need of repetitive coding or extensive programming skills.

You are most welcome to install, use, and/or comment on any of the code and functionalities. If you are colour blind as well, glad to share my colour palettes! Feel free to contact me via [Linkedin](https://www.linkedin.com/in/laresbernardo/), and please, do let me know where did you got my contact from.

## Installation

```{r}
# install.packages('devtools')
devtools::install_github("laresbernardo/lares")
```

**CRAN NOTE**: I currently don't have planned to submit the library into CRAN, eventhough it passes all its quality tests (and I'm a huge fan). I think `lares` is more of an everyday useful *package* rather than a "specialized for a specific task" *library*. It has too many useful and various kinds of functions, from NLP to querying APIs to plotting Machine Learning results to market stocks and portfolio reports. I gladly share my code with the community and encourage you to use/comment/share it, but I strongly think that CRAN is not aiming for this kind of libraries in their repertoire.

## See the library in action!

- DataScience+: [Visualizations for Classification Models Results](https://datascienceplus.com/machine-learning-results-one-plot-to-rule-them-all)

- DataScience+: [Visualizations for Regression Models Results](https://datascienceplus.com/machine-learning-results-in-r-one-plot-to-rule-them-all-part-2-regression-models)

- DataScience+: [AutoML and DALEX for Dataset Understanding](https://datascienceplus.com/understanding-titanic-dataset-with-h2os-automl-dalex-and-lares-library)

- DataScience+: [Portfolio's Performance and Reporting](https://datascienceplus.com/visualize-your-portfolios-performance-and-generate-a-nice-report-with-r)

- DataScience+: [Plot Timelines with Gantt Charts](https://datascienceplus.com/visualize-your-cvs-timeline-with-r-gantt-style/)

### AutoML Simplified Map from `h2o_automl()`
![AutoML Map (lares)](man/figures/automl_map.png?raw=true)

### Insights While Understanding
To get insights and value out of your dataset, first you need to understand its structure, types of data, empty values, interactions between variables... `corr_cross()` and `freqs()` are here to give you just that! They show a wide persepective of your dataset content, correlations, and frequencies. Additionally, with the `missingness()` function to detect all missing values and `df_str()` to break down you data frame's structure, you will be ready to squeeze valuable insights out of your data.
![Cross-Correlations and Frequencies (lares)](man/figures/titanic_df.png?raw=true)

### Kings of Data Mining
My favourite and most used functions are `freqs()`, `distr()`, and `corr_var()`. In this [RMarkdown](http://rpubs.com/laresbernardo/freqs-distr-corr) you can see them in action. Basically, they group and count values within variables, show distributions of one variable vs another one (numerical or categorical), and calculate/plot correlations of one variables vs all others, no matter what type of data you insert. 

If there is space for one more, I would add `ohse()` (One Hot Smart Encoding), which has made my life much easier and my work much valuable. It converts a whole data frame into numerical values by making dummy variables (categoricals turned into new columns with 1s and 0s, ordered by frequencies and grouping less frequent into a single column) and dates into new features (such as month, year, week of the year, minutes if time is present, holidays given a country, currency exchange rates, etc).

## What else is there?

You can type `lares::` in **RStudio** and you will get a pop-up with all the functions that are currently available within the package. You might also want to check the whole documentation by running `help(package = "lares")` locally or in the [rdrr.io](https://rdrr.io/github/laresbernardo/lares/man/) or [rdocumentation.org](https://www.rdocumentation.org/packages/lares/versions/4.7) online websites. Remember to check the families and similar functions on the **See Also** sections.

### Getting further help

If you need help with any of the functions, use the `?` function (i.e. `?lares::function`) and the **Help** tab will display a short explanation on each function and its parameters. 

If you encounter a bug, please share with me a reproducible example on [Github issues](https://github.com/laresbernardo/lares/issues) and I'll take care of it. For inquiries, and other matters, you can [email me](mailto:laresbernardo@gmail.com "email me") directly or open a new ticket [here](https://github.com/laresbernardo/lares/issues).
