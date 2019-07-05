# lares <img src='man/figures/lares_logo.png' align="right" height="140px" />
### R Package for Analytics and Machine Learning
[![Build Status](https://travis-ci.com/laresbernardo/lares.svg?branch=master)](https://travis-ci.com/laresbernardo/lares) [![saythanks](https://img.shields.io/badge/say-thanks-1EAEDB.svg)](https://saythanks.io/to/laresbernardo)
----

`lares` is a library used to automate, improve, and speed everyday **Analysis and Machine Learning** tasks. It also simplifies the installation process of libraries, distribution, and reproducible research. You are most welcome to install, use, and comment on any of its code and functionalities. If you are colour blind as well, glad to share my colour palettes! Feel free to contact me and/or add me on Linkedin, and please, do let me know where did you got my contact from!

## Install the package

```{r}
devtools::install_github("laresbernardo/lares")
```
  
I currently don't have planned to post `lares` library to CRAN yet, but who knows what the future brings!

### Update the package
```{r}
updateLares() # From Github repository (Bitbucket discontinued)
updateLares(local = TRUE) # Personal and local files, not necessarily deployed
```

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

You can type `lares::` in **RStudio** and you will see a pop-up with all the functions that are currently available within the package. If in doubt, you can use the `?` function (i.e. `?lares::function`) and the **Help** tab will display a short explanation on each function and its parameters. If you want to check all the documentation, simply run `help(package = lares)`. You can find similar family functions in the **See Also** section of each documentation as well.

### Getting further help

If you encounter a clear bug, please share with us a reproducible example on [Github](https://github.com/laresbernardo/lares/issues) and I'll take care of it. For inquiries, and other matters, you can [email me](mailto:laresbernardo@gmail.com "email me") directly.
