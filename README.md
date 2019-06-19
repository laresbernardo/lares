# lares <img src="https://i.imgur.com/X8vtJS6.png" width = 150 align = "right" />
### R Package for Analytics and Machine Learning
[![Build Status](https://travis-ci.com/laresbernardo/lares.svg?branch=master)](https://travis-ci.com/laresbernardo/lares) [![saythanks](https://img.shields.io/badge/say-thanks-1EAEDB.svg)](https://saythanks.io/to/laresbernardo)
----

`lares` is a library used to automate, improve, and speed everyday **Analysis and Machine Learning** tasks. It also simplifies the installation process of libraries, distribution, and reproducible research. You are most welcome to install, use, and comment on any of its code and functionalities. Feel free to contact me and/or add me on Linkedin, and please, do let me know where did you got my contact from!

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
- RMarkdown: [Frequencies, Distributions, and Correlations](http://rpubs.com/laresbernardo/freqs-distr-corr)

- DataScience+: [Visualizations for Classification Models Results](https://datascienceplus.com/machine-learning-results-one-plot-to-rule-them-all)

- DataScience+: [Visualizations for Regression Models Results](https://datascienceplus.com/machine-learning-results-in-r-one-plot-to-rule-them-all-part-2-regression-models)

- DataScience+: [AutoML and DALEX for Dataset Understanding](https://datascienceplus.com/understanding-titanic-dataset-with-h2os-automl-dalex-and-lares-library)

- DataScience+: [Portfolio's Performance and Report](https://datascienceplus.com/visualize-your-portfolios-performance-and-generate-a-nice-report-with-r)

- DataScience+: [Plot timelines with Gantt charts](https://datascienceplus.com/visualize-your-cvs-timeline-with-r-gantt-style/)

### AutoML Simplified Map from `h2o_automl()`
![AutoML Map (lares)](inst/docs/automl_map.png?raw=true)

## What else is there?

You can type `lares::` in **RStudio** and you will see a pop-up with all the functions that are currently available within the package. If in doubt, you can use the `?` function (i.e. `?lares::function`) and the **Help** tab will display a short explanation on each function and its parameters. If you want to check all the documentation, simply run `help(package = lares)`. You can find similar family functions in the **See Also** section of each documentation as well.

### Getting further help

If you encounter a clear bug, please share with us a reproducible example on [Github](https://github.com/laresbernardo/lares/issues) and I'll take care of it. For inquiries, and other matters, you can [email me](mailto:laresbernardo@gmail.com "email me") directly.
