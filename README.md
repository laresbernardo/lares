# library(lares)

This is my personal library used to automate and speed my everyday work on **Analysis and Machine Learning** tasks. You are most welcome to install, use, and comment on any of its code and functionalities. Feel free to contact me and/or add me on [Linkedin](https://www.linkedin.com/in/laresbernardo/)!

## Getting Started

### Fresh library install
```devtools::install_github("laresbernardo/lares")```

### Update library
```
lares::updateLares() # From Github repository (Bitbucket discontinued)
lares::updateLares(local=T) # Personal and local files, not necessarily deployed
```
Everytime you update the library, I recommend restarting your session: close RStudio and reopen it. You can also set the ```restart = TRUE``` parameter on the ```updateLares()``` function.

## Some posts published using the library
- DataScience+: [Visualizations for Classification Models Results](https://datascienceplus.com/machine-learning-results-one-plot-to-rule-them-all)

- DataScience+: [Visualizations for Regression Models Results](https://datascienceplus.com/machine-learning-results-in-r-one-plot-to-rule-them-all-part-2-regression-models)

- DataScience+: [AutoML and DALEX for Dataset Understanding](https://datascienceplus.com/understanding-titanic-dataset-with-h2os-automl-dalex-and-lares-library/)

- DataScience+: [Portfolio's Performance and Report]()
