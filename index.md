# lares

### Lean Analytics and Robust Exploration Sidekick

[![R-CMD-check](https://github.com/laresbernardo/lares/workflows/R-CMD-check/badge.svg)](https://github.com/laresbernardo/lares/actions?query=workflow%3AR-CMD-check)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-ago/lares)](https://cran.r-project.org/package=lares)
[![](https://cranlogs.r-pkg.org/badges/lares)](https://cran.r-project.org/package=lares)
[![docs](https://github.com/laresbernardo/lares/workflows/documentation/badge.svg)](https://laresbernardo.github.io/lares/reference/index.html)
[![CodeFactor](https://www.codefactor.io/repository/github/laresbernardo/lares/badge)](https://www.codefactor.io/repository/github/laresbernardo/lares/overview/main)

------------------------------------------------------------------------

**lares** is an R package designed to automate, improve, and accelerate
everyday **analytics and machine learning** tasks. It offers a wide
variety of functions grouped in families for:

- **Machine Learning**: Streamlined model training and evaluation,
  including friendly AutoML pipelines.
- **Data Cleaning & Processing**: Functions to quickly prepare your data
  for modeling or analyzes.
- **Exploratory Data Analysis (EDA)**: Instantly visualize and summarize
  your data.
- **Reporting**: Easily generate comprehensive reports to share insights
  for MMM and ML models.
- **Visualization**: Out-of-the-box plotting for classification and
  regression models, timelines, and more.
- **API Integrations & Scrapers**: Simplify data collection from various
  sources.
- **Time Series & Portfolio Analysis**: Specialized utilities for
  financial and temporal data.
- **Credentials & Secrets Management**: Securely handle sensitive
  information in your analytics pipelines.
- **NLP & Text Analytics**: Tools to analyze and process text data.

> **Tip:** See all available functions and documentation
> [here](https://laresbernardo.github.io/lares/reference/index.html) or
> type `?lares::` in RStudio to explore interactively.

------------------------------------------------------------------------

## Installation

``` r
# CRAN VERSION
install.packages("lares")

# DEV VERSION (latest updates)
# If you don't have remotes installed yet, run: install.packages('remotes')
remotes::install_github("laresbernardo/lares")
# For a full installation with recommended dependencies:
remotes::install_github("laresbernardo/lares", dependencies = TRUE)
```

**Windows users:** You may need to install
[RTools](https://cran.r-project.org/bin/windows/Rtools/) to build the
dev version.

------------------------------------------------------------------------

## Read about `lares` in action!

- **AutoML Quickstart:** [Introduction to AutoML using
  `lares`](https://laresbernardo.github.io/lares/articles/h2o_automl.html)
- **Model Results Visualization:**
  [Classification](https://datascienceplus.com/machine-learning-results-one-plot-to-rule-them-all/)
  \|
  [Regression](https://datascienceplus.com/machine-learning-results-in-r-one-plot-to-rule-them-all-part-2-regression-models/)
- **Marketing Mix Model Selection:** [Select the right MMM
  candidate](https://medium.com/@laresbernardo/select-the-right-mmm-candidate-based-on-your-specific-criteria-and-business-knowledge-1f583c3cb97a)
- **Cross-Correlations:** [Find Insights with Ranked
  Cross-Correlations](https://datascienceplus.com/find-insights-with-ranked-cross-correlations/)
- **Secure Credentials:** [Manage Secrets in
  R](https://datascienceplus.com/how-to-manage-credentials-and-secrets-safely-in-r/)
- **Portfolio Analysis:** [Performance &
  Reporting](https://datascienceplus.com/visualize-your-portfolios-performance-and-generate-a-nice-report-with-r/)
- **Fun games**:
  [Wordle](https://datascienceplus.com/how-i-selected-my-starting-word-for-wordle-using-simulations-and-r/),
  [Scrabble](https://laresbernardo.github.io/lares/reference/scrabble.html),
  [Sudoku](https://laresbernardo.github.io/lares/reference/sudoku_solver.html),
  [Mazes](https://laresbernardo.github.io/lares/reference/maze_solve.html),
  etc.
- **More Examples:** [Read other
  posts](https://datascienceplus.com/author/bernardo-lares/)

------------------------------------------------------------------------

## Popular Functions

- [`h2o_automl()`](https://laresbernardo.github.io/lares/reference/h2o_automl.md),
  `plot_model_results()` – Automated machine learning pipeline with
  optimal model selection and visualizations.
- [`freqs()`](https://laresbernardo.github.io/lares/reference/freqs.md),
  [`distr()`](https://laresbernardo.github.io/lares/reference/distr.md),
  [`corr_var()`](https://laresbernardo.github.io/lares/reference/corr_var.md),
  [`corr_cross()`](https://laresbernardo.github.io/lares/reference/corr_cross.md)
  – Instantly summarize, visualize, and uncover relationships in your
  data.
- [`ohse()`](https://laresbernardo.github.io/lares/reference/ohse.md) –
  Efficient and smart one-hot encoding for categorical variables.
- `cache_*` – Speed up workflows by caching expensive computations.
- `robyn_*` – Additional functions to support
  [Robyn](https://github.com/facebookexperimental/Robyn) inputs and
  outputs.
- `fb_*` – Interact with Meta’s Marketing API
- `gpt_*` – Structured prompts builder and interact with OpenAI’s API
- [`read_encrypted()`](https://laresbernardo.github.io/lares/reference/encrypt_file.md),
  [`write_encrypted()`](https://laresbernardo.github.io/lares/reference/encrypt_file.md)
  – Interact with encrypted files to keep secrets safe
- …and [many
  more!](https://laresbernardo.github.io/lares/reference/index.html)

![AutoML Map (lares)](reference/figures/automl_map.png?raw=true)

AutoML Map (lares)

------------------------------------------------------------------------

## Getting Started & Help

- Browse all functions in the [online
  reference](https://laresbernardo.github.io/lares/reference/index.html).
- Use `?lares::function_name` in RStudio for detailed help on any
  function.
- Found a bug or have a feature request? [Open an
  issue](https://github.com/laresbernardo/lares/issues).
- For questions or suggestions, reach out to
  [laresbernardo](mailto:laresbernardo@gmail.com).
