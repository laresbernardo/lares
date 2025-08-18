# lares 5.3.2 (2025-08-18)

-   fix: month format on dont_sleep_time()'s print
-   fix: better handle of no connection for get_currency()
-   docs: family "Games" for Scrabble, Wordle, Maze, Sudoku
-   docs: include NEWS in Chagelog new tab on pkgdown site + fixed favicon

# lares 5.3.1 (2025-07-04)

-   feat: new encrypted family of functions encrypt_file(), read_encrypted(), write_encrypted(), hex_to_raw(), raw_to_hex()
-   fix: get_creds() and cache returns objects correctly
-   fix: improved order of messages in dont_sleep() + allow single value for sec_range
-   fix: track correctly change of date while asleep in dont_sleep_time()

# lares 5.3.0 (2025-06-25)

-   fix: prepare compatibility with ggplot2 4.0.0 #59
-   feat: improve robyn_modelselector()'s weights based on experience, mainly confidence criteria
-   feat: new dont_sleep() and dont_sleep_time()
-   feat: no internet connection messages for online interaction functions
-   feat: ramp up META_API_VER from v20.0 to v21.0
-   recode: repo "yt-dlp" for get_mp3() by default
-   recode: remove all return() to improve codefactor score to A+
-   recode: cache's overwrite set to TRUE by default
-   docs: explicitly defined minimum versions of imported and suggested packages
-   docs: standardizes quiet param

# lares 5.2.13 (2025-02-19)

-   feat: include variable type in robyn_performance()
-   fix: mCPA calculation should be inverted (1 / provided marginal which is initResponseMargUnit)
-   fix: marginal results on robyn_performance() when OutputCollect has more than one model
-   fix: robyn_modelselector()'s scores issue when a cluster had 2 models, thus no sd
-   fix: cache filenames for stocks_hist + cache new overwrite parameter
-   fix: rename outlier_tukey() and allow for turkey too #58

# lares 5.2.11 (2025-01-09)

-   feat: new "certainty" and "cluster_sd" metrics for robyn_modelselector() with spend_wt lazy boolean param
-   feat: new scale argument for normalize() to customize minimum and maximum (instead of default 0, 1 range)
-   feat: new non_promo param for robyn_performance() to include non-promotional contributions
-   fix: adapt robyn_performance() to Robyn 3.12.0 update

# lares 5.2.10 (2024-12-13)

-   feat: new cross models budget allocation functions robyn_xchannels() and robyn_xchannels() #57
-   feat: formatNum() improvements to be more flexible with abbr = TRUE on very different scales of numbers
-   feat: new is_odd(), is_even() and seq_surnames() functions
-   fix: better error in connection handling for holidays()
-   fix: add some missing try_require("h2o")

# lares 5.2.9 (2024-09-13)

-   feat: new optional marginals/carryovers params to add metrics to robyn_performance()
-   feat: new repeated param on scrabble_words()
-   feat: new "chars" and "unique" scoring systems on scrabble_words()
-   fix: cache of OpenAI's reply stored in JSON format
-   fix: better handle of filenames on get_mp3()
-   fix: param exclude_here accepts upper case too for scrabble_words()
-   docs: update meta API version to v20.0
-   recode: fix some codefactor warnings on return()'s + font warning in Windows #55

# lares 5.2.8 (2024-06-11)

-   feat: allow corr_var() to pass string var input for loops - based on user feedback in StackOverflow
-   feat: new quiet parameter for font check on theme_lares()
-   feat: new baseline distance criteria for robyn_modelselector() and refactoring for better cleaner code
-   feat: new robyn_performance() function to calculate performance and contribution per channel
-   fix: keep organic vars to calculate non-zero betas in robyn_modelselector()
-   fix: correct solID filtering when multiple models are available in OutputCollect
-   docs: include example to recreate a Robyn model, added external posts, centralized some functions .md

# lares 5.2.7 (2024-04-22)

-   feat: enable font_dirs parameter and multiple fonts to check in font_exists()
-   feat: sort clusters in plot by importance in robyn_modelselector()
-   fix: when tied ranks on robyn_modelselector() set integer number of stars
-   fix: faster dir_size() using du -hs instead + default path getwd() in dir_size()
-   fix: facet in corr_cross() will consider variables in contains input
-   fix: update default META_API_VER to v17.0
-   docs: standard inheritParams from cache_write() for most ... params

# lares 5.2.5 (2024-01-22)

-   docs: removed h2o dependency and passed to Suggests to lighten the package
-   feat: new cache_pipe() to automate the use of cache
-   feat: new param path for what_size()
-   feat: new maze solving functions inspired by micromouse competitions: maze_solve() & maze_gridsearch()
-   feat: new gemini\_\*() functions to interact with Google's Gemini API
-   fix: inverted NRMSE, DECOMP.RSSD and MAPE + skip boost-performance option + non-clustered sols in robyn_modelselector()

# lares 5.2.4 (2023-12-07)

-   feat: new robyn_modelselector() to help users visually select Robyn models based on several metrics available and analysts weights criteria
-   feat: new robyn_hypsbuilder() to automatically build hyperparameters list for paid media and organic variables, ready to be used with Robyn
-   fix: normalize() for all equal values returns 1s instead of crashing
-   fix: cran_logs() mean on plot's first date per package

# lares 5.2.3 (2023-11-04)

-   feat: improve font_exists() suggestions and new use-case to search for all available fonts
-   feat: additional parameter for gpt\_\*() functions: num_retries, temperature, max_tokens, pause_base
-   feat: new chr2num(), chr2logical(), chr2date() functions
-   feat: new weighted_value() function, enabling FIFO/LIFO
-   feat: new delete/open params for get_mp3()
-   feat: New 'quiet' and 'include_regions' parameters to allow additional information on holidays
-   fix: better handling of scrapping responses using header and more flexible date formats for holidays() #45
-   fix: better gpt_table() prompt to deal with numeric values and delimiters
-   fix: live quote migrated to quantmod functions
-   fix: get_currency() correctly handles rownames now [quantmod update]
-   fix: better handling of NULL in formatNum()
-   docs: fix dependent vs independent term across parameters #51
-   docs: removed rdrop2 recommended dependency given it was deprecated from CRAN

# lares 5.2.2 (2023-05-18)

-   feat: new keep parameter for cleanText() and cleanNames()
-   feat: new gpt_history() and markdown2df() functions
-   fix: when no variables importance is returned by h2o #43
-   fix: NA or NULL for get_credentials accepted
-   fix: ohse() warning with dates and large left joins

# lares 5.2.1 (2023-03-23)

-   feat: new gpt\_\*() function to classify, tag, extract, format, convert, table, translate using ChatGPT
-   feat: enable repo parameter for get_mp3() so when the main repo is down
-   feat: pass ... to additional formatings using formatNum()
-   recode: default weekly_start = 1 for prophet's components plot

# lares 5.2.0 (2023-02-03)

-   feat: centralized all FB API results through fb_process() to standardize results, pagination and partial results if API crashes while paging
-   feat: upgrade FB API default version from v13.0 to v16.0 (latest)
-   feat: new filtering parameter for fb_insights()
-   feat: new fb_report_check() function to check async reports status with a live functionality
-   feat: fb_token() now returns expiration timestamp as an attribute
-   feat: new what_size() function to easily get an R object's size
-   fix: correct get_id numbering column (always starting in 1)
-   fix: show corr labels in corr_cross() by default (like in corr_var())
-   fix: cran_logs() fails gracefully when site is not available
-   fix: add --no-check-certificate to get_mp3
-   deprecated: fb_post() and fb_posts() given their deprecation since v2.4
-   recode: filled statusbar() fixed length as default
-   recode: conditional expressions with scalar logical operators (\|\|, &&) and avoid sapply [codefactor.io]

# lares 5.1.4 (2022-09-08)

-   fix: broken Wordle dictionary link from nytimes (crashed CRAN's checks)

# lares 5.1.3 (2022-08-26)

-   feat: enabled automatic clustering number based on WSS variance using wss_var parameter in clusterKmeans()
-   feat: new warnifnot() function, similar to stopifnot()
-   feat: new exclude_here param enables multiple options for same position in scrabble_words()(Wordle adaptation)
-   feat: new lares_logo() fun function prints lares library logo
-   docs: updated fb\_\*()'s api_version to v13.0
-   fix: skip drop when no transformations happened in ohse()

# lares 5.1.2 (2022-04-05)

-   docs: reduced dependencies by removing magrittr (Imports), DBI, forecats, plotly, rtweet, tm, and wordcloud (Suggests)
-   feat: new set_colnames() function, emulating magrittr's
-   fix: changed behavior on corr_var() when max_pvalue = 1

# lares 5.1.1 (2022-03-25)

-   feat: new force_exclude parameter (Wordle adaptation) and cache usage
-   feat: new wordle\_\*() functions to run simulations (and cheat)
-   feat: new print_coloured() auxiliary function
-   feat: new verbosity parameter on h2o_automl()
-   fix: repeated tickers input now managed correctly

# lares 5.0.5 (2022-01-31)

-   fix: corr_var() wrong outputs with cor(..., half = FALSE)
-   fix: no partial plots printed on prophesize()
-   feat: new year_quarter() function
-   feat: new append param on date_feats() and fixed issue on joining data
-   feat: enabled kmeans's algorithm parameter on clusterKmeans()
-   docs: explicitly pick exported tidyverse function to be exported, instead of all

# lares 5.0.3 (2021-11-16)

-   feat: new "filled" mode on statusbar()
-   feat: enable MOJO usage on explainers functions
-   feat: new half param on corr() for faster corr_cross() deduped results
-   feat: enable options to customize default colour palettes
-   fix: corr_cross() dim error. Re-wrote internal function .transf()
-   fix: issue with cor.test = "exact" default value on corr\*() functions #34 @michellekee
-   refactor: clean tidy code style using styler globally
-   docs: updated master branch to main

# lares 5.0.2 (2021-09-10)

-   docs: Excluded xml2 dependency temporally to avoid rvest (\>= 1.0.0). Used httr instead. Added rpart.plot as well.
-   docs: new h2o_automl() vignette
-   refactor: improved ohse() ignored columns logic
-   refactor: overall more NULL parameters as default, less NA
-   feat: new cran_logs() function to download and plot CRAN stats
-   feat: New reduce_pca() and reduce_tsne() functions reduction dimentionality
-   feat: enabled tree_var() for multi-categorical y's
-   feat: new force_single parameter for v2t()
-   deprecated: gg_pie()

# lares 5.0.1 (2021-06-09)

-   feat: new redundant = NULL option for 2+ categorical columns only
-   fix: specified R and rvest min versions + @ledell recommendations
-   docs: use \dontrun on problematic examples and reduces dependencies: mice, rlist, skimr, sp

# lares 5.0.0 (2021-06-03)

# lares 4.10.6 (2021-05-31)
