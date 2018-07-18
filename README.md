# library(lares)

## Fresh library install
devtools::install_bitbucket("laresbernardo/lares")

## Update library
lares::updateLares() # From repository. Set user and file (config.yml) if necessary
lares::updateLares(local=T) # Personal and local files, not necessarily deployed
