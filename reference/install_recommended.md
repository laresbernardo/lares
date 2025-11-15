# Install/Update Additional Recommended Libraries

All needed libraries to use (most) lares are already a dependency. There
are some functions that many people won't event know exist that will
require other additional libraries. Also, this may be used as a Docker
way of installing useful libraries on an new instance.

## Usage

``` r
install_recommended(progress = TRUE, all = FALSE)
```

## Arguments

- progress:

  Boolean. Show status bar?

- all:

  Boolean. All packages? If not, only the ones not installed yet.
