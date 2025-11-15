# Titanic Dataset

The sinking of the Titanic is one of the most infamous shipwrecks in
history. On April 15, 1912, during her maiden voyage, the widely
considered "unsinkable" RMS Titanic sank after colliding with an
iceberg. Unfortunately, there weren’t enough lifeboats for everyone
onboard, resulting in the death of 1502 out of 2224 passengers and crew.
While there was some element of luck involved in surviving, it seems
some groups of people were more likely to survive than others. This
dataset contains the details of a subset of the passengers on board (891
to be exact) taken from Kaggle's Titanic
[Train.csv](https://www.kaggle.com/c/titanic/overview).

## Usage

``` r
data(dft)
```

## Format

An object of class `"data.frame"`

- PassengerId:

  Unique ID for each passenger (1-891)

- Survived:

  Did the passenger survive? (TRUE, FALSE)

- Pclass:

  Ticket class, from first to third (1, 2, 3)

- Sex:

  Gender (female, male)

- Age:

  Age for each passenger in years (0.42-80)

- SibSp:

  Amount of siblings / spouses aboard the Titanic (0-8)

- Parch:

  Amount of parents / children aboard the Titanic (0-6)

- Ticket:

  Ticket IDs

- Fare:

  Amount paid for passenger's ticket (0-512.3292)

- Cabin:

  width of top of diamond relative to widest point (43-95)

- Embarked:

  Port of Embarkation (43-95)

## Value

data.frame

## See also

Other Dataset:
[`dfr`](https://laresbernardo.github.io/lares/reference/dfr.md)

## Examples

``` r
data(dft)
head(dft)
#>   PassengerId Survived Pclass    Sex Age SibSp Parch           Ticket    Fare
#> 1           1    FALSE      3   male  22     1     0        A/5 21171  7.2500
#> 2           2     TRUE      1 female  38     1     0         PC 17599 71.2833
#> 3           3     TRUE      3 female  26     0     0 STON/O2. 3101282  7.9250
#> 4           4     TRUE      1 female  35     1     0           113803 53.1000
#> 5           5    FALSE      3   male  35     0     0           373450  8.0500
#> 6           6    FALSE      3   male  NA     0     0           330877  8.4583
#>   Cabin Embarked
#> 1              S
#> 2   C85        C
#> 3              S
#> 4  C123        S
#> 5              S
#> 6              Q
```
