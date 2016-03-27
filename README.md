DataCombine
======

Christopher Gandrud

Version 0.2.20
[![Build Status](https://travis-ci.org/christophergandrud/DataCombine.png)](https://travis-ci.org/christophergandrud/DataCombine) [![CRAN Version](http://www.r-pkg.org/badges/version/DataCombine)](http://cran.r-project.org/package=DataCombine) ![CRAN Downloads](http://cranlogs.r-pkg.org/badges/last-month/DataCombine) ![CRAN Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/DataCombine)


Please report any **bugs** or **suggestions** at:
<https://github.com/christophergandrud/DataCombine/issues>.

## Motivation and Functions

**DataCombine** is a set of miscellaneous tools intended to make combining
data sets--especially time-series cross-section data--easier. The package is
continually being developed as I turn lines of code that I frequently use into
single functions. It currently includes the following functions:

- `change`: calculates the absolute, percentage, and proportion change from 
a specified lag, including within groups.

- `CountSpell`: function that returns a variable counting the spell number
for an observation. Works with grouped data.

- `dMerge`: merges 2 data frames and report/drop/keeps only duplicates.

- `DropNA`: drops rows from a data frame when they have missing (`NA`) values on a
given variable(s).

- `FillDown`: fills in missing (`NA`) values with the previous non-missing value

- `FillIn`: fills in missing values of a variable from one data frame with the
values from another variable.

- `FindDups`: find duplicated values in a data frame and subset it to either
include or not include them.

- `FindReplace`: replaces multiple patterns found in a character string column
of a data frame.

- `grepl.sub`: subsets a data frame if a specified pattern is found in a
character string.

- `InsertRow`: allows user to insert a row into a data frame. Largely
implements: [Ari B. Friedman's function](http://stackoverflow.com/a/11562428).

- `MoveFront`: moves variables to the front of a data frame. This can be useful
if you have a data frame with many variables and want to move a variable or
variables to the front.

- `NaVar`: create new variable(s) indicating if there are missing values in
other variable(s).

- `shift`: creates lag and lead variables, including for time-series
cross-sectional data. The shifted variable is returned to a new vector. This
function is largely based on
[TszKin Julian's shift function](http://ctszkin.com/2012/03/11/generating-a-laglead-variables/).

- `slide`: creates lag and lead variables, including for time-series
cross-sectional data. The slid variable are added to the original data frame.
This expands the capabilities of `shift`.

- `slideMA`: creates a moving average for a period before or after each time
point for a given variable.

- `SpreadDummy`: spread a dummy variable (1's and 0') over a specified time
period and for specified groups.

- `StartEnd`: finds the starting and ending time points of a spell, including
for time-series cross-sectional data.

- `rmExcept`: removes all objects from a workspace except those specified by the
user.

- `TimeExpand`: expands a data set so that it includes an observation for each
time point in a sequence. Works with grouped data.

- `TimeFill`: creates a continuous `Unit`-`Time`-`Dummy` data frame from a data
frame with `Unit`-`Start`-`End` times.

- `VarDrop`: drops one or more variables from a data frame.

## Updates

I will continue to add to the package as I build data sets and run across other
pesky tasks I do repeatedly that would be simpler if they were a function.

## Installation

**DataCombine** is on [CRAN](http://cran.r-project.org/).

You can also install the most recent stable version with `install_github` from
the [devtools](https://github.com/hadley/devtools):

```{S}
devtools::install_github('christophergandrud/DataCombine')
```
