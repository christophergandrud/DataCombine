DataCombine
======

### Christopher Gandrud

### Version 0.01

### Note: **DataCombine** is in beta. Please report any bugs at: <https://github.com/christophergandrud/DataCombine/issues>.

---

**DataCombine** is intended for making combining data sets--especially time-series cross-section data--easier. The package is in the very very early stages of development. It includes two functions:

- `FillIn`: a function for filling in missing values of a variable from one data frame with the values from another variable.

- `MoveFront`: a simple function for moving a variable to the front of a data frame. This can be useful if you have a data frame with many variables and just want to move one variable to the front.

I will continue to add to the package as I build data sets and run across other pesky tasks I do repeatedly that would be simpler if they were turned into a function.

---

## Installation

**DataCombine** is in beta and so is not currently on CRAN. 

To install the package use `install_github` from the [devtools](https://github.com/hadley/devtools):

```
devtools::install_github("DataCombine", "christophergandrud")
```