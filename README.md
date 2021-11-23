# Trust and Climate Change

R code and materials for trust and climate change project

## Getting Started

### Installing

To run this code, you will need to [install R](https://www.r-project.org/) and the following R packages:

```
install.packages(c("brms", "cowplot", "grid", "gridExtra", "haven", "mice", 
                   "ordinal", "readxl", "skimr", "targets", "tarchetypes",
                   "tidybayes", "tidyverse"))
```

### Executing code

1. Set the working directory to this code repository
2. Load the `targets` package with `library(targets)`
3. To run all analyses, run `tar_make()`
4. To load individual targets into your environment, run `tar_load(targetName)`

_Note: not all analyses will run as not all data is publicly available_

## Help

Any issues, please email scott.claessens@gmail.com.

## Authors

Scott Claessens, scott.claessens@gmail.com
