
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mirmodels

<!-- badges: start -->
<!-- badges: end -->

## Installation

1.  In R, run

        install.packages("BiocManager", repo = "https://cran.rstudio.com/") 

2.  In R, run

        BiocManager::install(c("arrow", "BiocParallel", "broom", "car", 
                               "checkmate", "clipr", "DescTools", "DESeq2", 
                               "dials", "doFuture", "doParallel", "dplyr", 
                               "edgeR", "embed", "forcats", "foreach", "fs", 
                               "furrr", "future", "generics", "ggplot2", 
                               "ggpmisc", "ggrepel", "ggthemes", "glmnet", 
                               "glue", "janitor", "knitr", "magrittr", "MASS", 
                               "matrixStats", "mlbench", "mockery", "pacman", 
                               "parsnip", "pROC", "purrr", "RcppRoll", 
                               "readr", "recipes", "rlang", "rmarkdown", 
                               "rprojroot", "rrcov", "rsample", "scales", 
                               "spelling", "strex", "stringr", "styler", 
                               "testthat", "tibble", "tidyr", "tune", 
                               "usethis", "vip", "withr", "workflows", 
                               "xgboost", "yardstick", "zoo")) 

3.  The tar.gz files are at `built/mirmodels_x.x.x.tar.gz`. At the
    command line, enter

    ``` commandline
    R CMD INSTALL {path-to-latest-targz-file}
    ```

## Documentation

See the `pkgdown` site at <https://mirvie.gitlab.io/mirmodels/>.

## Building

To build the package, set your working directory to the root of the
package, i.e. the directory containing `DESCRIPTION`. Then at the
terminal run `Rscript build.R`. This will create the built package as a
`.tar.gz` in `built/`.

## Testing

In R, run `devtools::test()`.
