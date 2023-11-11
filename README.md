
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OrderedHeatMapAnalysis

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

OrderedHeatMapAnalysis (OHMA) is a direct data analysis framework
allowing to simultaneously visualize and analyze the structure of
complex datasets. An optimized seriation of rows and columns of the
input data table is performed, resulting in a mapping of the whole
dataset into an ordered heatmap. Following analysis of the ordered
heatmap structure directly highlights submatrix of regularly ordered
data. Subsequently, an exhaustive identification of biculsters laying in
the subspaces of the dataset can be performed, and their mutual
relationships can easily be characterized. This method allows a
straitforwrard and deep exploration of all dimensions of the dataset.

## Installation

You can install the development version of OrderedHeatMapAnalysis from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("NoeDemange/OrderedHeatMapAnalysis")
```

## Example

This is a basic example which shows you how to run the app:

``` r
library("OrderedHeatMapAnalysis")
OrderedHeatMapAnalysis::run_app(options=list("launch.browser"=TRUE))
```

## Credits

This app was developed by [Noe Demange](https://github.com/NoeDemange).
Contact the maintainer of the app, [Guillaume
Sapriel](https://orcid.org/0000-0003-0549-9376). It is deployed on the
[MIGALE platform](https://migale.inrae.fr/) by [Cédric
Midoux](https://orcid.org/0000-0002-7964-0929). We are grateful to the
INRAE MIGALE bioinformatics facility (MIGALE, INRAE, 2020. Migale
bioinformatics Facility, doi: 10.15454/1.5572390655343293E12) for
providing help and storage resources.
