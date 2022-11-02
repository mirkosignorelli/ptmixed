# Poisson-Tweedie mixed-effects model: flexible modelling of longitudinal overdispersed counts

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ptmixed)](https://cran.r-project.org/package/ptmixed)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/ptmixed?color=orange)](http://cranlogs.r-pkg.org/badges/grand-total/ptmixed)
[![Monthly Downloads](http://cranlogs.r-pkg.org/badges/ptmixed)](http://cranlogs.r-pkg.org/badges/ptmixed)

<img src="https://user-images.githubusercontent.com/20061736/84284787-f3821f00-ab3c-11ea-85a7-220982ce518c.png" align="right" alt="" width="250" />

## What is ptmixed
`ptmixed` is an `R` package that allows to estimate the Poisson-Tweedie generalized linear mixed model presented in Signorelli, Spitali and Tsonaka (2021). Poisson-Tweedie mixed-effects model: a flexible approach for the analysis of longitudinal RNA-seq data. *Statistical Modelling*, 21 (6), 520-545. DOI: 10.1177/1471082X20936017. You can [read and download the paper here](https://journals.sagepub.com/doi/10.1177/1471082X20936017).

## About this repository
This repository contains data and code to reproduce the simulations presented in Signorelli, Spitali and Tsonaka (2021).

## How to use `ptmixed`
`ptmixed` [can be downloaded from CRAN](https://cran.r-project.org/web/packages/ptmixed/index.html). Installation of the package in `R` can be done through the command 
```
install.packages('ptmixed')
```

If you encounter problems with packages on which `ptmixed` depends, you may alternatively install the package using `BiocManager`:

```
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install('ptmixed')
```


## Further information
More information on `ptmixed` can be found at the following pages:
* [CRAN package page](https://cran.r-project.org/web/packages/ptmixed/index.html);
* [my personal website](https://mirkosignorelli.github.io/r.html);
* [vignette that illustrates how to use the `R` package's functions](https://cran.r-project.org/web/packages/ptmixed/vignettes/Overview_functionalities_ptmixed.html).

A read-only mirror of the package's source code is [available here](https://github.com/cran/ptmixed).
