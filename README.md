# sdtmchecks

<!-- badges: start -->
[![R-CMD-check](https://github.com/pharmaverse/sdtmchecks/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/pharmaverse/sdtmchecks/actions/workflows/R-CMD-check.yml)
<!--Code coverage status -->[![codecov](https://codecov.io/github/pharmaverse/sdtmchecks/branch/devel/graph/badge.svg?token=ICYI400VDZ)](https://codecov.io/github/pharmaverse/sdtmchecks) 
<!--Test status -->[![CRAN status](https://www.r-pkg.org/badges/version/sdtmchecks)](https://cran.r-project.org/package=sdtmchecks)
<!-- badges: end -->

<img src="man/figures/logo_em.png" alt="drawing" align="right" width="200"/>

This package contains functions to identify common data issues in SDTM data.  These checks are intended to be **generalizable**, **actionable**, and **meaningful for analysis**.



### Installation

You can install sdtmchecks from GitHub with:

```{r}
devtools::install_github("pharmaverse/sdtmchecks")
```

### Use the package

The [Get started page](https://pharmaverse.github.io/sdtmchecks/articles/sdtmchecks.html) walks you through how the package can be used. 


### Data checks in the package

To search available data check functions, please see [this search page](https://pharmaverse.github.io/sdtmchecks/articles/search_checks.html).


### Contributing

This package has been developed internally at Roche since 2014. There may be areas where the checks expect Roche-specific SDTM implementation choices. Proposed additions or modifications should attempt to maintain generalizability for slightly different data standards across companies. For examples on how to do so and to learn more about contributing in general, please [click here](https://pharmaverse.github.io/sdtmchecks/articles/write_a_check.html).  

If you have an idea for a new check but no time for development, please [open an 
issue](https://github.com/pharmaverse/sdtmchecks/issues).


  
