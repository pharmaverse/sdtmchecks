
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sdtmchecks

<!-- package hex logo -->

<img src="man/figures/logo_em.png" alt="drawing" align="right" width="100"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/pharmaverse/sdtmchecks/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/pharmaverse/sdtmchecks/actions/workflows/R-CMD-check.yml)
[![codecov](https://codecov.io/github/pharmaverse/sdtmchecks/branch/devel/graph/badge.svg)](https://app.codecov.io/github/pharmaverse/sdtmchecks)
[![CRAN
status](https://www.r-pkg.org/badges/version/sdtmchecks)](https://cran.r-project.org/package=sdtmchecks)
<!-- badges: end -->

The sdtmchecks package contains functions to identify common data issues
in SDTM data. These checks are intended to be **generalizable**,
**actionable**, and **meaningful for analysis**.

## Installation

### CRAN

You can install {sdtmchecks} from [CRAN](https://cran.r-project.org/):

``` r
install.packages("sdtmchecks")
```

### Development version (devel branch)

The development version (based on the default *devel branch*) can be
installed via:

``` r
install.packages("sdtmchecks", repos = "https://pharmaverse.r-universe.dev")  
```

Or, via [GitHub](https://github.com/) repo
`pharmaverse/sdtmchecks@HEAD`:

``` r
# install.packages("devtools")
devtools::install_github("pharmaverse/sdtmchecks")

# Note that `ref = "devel"` is not needed since devel is the default branch
```

## Use the package

The [Get started
page](https://pharmaverse.github.io/sdtmchecks/articles/sdtmchecks.html)
walks you through how the package can be used.

## Data checks in the package

To search available data check functions, please see [this search
page](https://pharmaverse.github.io/sdtmchecks/articles/search_checks.html).

The main data check functions of the package are saved as R program
files beginning with **“check\_…”** and are referenced under the [Data
Checks](https://pharmaverse.github.io/sdtmchecks/reference/index.html#data-checks)
portion of the Reference page.

## SDTM version

This package aims to be SDTM version agnostic and does not require users
to specify the version.

## Contributing

This package has been developed internally at
[Roche](https://www.roche.com/) since 2014. There may be areas where the
checks expect Roche-specific SDTM implementation choices. Proposed
additions or modifications should attempt to maintain generalizability
for slightly different data standards across companies. For examples on
how to do so and to learn more about contributing in general, please
refer to the [Writing a New
Check](https://pharmaverse.github.io/sdtmchecks/articles/write_a_check.html)
page.

If you have an idea for a new check but no time for development, please
[open an issue](https://github.com/pharmaverse/sdtmchecks/issues).

## FAQs

The package
[FAQs](https://pharmaverse.github.io/sdtmchecks/articles/faqs.html) may
provide additional information you are looking for. If not, please feel
free to post your question as [an
issue](https://github.com/pharmaverse/sdtmchecks/issues).

## Acknowledgments

This package has been created from the contributions, ideas, and support
of numerous individuals:

Josue Alcaraz, Anastasiia Andriushchenko, Yuliia Bahatska, Stella Banjo,
Tim Barnett, Sara Bodach, Ben Byrne, Jen Chen, Jingyuan Chen, Shumei
Chi, Tina Cho, Saibah Chohan, Ross Farrugia, Kim Fernandes, Pasha
Foroudi, Will Harris, Martin Hewing, Antony Howard, Anastasiia
Khmelnytska, Dinakar Kulkarni, Joel Laxamana, Jennifer Li, Rosemary Li,
Jennifer Lomax, Simon Luo, Madeleine Ma, Edoardo Mancini, Chandra
Mannem, Edgar Manukyan, Ashley Mao, Ryan Marinelli, Kieran Martin,
Yinghui Miao, Bonita Viegas Monteiro, Vani Nimbal, Katie Patel, Nina
Ting Qi, Beeya Na, Sam Parmar, Hiral Raval, Mij Rehman, Reza Rezai, Jim
Rothstein, Aldrich Salva, Monarch Shah, Sarwan Singh, Natalie
Springfield, Lasya Sreeramagiri, Tom Stone, Vira Vrakina, Adrian
Waddell, Betty Wang, Laura Wang, Rena Wang, Fang Yuan, Ying Yuen, James
Zhang, Iris Zhao, Lei Zhao.
