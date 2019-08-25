# MASH (Modular Analysis & Simulation for Human Health)

## Hosted Software

### macro.pfsi

This directory contains the MACRO simulator with PfSI course of infection (CoI) module and Ross-Macdonald (RM) mosquito model. It allows user selection of Poisson or Negative Binomial biting with individual overdispersion for human agents, discrete trip movement model for humans, and diffusion-based movement for mosquitoes. It uses R's built in random number generator exposed through R's C API because of its known quality.

To install this package, please enter the following command in an R terminal: `devtools::install_github(repo = "https://github.com/dd-harp/MASH",subdir = "macro.pfsi")`

This software (R package) has the following pre-requisites:
  * C++11/14 compatible compiler
  * `Rcpp` R package
  * `RcppArmadillo` R package
  * `RcppProgress` R package

### macro.pfmoi

This directory contains the MACRO simulator with PfMOI course of infection (CoI) module and Ross-Macdonald (RM) mosquito model. It allows user selection of Poisson or Negative Binomial biting with individual overdispersion for human agents, discrete trip movement model for humans, and diffusion-based movement for mosquitoes. It uses R's built in random number generator exposed through R's C API because of its known quality.

To install this package, please enter the following command in an R terminal: `devtools::install_github(repo = "https://github.com/dd-harp/MASH",subdir = "macro.pfmoi")`

This software (R package) has the following pre-requisites:
  * C++11/14 compatible compiler
  * `Rcpp` R package
  * `RcppArmadillo` R package
  * `RcppProgress` R package

## Miscellaneous

### Technical Bits
If you encounter problems with the `Rcpp` ecosystem for Mac OS, please see this guide (https://thecoatlessprofessor.com/programming/r-compiler-tools-for-rcpp-on-macos/) to fix most common problems. Also make sure R is running the latest version.
