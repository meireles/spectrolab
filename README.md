[![Build Status](https://travis-ci.com/meireles/spectrolab.svg?branch=master)](https://travis-ci.com/meireles/spectrolab)
[![Coverage Status](https://coveralls.io/repos/github/meireles/spectrolab/badge.svg?branch=master)](https://coveralls.io/github/meireles/spectrolab?branch=master)
[![CRAN Status](https://www.r-pkg.org/badges/version/spectrolab)](https://cran.r-project.org/package=spectrolab)
[![DOI](https://zenodo.org/badge/73844175.svg)](https://zenodo.org/badge/latestdoi/73844175)

# spectrolab

Spectrolab is an R package that provides a class and methods for processing and visualizing high resolution spectra in R. It is licensed under GPL-3.

## Installation

You can install spectrolab from Github using:

```R
library("devtools")
install_github("meireles/spectrolab")
```

Or from CRAN using:

```R
install.packages("spectrolab")
```

## Using `spectrolab`

This vignette [introduces spectrolab](https://github.com/meireles/spectrolab/blob/master/vignettes/introduction_to_spectrolab.pdf), and walks you through the basics of the package.

This vignette shows how to [match (splice) sensors](https://github.com/meireles/spectrolab/blob/master/vignettes/match_sensors.pdf).


## Contributing

In an effort to keep things tidy and in running order, __direct commits to master are discouraged__. The master branch of the "meireles/spectrolab" fork is protected.

I also use a [__pre-commit hook__](https://stackoverflow.com/questions/40462111/git-prevent-commits-in-master-branch) in my local repo that prevents me from breaking my own policy! I suggest you do the same.
