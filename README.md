[![Build Status](https://travis-ci.org/meireles/spectrolab.svg?branch=master)](https://travis-ci.org/meireles/spectrolab)
[![Coverage Status](https://coveralls.io/repos/github/meireles/spectrolab/badge.svg?branch=master)](https://coveralls.io/github/meireles/spectrolab?branch=master)
[![CRAN Status](https://www.r-pkg.org/badges/version/spectrolab)](https://cran.r-project.org/package=spectrolab)


# spectrolab

The package is being actively developed and parts of the *API may still change*. You’re welcome to give it a spin, but do so *at your own risk*. With that said, most parts of the package are pretty stable. Let us know if you find otherwise.

## Installation

You can install spectrolab from CRAN using:

```R
install.packages("spectrolab")
```

Alternativelly, you can install it from github with:

```R
library("devtools")
devtools::install_github("annakat/spectrolab")
```

## Using `spectrolab`

This vignette [introduces spectrolab](vignettes/introduction_to_spectrolab.md), and works you through the basics of the package.

This vignette shows you how to [splice sensors](vignettes/match_sensors.pdf) using the function  `match_sensors` and details about what its arguments do. A more general processing vignette is coming soon.


## Contributing

In an effort to keep things tidy and in running order, __direct commits to master are discouraged__. In fact, the master branch is protected the "meireles/spectrolab" fork.

I also use a [__pre-commit hook__](https://stackoverflow.com/questions/40462111/git-prevent-commits-in-master-branch) in my local repo that prevents me from breaking my own policy! I suggest you do the same. 

We highly encourage you to read the vignette on [advanced spectrolab use](vignettes/advanced_spectrolab.md) if you're planning on contributing to or developing a package that depends on `spectrolab`.
