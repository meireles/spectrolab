[![Build Status](https://travis-ci.org/meireles/spectrolab.svg?branch=master)](https://travis-ci.org/meireles/spectrolab)
[![Coverage Status](https://coveralls.io/repos/github/meireles/spectrolab/badge.svg?branch=master)](https://coveralls.io/github/meireles/spectrolab?branch=master)
[![CRAN Status](https://www.r-pkg.org/badges/version/spectrolab)](https://cran.r-project.org/package=spectrolab)

# spectrolab

``spectrolab`` 0.09 **Breaks Backwards Compatibility!**

The function `wavelengths` is now `bands` and the function `reflectance` is now `value`. We suggest that you update your code to reflect that change. If you really need to use the older api, install ``spectrolab`` from the `legacy` branch.


## Installation

You can install spectrolab from Github using:

```R
library("devtools")
install_github("meireles/spectrolab")
```

## Using `spectrolab`

This vignette [introduces spectrolab](spectrolab/vignettes/introduction_to_spectrolab.pdf), and walks you through the basics of the package.

## Contributing

In an effort to keep things tidy and in running order, __direct commits to master are discouraged__. The master branch of the "meireles/spectrolab" fork is protected.

I also use a [__pre-commit hook__](https://stackoverflow.com/questions/40462111/git-prevent-commits-in-master-branch) in my local repo that prevents me from breaking my own policy! I suggest you do the same. 
