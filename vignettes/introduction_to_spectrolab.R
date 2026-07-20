## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse    = TRUE,
    comment     = "#>",
    fig.align   = "center",
    fig.retina  = 2,
    dpi         = 150
)


## ----eval=FALSE---------------------------------------------------------------
# install.packages("spectrolab")


## ----eval=FALSE---------------------------------------------------------------
# library("devtools")
# install_github("meireles/spectrolab")


## ----echo=TRUE, message=FALSE-------------------------------------------------
library("spectrolab")


## ----eval=TRUE----------------------------------------------------------------
# `dir_path` is the directory where our example datasets live
dir_path = system.file("extdata/Acer_example", package = "spectrolab")

# Read spectra from .sig files inside the "extdata/Acer_example" folder
acer_spectra = read_spectra(path = dir_path)


## ----eval=TRUE----------------------------------------------------------------
# Reading the target's radiance
acer_spectra_rad = read_spectra(path = dir_path, format = "sig", type = "target_radiance")

# And the white reference's radiance
acer_white_ref = read_spectra(path = dir_path, type = "reference_radiance")


## ----eval=TRUE----------------------------------------------------------------
# Use the `exclude_if_matches` argument to excluded flagged files
acer_spectra = read_spectra(path = dir_path, exclude_if_matches = c("BAD","WR"))


## ----eval=TRUE, message=FALSE-------------------------------------------------
# Use the `exclude_if_matches` argument to excluded flagged files
acer_spectra_with_meta = read_spectra(path = dir_path,
                                      exclude_if_matches = c("BAD","WR"),
                                      extract_metadata = TRUE)

# Here are fields 2 to 6 of the metadata for the first 3 scans.
# More on the `meta` function later.

meta(acer_spectra_with_meta)[1:3, 2:6]


## ----eval=TRUE----------------------------------------------------------------
dir_path = system.file("extdata/spec_matrix_meta.csv", package = "spectrolab")

# Read data from the CSV file. If you don't use `check.names` = FALSE when reading
# the csv, R will usually add a letter to the column names (e.g. 'X650') which will
# cause problems when converting the matrix to spectra.
spec_csv = read.csv(dir_path, check.names = FALSE)

# The sample names are in column 3. Columns 1 and 2 are metadata
achillea_spec = as_spectra(spec_csv, name_idx = 3, meta_idxs = c(1,2) )

# And now you have a spectra object with sample names and metadata...
achillea_spec


## ----eval=TRUE----------------------------------------------------------------
# Simply print the object
acer_spectra

# Get the dataset's dimensions
dim(acer_spectra)


## ----eval=TRUE----------------------------------------------------------------
# Vector of all sample names. Note: Duplicated sample names are permitted
n = names(achillea_spec)

# Vector of bands. Note: Duplicated band labels are permitted too (see "Gotchas" below)
w = bands(achillea_spec)

# value matrix
r = value(achillea_spec)

# Metadata. Use simplify = TRUE to get a vector instead of a data.frame
m = meta(achillea_spec, "ssp", simplify = TRUE)


## ----eval = TRUE--------------------------------------------------------------
# Subset band regions. Here we know that bands are integers (e.g. 400, 401, ...)
spec_sub_vis = achillea_spec[ , 400:700 ]

# Subset spectra to all entries where sample_name matches "ACHMI_7" or
# get the first three samples
spec_sub_byname = achillea_spec["ACHMI_7", ]
spec_sub_byidx  = achillea_spec[ 1:3, ]


## ----eval=TRUE----------------------------------------------------------------
acer_spectra_trim = acer_spectra[ , bands(acer_spectra, 400, 2400) ]


## ----eval=TRUE----------------------------------------------------------------
# Subsetting samples by indexes works and so does subsetting bands by numerics or characters.
spec_sub_byidx[1, "405"] == spec_sub_byidx[1, 405]


## ----eval=TRUE, error=TRUE----------------------------------------------------
try({
# Something that is obviously a column position, like 2 instead of 401
# (the actual label of the 2nd band in our data), will fail with an
# informative error rather than silently returning the wrong band.
spec_sub_byidx[ , 2]
})


## ----fig.height=2.5, fig.width=7, eval=TRUE-----------------------------------
# Simple spectra plot
oldpar = par(no.readonly = TRUE)

par(mfrow = c(1, 3))
plot(achillea_spec, lwd = 0.75, lty = 1, col = "grey25", main = "All Spectra")

# Stand along quantile plot
plot_quantile(achillea_spec, total_prob = 0.8, col = rgb(1, 0, 0, 0.5), lwd = 0.5, border = TRUE)
title("80% spectral quantile")

# Combined individual spectra, quantiles and shade spectral regions
plot(achillea_spec, lwd = 0.25, lty = 1, col = "grey50", main="Spectra, quantile and regions")
plot_quantile(achillea_spec, total_prob = 0.8, col = rgb(1, 0, 0, 0.25), border = FALSE, add = TRUE)
plot_regions(achillea_spec, regions = default_spec_regions(), add = TRUE)

par(oldpar)


## ----eval=TRUE----------------------------------------------------------------
spec_new = achillea_spec

# Replace names with a lowercase version
names(spec_new) = tolower(names(achillea_spec))

# Check the results
names(spec_new)[1:5]


## ----fig.height=3, fig.width=4, fig.align="center", eval=TRUE-----------------
# Scale value by 0.75
spec_new = spec_new * 0.75

# Plot the results
plot(achillea_spec, col = "blue", lwd = 0.75, cex.axis = 0.75)
plot(spec_new, col = "orange", lwd = 0.75, add = TRUE)


## ----eval = TRUE--------------------------------------------------------------
## Adding metadata to a spectra object: a dummy N content
n_content = rnorm(n = nrow(achillea_spec), mean = 2, sd = 0.5)
meta(achillea_spec, label = "N_percent") = n_content


## ----eval = TRUE, message = FALSE---------------------------------------------
# Average reflectance per species (metadata columns get FUN applied too, when
# possible -- non-numeric metadata, like species names, becomes NA rather
# than erroring, with a message)
spec_species_mean = aggregate(achillea_spec, by = names(achillea_spec), FUN = mean)
dim(spec_species_mean)


## ----eval=TRUE----------------------------------------------------------------
# Make a matrix from a `spectra` object
spec_as_mat = as.matrix(achillea_spec, fix_names = "none")
spec_as_mat[1:4, 1:3]

# Make a matrix from a `spectra` object
spec_as_df = as.data.frame(achillea_spec, fix_names = "none", metadata = TRUE)
spec_as_df[1:4, 1:5]


## ----eval = TRUE--------------------------------------------------------------
dup = achillea_spec
bands(dup)[2] = bands(dup)[1]   # force a duplicate band label

# Selecting the duplicated label returns both matching columns -- not an error
dup[1, bands(dup)[1]]


## ----eval = TRUE--------------------------------------------------------------
s1 = s2 = achillea_spec
meta(s1, "source") = "s1"
meta(s2, "source") = "s2"

r = suppressWarnings(s1 + s2)
ncol(meta(r))   # 0 -- cleared, because s1 and s2's metadata disagreed


## ----eval = TRUE--------------------------------------------------------------
length(achillea_spec)   # number of samples, same as nrow()
any(is.na(achillea_spec))   # elementwise check of the value matrix

sqrt(achillea_spec)   # Math group generic: abs, sqrt, log, round, exp, ...
-achillea_spec        # unary minus

achillea_spec %*% t(as.matrix(achillea_spec))   # matrix algebra -> a plain matrix


## ----eval = TRUE--------------------------------------------------------------
quantity(acer_spectra)
wavelength_unit(acer_spectra)

