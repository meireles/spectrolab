## ---- eval=FALSE---------------------------------------------------------
#  install.packages("spectrolab")

## ---- eval=FALSE---------------------------------------------------------
#  library("devtools")
#  install_github("meireles/spectrolab")

## ---- echo=FALSE---------------------------------------------------------
library("spectrolab")

## ---- eval=TRUE----------------------------------------------------------
dir_path = system.file("extdata/spec_matrix_meta.csv", package = "spectrolab")

# Read data from the CSV file. If you don't use `check.names` = FALSE when reading
# the csv, R will usually add a letter to the column names (e.g. 'X650') which will 
# cause problems when converting the matrix to spectra.
spec_csv = read.csv(dir_path, check.names = FALSE)

# The sample names are in column 3. Columns 1 and 2 are metadata
achillea_spec = as.spectra(spec_csv, name_idx = 3, meta_idxs = c(1,2) )

# And now you have a spectra object with sample names and metadata...
achillea_spec

## ---- eval=TRUE----------------------------------------------------------
# `dir_path` is the directory where our example datasets live
dir_path = system.file("extdata", "Acer_example", package = "spectrolab")

# Read .sig files
acer_spectra = read_spectra(path = dir_path, format = "sig")

## ---- eval=TRUE----------------------------------------------------------
# use the `exclude_if_matches` argument to excluded flagged files
acer_spectra = read_spectra(path = dir_path, format = "sig", exclude_if_matches = c("BAD","WR"))

## ---- eval=TRUE----------------------------------------------------------
# Simply print the object
acer_spectra

# Get the dataset's dimensions
dim(acer_spectra)

## ---- eval=TRUE----------------------------------------------------------
# Vector of all sample names. Note: Duplicated sample names are permitted
n = names(achillea_spec)

# Vector of wavelengths
w = wavelengths(achillea_spec)

# Reflectance matrix
r = reflectance(achillea_spec)

# Metadata. Use simplify = TRUE to get a vector instead of a data.frame
m = meta(achillea_spec, "ssp", simplify = TRUE)

## ---- eval = TRUE--------------------------------------------------------
# Subset wavelength regions.
spec_sub_vis = achillea_spec[ , 400:700 ]

# Subset spectra to all entries where sample_name matches "ACHMI_7" or
# get the first three samples
spec_sub_byname = achillea_spec["ACHMI_7", ]
spec_sub_byidx  = achillea_spec[ 1:3, ]

## ---- eval=TRUE----------------------------------------------------------
acer_spectra_trim = acer_spectra[ , wavelengths(acer_spectra, 400, 2400) ]

## ---- eval=TRUE----------------------------------------------------------
# Subsetting samples by indexes works and so does subsetting wavelengths by numerics or characters.
spec_sub_byidx[1, "405"] == spec_sub_byidx[1, 405]

## ---- eval=T, error=T----------------------------------------------------
# Something that is obvioulsy an index, like using 2 instead of 401 (the 2nd band 
# in our case), will fail.
spec_sub_byidx[ , 2]

`Error in i_match_ij_spectra(this = this, i = i, j = j) : Wavelength subscript out of bounds. Use wavelength labels instead of raw indices.`

## ---- fig.height=2.5, fig.width=7, eval=TRUE-----------------------------
# Simple spectra plot
par(mfrow = c(1, 3))
plot(achillea_spec, lwd = 0.75, lty = 1, col = "grey25", main = "All Spectra")

# Stand along quantile plot
plot_quantile(achillea_spec, total_prob = 0.8, col = rgb(1, 0, 0, 0.5), lwd = 0.5, border = TRUE)
title("80% spectral quantile")

# Combined individual spectra, quantiles and shade spectral regions
plot(achillea_spec, lwd = 0.25, lty = 1, col = "grey50", main="Spectra, quantile and regions")
plot_quantile(achillea_spec, total_prob = 0.8, col = rgb(1, 0, 0, 0.25), border = FALSE, add = TRUE)
plot_regions(achillea_spec, regions = default_spec_regions(), add = TRUE)

## ---- eval=T-------------------------------------------------------------
spec_new = achillea_spec

# Replace names with a lowercase version
names(spec_new) = tolower(names(achillea_spec))

# Check the results
names(spec_new)[1:5]

## ----  fig.height=3, fig.width=4, fig.align="center", eval=T-------------
# Scale reflectance by 0.75
spec_new = spec_new * 0.75

# Plot the results
plot(achillea_spec, col = "blue", lwd = 0.75, cex.axis = 0.75)
plot(spec_new, col = "orange", lwd = 0.75, add = TRUE)

## ---- eval = TRUE--------------------------------------------------------
## Adding metadata to a spectra object: a dummy N content
n_content = rnorm(n = nrow(achillea_spec), mean = 2, sd = 0.5)
meta(achillea_spec, label = "N_percent") = n_content

## ---- eval=T-------------------------------------------------------------
# Make a matrix from a `spectra` object
spec_as_mat = as.matrix(achillea_spec, fix_names = "none")
spec_as_mat[1:4, 1:3]

# Make a matrix from a `spectra` object
spec_as_df = as.data.frame(achillea_spec, fix_names = "none", metadata = TRUE)
spec_as_df[1:4, 1:5]

