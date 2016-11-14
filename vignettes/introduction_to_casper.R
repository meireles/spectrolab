## ---- eval=FALSE---------------------------------------------------------
#  library("devtools")
#  install_github("meireles/casper")

## ------------------------------------------------------------------------
library("casper")

## ------------------------------------------------------------------------
# Example spectral dataset in matrix format.
spec_matrix_example[1:4, 1:3]

# Note that this is NOT a spectra object. You can verify this by either asking what class `spec_example` is or using casper's `is_spectra()` function.

class(spec_matrix_example)
is_spectra(spec_matrix_example)

## ------------------------------------------------------------------------
# Make a spectra object if you have a matrix in the right format
spec = casper::as.spectra(spec_matrix_example)

# Did it work?
is_spectra(spec)

## ------------------------------------------------------------------------
# (1) Create a reflectance matrix.
#     In this case, by removing the species column
rf = spec_matrix_example[ , -1 ]

# Check the result
rf[1:4, 1:3]

# (2) Create a vector with wavelength labels that match
#     the reflectance matrix columns.
wl = colnames(rf)

# Check the result
wl[1:4]

# (3) Create a vector with sample labels that match
#     the reflectance matrix rows.
#     In this case, use the first colum of spec_matrix_example
sn = spec_matrix_example[ , 1] 

# Check the result
sn[1:4]

# Finally, construct the spectra object using the `spectra` constructor
spec = spectra(reflectance = rf, wavelengths = wl, names = sn)

# And hopefully this worked fine
is_spectra(spec)

## ---- error=TRUE---------------------------------------------------------
# Make a matrix from a `spectra` object
spec_as_mat = as.matrix(spec, fix_names = "none")
spec_as_mat[1:4, 1:3]

## ---- fig.height=2.5, fig.width=8----------------------------------------
par(mfrow = c(1, 3))

# Simple spectra plot
plot(spec, lwd = 0.75, lty = 1, col = "grey25", main = "All Spectra")

# Stand along quantile plot
plot_quantile(spec, total_prob = 0.8, main = "80% spectral quantile", 
              col = rgb(1, 0, 0, 0.5), lwd = 0.5, border = TRUE)

# Combined quantile and individual spctra plot
# With an added bonus of shading 4 spectral regions
plot(spec, lwd = 0.25, lty = 1, col = "grey50", "Spectra, quantile and regions")
plot_quantile(spec, total_prob = 0.8, 
              col = rgb(1, 0, 0, 0.25), add = TRUE, border = FALSE)
plot_spec_regions(spec, regions = default_spec_regions, add = TRUE)

## ------------------------------------------------------------------------
# Get the vector of all sample names
# Note that duplicate sample names are permitted
n = names(spec)
n[1:5]

# Or get the vector of wavelengths
w = wavelengths(spec)
w[1:5]

# You can also get the dimensions of your `spectra` object
dim(spec)

## ---- fig.height=2.5, fig.width=6, fig.align="center"--------------------
# Subset spectra to all entries where sample_name matches "species_8"
spec_sp8 = spec[ "species_8", ]

# Check the results
dim(spec_sp8)

# Plotting the seubset result should work just fine
par(mfrow = c(1, 2), cex.main = 0.8, cex.axis = 0.6, cex.lab = 0.8)

plot(spec_sp8, col = "red", main = "Species 8 spectra")
plot_quantile(spec, total_prob = 1.0, add = TRUE,  col = rgb(0.2, 0.2, 0.2, 0.2), border = FALSE)
plot_spec_regions(spec_sp8, default_spec_regions, col = rgb(1, 0.5, 0, 0.1), add = TRUE)

# And maybe further subset to the visible wavelengths only
spec_sp8 = spec_sp8[ , 400:700 ]

# This subset should still plot just fine
plot(spec_sp8, col = "red", main = "Visible spectra for species 8")
plot_quantile(spec, total_prob = 1.0, add = TRUE,  col = rgb(0.2, 0.2, 0.2, 0.2), border = FALSE)

## ---- error=TRUE---------------------------------------------------------
# Subset samples by index should work. It is also OK to subset by wavelength 
# using numerics or characters.
reflectance(spec_sp8[ 1 , "405"]) == reflectance(spec_sp8[ 1 , 405])

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# But remember that you CANNOT use indexes to subset wavelengths!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

# Something that is obvioulsy an index, like using 2 instead of 401 (the 2nd band), will fail.
spec_sp8[ , 2 ]

# However, `casper` canot detect if you meant to get the two last bands when
# you use 2000:2001. It will assume that you wanted wavelengths "2000" and "2001"
# Bottomline, be very careful not to use indexes to subset wavelengths!

## ------------------------------------------------------------------------
spec_new = spec

# Replace names with an uppercase version
names(spec_new) = toupper(names(spec_new))

# Check the results
names(spec_new)[1:5]

## ----  fig.height=3, fig.width=4, fig.align="center"---------------------
# Scale reflectance by 0.75
# spec_new[] = reflectance(spec_new) * 0.75
spec_new = spec_new * 0.75

# Plot the results
plot(spec, col = "blue", lwd = 0.75, cex.axis = 0.75)
plot(spec_new, col = "orange", lwd = 0.75, add = TRUE)

## ---- error=TRUE---------------------------------------------------------
# Trying to add 1.0 to all reflectance values will fail.
spec_new[] = reflectance(spec_new) + 1.0

