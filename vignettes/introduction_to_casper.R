## ---- eval=FALSE---------------------------------------------------------
#  library("devtools")
#  install_github("meireles/casper")

## ------------------------------------------------------------------------
library("casper")

## ------------------------------------------------------------------------
# Example spectral dataset in matrix format.
spec_example[1:4, 1:3]

# Note that this is NOT a spectra object.
# You can verify this by asking what class `spec_example` is.
class(spec_example)

# An alternative is to use casper's `is_spectra()` function.
is_spectra(spec_example)

## ------------------------------------------------------------------------
# Make a spectra object if you have a matrix in the right format
spec = casper::as.spectra(spec_example)

# Did it work?
is_spectra(spec)

## ------------------------------------------------------------------------
# (1) Create a reflectance matrix.
#     In this case, by removing the species column
rf = spec_example[ , -1 ]

# Check the result
rf[1:4, 1:3]

# (2) Create a vector with wavelength labels that match
#     the reflectance matrix columns.
wl = colnames(rf)

# Check the result
wl[1:6]

# (3) Create a vector with sample labels that match
#     the reflectance matrix rows.
#     In this case, use the first colum of spec_example
sn = spec_example[ , 1] 

# Check the result
sn[1:6]

# Finally, construct the spectra object using the `spectra` constructor
spec = spectra(reflectance = rf, wavelengths = wl, sample_names = sn)

# And hopefully this worked fine
is_spectra(spec)

## ---- error=TRUE---------------------------------------------------------
# Make a matrix from a `spectra` object
spec_as_mat = as.matrix(spec, fix_dimnames = TRUE)
spec_as_mat[1:4, 1:3]

## ---- fig.height=2.5, fig.width=8----------------------------------------
par(mfrow = c(1, 3))

# Simple spectra plot
plot(spec, lwd = 0.75, lty = 1, col = "grey25")

# Stand along quantile plot
plot_quantile(spec, total_prob = 0.8, 
              col = rgb(1, 0, 0, 0.5), lwd = 0.5, border = TRUE)

# Combined quantile and individual spctra plot
plot(spec, lwd = 0.25, lty = 1, col = "grey50")
plot_quantile(spec, total_prob = 0.8, 
              col = rgb(1, 0, 0, 0.25), add = TRUE, border = FALSE)

## ------------------------------------------------------------------------
# Get the vector of all sample names
# Note that duplicate sample names are permitted
n = sample_names(spec)
n[1:5]

# Or get the vector of wavelengths
w = wavelengths(spec)
w[1:5]

# You can also get the dimensions of your `spectra` object
dim(spec)

## ---- error=TRUE---------------------------------------------------------
# Subset spectra to all entries where sample_name matches "species_8"
spec_sp8 = spec[ "species_8", ]

# And maybe further subset to the visible wavelengths only
spec_sp8 = spec_sp8[ , 400:700 ]

dim(spec_sp8)

## ---- error=TRUE---------------------------------------------------------
# Note that you can subset by wavelength using numerics or characters.
reflectance(spec_sp8[ 1 , "405"]) == reflectance(spec_sp8[ 1 , 405])

# But you CANNOT use indexes to subset wavelengths!
# Something that is obvioulsy an index will fail. For instance, using 2 instead of 401
spec_sp8[ , 2 ]

# However, if you use 2000:2001 you will NOT get the two last bands, but instead
# wavelengths "2000" and "2001".Bottomline, be careful not to use indexes!

## ------------------------------------------------------------------------
#x1 = spec[ "species_8", ]
#plot(spec[ "species_8", ], lwd = 1, lty = 1, col = "red4", add = TRUE)
#dim(x1)

