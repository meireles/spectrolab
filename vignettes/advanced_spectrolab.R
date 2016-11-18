## ------------------------------------------------------------------------
library("spectrolab")

## ------------------------------------------------------------------------
# (1) Create a reflectance matrix.
#     In this case, by removing the first column that holds the species name
rf = spec_matrix_example[, -1]

# (2) Create a vector with wavelength labels that match
#     the reflectance matrix columns.
wl = colnames(rf)

# (3) Create a vector with sample labels that match
#     the reflectance matrix rows.
#     In this case, use the first colum of spec_matrix_example
sn = spec_matrix_example[, 1] 

# Finally, construct the spectra object using the `spectra` constructor
spec = spectra(reflectance = rf, wavelengths = wl, names = sn)

# And hopefully this worked fine
is_spectra(spec)
plot(spec)

## ---- eval=F-------------------------------------------------------------
#  
#  # Getters
#  names(spec)[1:4]
#  wavelengths(spec)[1:4]
#  
#  # Setters
#  names(spec)       = toupper(names(spec))

## ------------------------------------------------------------------------
spec[1, 400:1200] = spec[1, 400:1200] * 2
plot(spec)

## ---- error=TRUE---------------------------------------------------------
# Trying to add 1.0 to all reflectance values will fail.
spec[] = reflectance(spec) + 1.0

