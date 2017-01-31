## ---- eval=FALSE---------------------------------------------------------
#  library("devtools")
#  install_github("annakat/spectrolab")

## ------------------------------------------------------------------------
library("spectrolab")

## ---- eval=FALSE---------------------------------------------------------
#  
#  # `dir_path` is the directory where our example datasets live
#  dir_path <- system.file("extdata", "Acer_example", package = "spectrolab")
#  
#  # Read .sig files
#  acer_spectra <- read_spectra(path = dir_path, format = "sig")
#  
#  # Note that `acer_spectra` is a `spectra` object.
#  # You can ensure that this is true using the `is_spectra()` function.
#  
#  is_spectra(acer_spectra)

## ---- eval=FALSE---------------------------------------------------------
#  # Simply print the object
#  acer_spectra
#  
#  # or look at the vector with the dataset's dimensions
#  dim(acer_spectra)
#  
#  # list the file names
#  names(acer_spectra)
#  
#  # and plot the spectra
#  plot(acer_spectra)

## ---- eval=FALSE---------------------------------------------------------
#  
#  # Check the filenames to see if there are any flags
#  names(acer_spectra)
#  
#  # use `exclude_if_matches` to excluded flagged files
#  acer_spectra <- read_spectra(path = dir_path, format = "sig",   exclude_if_matches = c("BAD","WR"))
#  
#  # and check result
#  plot(acer_spectra)

## ------------------------------------------------------------------------
# Read the example data
spec <- spec_matrix_example

## ---- eval=F-------------------------------------------------------------
#  # Check out the format of the matrix
#  spec[1:3, 1:4]

## ------------------------------------------------------------------------
# To convert it to spectra, simply run
spec_from_matrix <- as.spectra(spec, name_idx = 1)

## ---- eval=F-------------------------------------------------------------
#  # and again you can plot your data to make sure everything worked okay
#  plot(spec_from_matrix)
#  
#  spec_from_matrix2 <- spec_matrix_meta

## ---- eval=F-------------------------------------------------------------
#  # Make a matrix from a `spectra` object
#  spec_as_mat = as.matrix(spec_from_matrix, fix_names = "none")
#  spec_as_mat[1:4, 1:3]

## ---- fig.height=2.5, fig.width=8, dev_svg-------------------------------
# Simple spectra plot
par(mfrow = c(1, 3))
plot(spec_from_matrix, lwd = 0.75, lty = 1, col = "grey25", main = "All Spectra")

# Stand along quantile plot
plot_quantile(spec_from_matrix, total_prob = 0.8, col = rgb(1, 0, 0, 0.5), lwd = 0.5, border = TRUE)
title("80% spectral quantile")

# Combined individual spectra, quantiles and shade spectral regions
plot(spec_from_matrix, lwd = 0.25, lty = 1, col = "grey50", main="Spectra, quantile and regions")
plot_quantile(spec_from_matrix, total_prob = 0.8, col = rgb(1, 0, 0, 0.25), border = FALSE, add = TRUE)
plot_regions(spec_from_matrix, regions = default_spec_regions(), add = TRUE)

## ---- eval=F-------------------------------------------------------------
#  # Get the vector of all sample names. Note: Duplicated sample names are permitted
#  n <- names(spec_from_matrix)
#  n[1:5]
#  
#  # Get the vector of wavelengths
#  w <- wavelengths(spec_from_matrix)
#  w[1:5]
#  
#  # or the reflectances in matrix format
#  r <- reflectance(spec_from_matrix)
#  
#  # First 10 wavelengths of first 5 species
#  r[1:5,1:10]
#  
#  # You can also get the dimensions of your `spectra` object
#  dim(spec_from_matrix)

## ---- fig.height=2.5, fig.width=6, fig.align="center", eval=F------------
#  
#  # Subset wavelength regions
#  spec_sub <-  spec_from_matrix[ ,400:700]
#  
#  # Check the result
#  plot(spec_sub)
#  
#  # Subset spectra to all entries where sample_name matches "species_8"
#  spec_sp8 <- spec_from_matrix["species_8", ]
#  
#  # Check the results
#  dim(spec_sp8)
#  
#  # Plotting the subset result should work just fine
#  plot(spec_sp8, col = "red", main = "Species 8 spectra")
#  plot_quantile(spec_sp8, total_prob = 0.75, add = TRUE,  col = rgb(0.2, 0.2, 0.2, 0.2), border = TRUE)
#  plot_regions(spec_sp8, default_spec_regions(), col = rgb(1, 0.5, 0, 0.1), add = TRUE)

## ---- eval=F-------------------------------------------------------------
#  # Subsetting samples by indexes works and so does subsetting wavelengths by numerics or characters.
#  reflectance(spec_sp8[1,"405"]) == reflectance(spec_sp8[1,405])

## ---- eval=F-------------------------------------------------------------
#  # Something that is obvioulsy an index, like using 2 instead of 401 (the 2nd band
#  # in our case), will fail.
#  spec_sp8[ ,2]
#  
#  `Error in i_match_ij_spectra(this = this, i = i, j = j) : Wavelength subscript out of bounds. Use wavelength labels instead of raw indices.`

## ---- eval=F-------------------------------------------------------------
#  spec_new <- spec_from_matrix
#  
#  # Replace names with an uppercase version
#  names(spec_new) <- toupper(names(spec_new))
#  
#  # Check the results
#  names(spec_new)[1:5]

## ----  fig.height=3, fig.width=4, fig.align="center", eval=F-------------
#  # Scale reflectance by 0.75
#  spec_new <- spec_new * 0.75
#  
#  # Plot the results
#  plot(spec_from_matrix, col = "blue", lwd = 0.75, cex.axis = 0.75)
#  plot(spec_new, col = "orange", lwd = 0.75, add = TRUE)

## ---- eval=F-------------------------------------------------------------
#  # Read Acer example spectra
#  acer_spectra <- read_spectra(path = dir_path, format = "sig",   exclude_if_matches = c("BAD","WR"))
#  
#  # and check result
#  plot(acer_spectra)
#  
#  # Jump correction
#  acer_juco <- jump_corr(acer_spectra)
#  
#  # This looks better!
#  plot(acer_juco)

## ---- eval=F-------------------------------------------------------------
#  # Subset jump corrected spectra to 400 - 2400 nm
#  acer_sub <- acer_juco[, 400:2400]
#  
#  # and check result
#  plot(acer_sub)
#  
#  # Perform vector normalization
#  acer_vn <- normalize(acer_sub)
#  
#  # and check result
#  plot(acer_vn)

## ---- eval=FALSE---------------------------------------------------------
#  # Smooth only VIS/NIR or NIR/SWIR
#  acer_smoo1 <- smoo.visnir(acer_juco)
#  acer_smoo2 <- smoo.nirswir(acer_juco)
#  
#  # Smooth both regions
#  acer_smoo <- smoo.nirswir.svc(acer_smoo1)
#  
#  # Same result
#  acer_smoo <- smoo.visnir.svc(acer_smoo2)

## ---- eval=FALSE---------------------------------------------------------
#  # Subset jump corrected spectra to 400 - 2400 nm
#  acer_sub <- acer_juco[, 400:2400]
#  
#  # Exclude spectra with reflectances at 780 nm (the "NIR shoulder") <0.3 or >0.65
#  acer_sub1 <- excl_shoulder(acer_sub, refl_high = 0.65, refl_low = 0.3)
#  plot(acer_sub1)

## ---- eval=FALSE---------------------------------------------------------
#  # First plot your spectra to search for outliers
#  plot(acer_sub)
#  
#  # and add vertical and horizontal guides if you like.
#  abline(v=1500)
#  abline(h=0.2)
#  
#  # Instead of using the NIR shoulder, remove the outlier by defining a lower limit,
#  # e.g., 0.2 reflectance at 1500 nm, and check the result.
#  acer_sub2 <- excl_low(acer_sub, refl_low = 0.2, wvl_low = 1500)
#  plot(acer_sub2)
#  
#  # Or exclude the same measurement by defining a upper limit, e.g. 0.1 reflectance
#  # at 600 nm, and check the result.
#  acer_sub3 <- excl_high(acer_sub, refl_high = 0.1, wvl_high = 600)
#  plot(acer_sub3)

