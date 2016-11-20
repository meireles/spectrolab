## ---- eval=FALSE---------------------------------------------------------
#  library("devtools")
#  install_github("annakat/spectrolab")

## ---- eval=FALSE---------------------------------------------------------
#  library("spectrolab")

## ---- eval=FALSE---------------------------------------------------------
#  
#  # dir_path is the directory where our example datasets live
#  dir_path <- system.file("extdata", "Acer_example", package = "spectrolab")
#  
#  # Read .sig files
#  acer_spectra <- read_spectra(path = dir_path, format ="sig")
#  
#  # Note that `acer_spectra` is a `spectra` object. You can ensure that is true
#  # using spectrolab's `is_spectra()` function.
#  
#  is_spectra(acer_spectra)

## ---- eval=FALSE---------------------------------------------------------
#  # Simply print the object
#  acer_spectra
#  
#  # Get a vector with the dataset dimension
#  dim(acer_spectra)
#  
#  # and plot the spectra
#  plot(acer_spectra)

## ---- eval=FALSE---------------------------------------------------------
#  
#  # Use `exclude_if_matches` to excluded flagged files
#  acer_spectra <- read_spectra(path = dir_path, format ="sig",   exclude_if_matches = c("BAD","WR"))
#  
#  # and check result
#  plot(acer_spectra)

## ---- eval=FALSE---------------------------------------------------------
#  spec <- spec_matrix_example
#  
#  # Check out the format of the matrix
#  spec[1:3, 1:4]
#  
#  # To convert it to spectra, simply run
#  spec_from_matrix <- as.spectra(spec)
#  
#  # and again you can plot it to make sure everything worked okay
#  plot(spec_from_matrix)

## ---- fig.height=2.5, fig.width=8, error=TRUE----------------------------
# Simple spectra plot
par(mfrow = c(1, 3))
plot(spec_from_matrix, lwd = 0.75, lty = 1, col = "grey25", main = "All Spectra")

# Stand along quantile plot
plot_quantile(spec_from_matrix, total_prob = 0.8, main = "80% spectral quantile", col = rgb(1, 0, 0, 0.5), lwd = 0.5, border = TRUE)

# Combined quantile and individual spctra plot
# With an added bonus of shading 4 spectral regions
plot(spec_from_matrix, lwd = 0.25, lty = 1, col = "grey50", "Spectra, quantile and regions")
plot_quantile(spec_from_matrix, total_prob = 0.8, col = rgb(1, 0, 0, 0.25), border = FALSE)
plot_spec_regions(spec_from_matrix, regions = default_spec_regions(), add = TRUE)

## ---- eval=F-------------------------------------------------------------
#  # Get the vector of all sample names
#  # Note that duplicate sample names are permitted
#  n = names(spec)
#  n[1:5]
#  
#  # Or get the vector of wavelengths
#  w = wavelengths(spec)
#  w[1:5]
#  
#  # You can also get the dimensions of your `spectra` object
#  dim(spec)

## ---- fig.height=2.5, fig.width=6, fig.align="center", eval=F------------
#  
#  ### Subset wavelength regions
#  acer_clip <-  acer_juco[ , 400:2400]
#  
#  ### Check the result
#  plot(spec_from_matrix)
#  
#  # Subset spectra to all entries where sample_name matches "species_8" and wavelength regions
#  spec_sp8 = spec[ "species_8", 400:2400]
#  
#  # Check the results
#  dim(spec_sp8)
#  
#  # Plotting the subset result should work just fine
#  par(mfrow = c(1, 2), cex.main = 0.8, cex.axis = 0.6, cex.lab = 0.8)
#  
#  plot(spec_sp8, col = "red", main = "Species 8 spectra")
#  plot_quantile(spec, total_prob = 1.0, add = TRUE,  col = rgb(0.2, 0.2, 0.2, 0.2), border = FALSE)
#  plot_spec_regions(spec_sp8, default_spec_regions(), col = rgb(1, 0.5, 0, 0.1), add = TRUE)

## ---- error=TRUE---------------------------------------------------------
# Subset samples by index should work. It is also OK to subset by wavelength 
# using numerics or characters.
reflectance(spec_sp8[ 1 , "405"]) == reflectance(spec_sp8[ 1 , 405])

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# But remember that you CANNOT use indexes to subset wavelengths!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

# Something that is obvioulsy an index, like using 2 instead of 401 (the 2nd band), will fail.
spec_sp8[ , 2 ]

# However, `spectrolab` canot detect if you meant to get the two last bands when
# you use 2000:2001. It will assume that you wanted wavelengths "2000" and "2001"
# Bottomline, be very careful not to use indexes to subset wavelengths!

## ---- eval=F-------------------------------------------------------------
#  spec_new = spec
#  
#  # Replace names with an uppercase version
#  names(spec_new) = toupper(names(spec_new))
#  
#  # Check the results
#  names(spec_new)[1:5]

## ----  fig.height=3, fig.width=4, fig.align="center", eval=F-------------
#  # Scale reflectance by 0.75
#  # spec_new[] = reflectance(spec_new) * 0.75
#  spec_new = spec_new * 0.75
#  
#  # Plot the results
#  plot(spec, col = "blue", lwd = 0.75, cex.axis = 0.75)
#  plot(spec_new, col = "orange", lwd = 0.75, add = TRUE)

## ---- error=TRUE---------------------------------------------------------
# Trying to add 1.0 to all reflectance values will fail.
spec_new[] = reflectance(spec_new) + 1.0

## ---- error=TRUE---------------------------------------------------------
# Make a matrix from a `spectra` object
spec_as_mat = as.matrix(spec, fix_names = "none")
spec_as_mat[1:4, 1:3]

## ---- eval=F-------------------------------------------------------------
#  # Read Acer example spectra
#  acer_spectra <- read_spectra(path = dir_path, format ="sig",   exclude_if_matches = c("BAD","WR"))
#  
#  # and check result
#  plot(acer_spectra)
#  
#  # Jump correction
#  acer_juco <- jump_corr(acer_spectra)
#  
#  # Looks better!
#  plot(acer_juco)

## ---- eval=FALSE---------------------------------------------------------
#  ### Smooth only VIS/NIR or NIR/SWIR
#  Acer_smoo1 <- smoo.visnir.svc(Acer_juco)
#  Acer_smoo2 <- smoo.nirswir.svc(Acer_juco)
#  
#  ### Smooth both regions
#  Acer_smoo <- smoo.nirswir.svc(Acer_smoo1)
#  ### Same result
#  Acer_smoo <- smoo.visnir.svc(Acer_smoo2)

## ---- eval=FALSE---------------------------------------------------------
#  ### Some common examples, see `excl.` for more.
#  ### Exclude spectra with reflectances at the 'NIR shoulder' @ 761 nm <0.3 or >0.65
#  Acer_excl <- exclhilo_spec(Acer_juco)
#  
#  ### Exclude spectra with high reflectances in VIS: @ 450 nm >0.2
#  Acer_excl <- exclhivis_spec(Acer_juco)
#  
#  ### Exclude spectra with dips in NIR: @800 - @770 >0.02 ###
#  Acer_excl <- excldipnir_spec(Acer_juco)
#  
#  ### Create spectra object and plot
#  Acer_corr <- as.spectra (Acer_excl)
#  plot(Acer_corr)

