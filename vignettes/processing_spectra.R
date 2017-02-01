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

