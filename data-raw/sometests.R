library(devtools)

devtools::install_github("annakat/spectrolab")
library(spectrolab)

### Inspecting original spectra
spec <- read.svc(myfolder, read = T)
spec_s <- as.spectra(spec)
is_spectra(spec_s)
plot(spec_s)  ### to fix: duplicated wavelengthts



##### Typical workfow ###
#### Jump corrected spectra ###
spec <- read.svc("./inst/extdata/", filename = T)
spec <- smoo.nirswir.svc(spec)
spec <- smoo.visnir.svc(spec)
spec_s <- as.spectra(spec)
plot(spec_s)



#### Jump corrected and filtered spectra ###
# spec <- read.svc("./inst/extdata/", filename = T, read = T)
spec <- jump.corr.svc("./inst/extdata/", filename = T)

spec <- clip.svc(spec)

spec_s <- as.spectra(spec)
plot(spec_s)


# m = as.data.frame( as.matrix(spec_s)) ### use this as input to jump corr
