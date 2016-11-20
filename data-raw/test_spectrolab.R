#############################################
#### Predicting traits from SVC spectra ####
#### Example 11-20-2016 AS #################

### read spectra ####
library("spectrolab")
### example spectra
dir_path = system.file("extdata", "Acer_example", package = "spectrolab")
spec <- read_spectra(dir_path, format = "svc",
                     exclude_if_matches = c("BAD","WR"))

### jump correction
spec_corr <- jump_corr(spec)
plot(spec_corr)

### vector normalization
spec_corr <- normalize_spectra(spec_corr)


### Inspecting original spectra
path <- ("./inst/extdata/Acer_example/")
spec <-read_spectra("./inst/extdata/Acer_example/",format = "sig",
                    exclude_if_matches = c("WR","BAD"))

plot(spec)


jumpi <- jump_corr(spec)

plot(jumpi)

##### Typical workfow ###
#### Jump corrected spectra ###

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
