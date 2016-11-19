
### Inspecting original spectra
spec <- read_spec("./inst/extdata/Acer_example/", read = T, filename = T)
# plot_rawspec(spec,1,2)
spect <- as.spectra(spec) ### doesnot like duplicated wvls


### After juco o.k.
spec <- jump_corr("./inst/extdata/Acer_example/", filename = T)
spect <- as.spectra(spec)

plot(spect)  ### to fix: duplicated wavelengthts

specn <- normalize_spectra(spect) ### normalise works
plot(specn)

as.numeric(names(spec))

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
