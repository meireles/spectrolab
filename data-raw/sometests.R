devtools::load_all(".")

### Inspecting original spectra
spec <- read_spec("./inst/extdata/Acer_example/", read = T, filename = T)
plot_rawspec(spec,1,2)

as.numeric(names(spec[,-1])[505:525])

spec <- jump_corr(myfolder, filename = T)

as.numeric(names(spec))

spec <- as.spectra(spec)

is_spectra(spec_s)
plot(spec_s)  ### to fix: duplicated wavelengthts



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
