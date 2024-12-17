## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)
library(spectrolab)

## ----eval=TRUE----------------------------------------------------------------
dir_path = system.file("extdata/svc_raw_and_overlap_matched_serbin/SVC_Files_moc", package = "spectrolab")

s_raw     = read_spectra(dir_path)

new_bands = seq(400, 2400, 5)
fwhm      = make_fwhm(s_raw, new_bands)

s_resamp  = resample(s_raw,new_bands = new_bands, fwhm = fwhm)

plot(s_raw, lwd = 0.5)
plot(s_resamp, add = TRUE, lwd = 0.25, col = "red")

