## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(spectrolab)

## ----eval=TRUE----------------------------------------------------------------
dir_path = system.file("extdata/svc_raw_and_overlap_matched_serbin/SVC_Files_moc", package = "spectrolab")

s_raw = read_spectra(dir_path)

new_bands = seq(400, 2500, 1)

fwhm_A = make_fwhm(s_raw, new_bands)
fwhm_B = make_fwhm(s_raw, new_bands, return_type = "old")
fwhm_C = 1

AAA = resample_spec_fwhm(s_raw,
                         new_bands = new_bands,
                         fwhm = fwhm_A)
BBB = resample_spec_fwhm(s_raw,
                         new_bands = new_bands,
                         fwhm = fwhm_B)
CCC = resample_spec_fwhm(s_raw,
                         new_bands = new_bands,
                         fwhm = fwhm_C)

plot(s_raw, lwd = 0.2)
plot(AAA, add = TRUE, lwd = 0.1, col = "red")
plot(BBB, add = TRUE, lwd = 0.1, col = "blue")
plot(CCC, add = TRUE, lwd = 0.1, col = "purple")

