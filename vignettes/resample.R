## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse   = TRUE,
    comment    = "#>",
    fig.align  = "center",
    fig.retina = 2,
    dpi        = 150
)

library(spectrolab)


## ----eval=TRUE, out.width="100%"----------------------------------------------
dir_path = system.file("extdata/svc_raw_and_overlap_matched_serbin/SVC_Files_moc", package = "spectrolab")

s_raw     = read_spectra(dir_path)

new_bands = seq(400, 2400, 5)
fwhm      = make_fwhm(s_raw, new_bands)

s_resamp  = resample(s_raw, new_bands = new_bands, fwhm = fwhm)

plot(s_raw, lwd = 0.5)
plot(s_resamp, add = TRUE, lwd = 0.25, col = "red")


## -----------------------------------------------------------------------------
# FWHM computed from make_fwhm() is one value per new band
length(fwhm) == length(new_bands)
range(fwhm)


## -----------------------------------------------------------------------------
# Dense in the VIS/NIR (2nm), sparse in the SWIR (10nm)
uneven_bands = c(seq(400, 1000, 2), seq(1005, 2400, 10))

fwhm_default = make_fwhm(s_raw, uneven_bands)          # k = 3 (default)
fwhm_full    = make_fwhm(s_raw, uneven_bands, k = 0)   # no clustering

length(unique(round(fwhm_default, 3)))  # only 3 distinct values...
length(unique(round(fwhm_full, 3)))     # ...vs the true band-to-band variation


## -----------------------------------------------------------------------------
# Ask for bands starting a bit before and ending a bit after the original range
too_wide = seq(330, 2525, 5)
s_trim   = resample(s_raw, new_bands = too_wide, fwhm = make_fwhm(s_raw, too_wide))

range(bands(s_raw))     # what we actually measured
range(bands(s_trim))    # what we got back -- trimmed to fit

