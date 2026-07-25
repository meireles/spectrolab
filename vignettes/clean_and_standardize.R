## ----setup, include=FALSE-----------------------------------------------------
source("_common.R")
library("spectrolab")

## ----message = FALSE----------------------------------------------------------
s = as_spectra(spec_matrix_example, name_idx = 1)   # simulated: 50 samples, 10 species
s

one = s[1, ]   # a single spectrum, for the before/after smoothing panels

## ----fig.height=3.5, fig.width=7, eval = requireNamespace("signal", quietly = TRUE)----
s_gauss = smooth(one, method = "gaussian")
s_sg    = suppressMessages(smooth(one, method = "sgolay"))

oldpar = par(no.readonly = TRUE)
par(mfrow = c(1, 2))

plot(one, lwd = 1, col = spec_muted, main = "Gaussian (default)")
plot(s_gauss, add = TRUE, col = spec_pal[6], lwd = 1)

plot(one, lwd = 1, col = spec_muted, main = "Savitzky-Golay")
plot(s_sg, add = TRUE, col = spec_pal[1], lwd = 1)

par(oldpar)

## ----fig.height=3.5, fig.width=7, eval = requireNamespace("signal", quietly = TRUE)----
s_narrow = smooth_sgolay(one, p = 3, n = 5)    # default-sized window
s_wide   = smooth_sgolay(one, p = 3, n = 51)   # a much wider window

oldpar = par(no.readonly = TRUE)
par(mfrow = c(1, 2))

plot(one, lwd = 1, col = spec_muted, main = "n = 5 (default-sized)")
plot(s_narrow, add = TRUE, col = spec_pal[1], lwd = 1)

plot(one, lwd = 1, col = spec_muted, main = "n = 51")
plot(s_wide, add = TRUE, col = spec_pal[1], lwd = 1)

par(oldpar)

## ----fig.height=3.5, fig.width=7, eval = requireNamespace("signal", quietly = TRUE)----
d1 = suppressMessages(deriv_spectra(one, order = 1))
d2 = suppressMessages(deriv_spectra(one, order = 2))

# The red-edge position: wavelength of the largest 1st-derivative value in the
# red/NIR transition (roughly 680-760 nm).
red_edge_win = bands(d1) >= 680 & bands(d1) <= 760
rep_nm       = bands(d1)[red_edge_win][which.max(value(d1)[1, red_edge_win])]

oldpar = par(no.readonly = TRUE)
par(mfrow = c(1, 2))

plot(d1, main = "1st derivative", col = spec_pal[4],
     ylab = "dReflectance/dWavelength")
abline(h = 0, col = spec_grid, lty = 2)
abline(v = rep_nm, col = spec_pal[6], lwd = 1.5)
text(rep_nm, max(value(d1)[1, ]), labels = paste0("red edge\n~", rep_nm, " nm"),
     pos = 4, col = spec_pal[6], cex = 0.8)

plot(d2, main = "2nd derivative", col = spec_pal[5],
     ylab = "d2Reflectance/dWavelength2")
abline(h = 0, col = spec_grid, lty = 2)

par(oldpar)

## ----out.width="100%"---------------------------------------------------------
new_bands = seq(400, 2400, 5)
fwhm      = make_fwhm(s, new_bands)

s_resamp  = resample(s, new_bands = new_bands, fwhm = fwhm)

plot(s, lwd = 0.5, col = spec_muted, main = "Original (grey) vs resampled (red)")
plot(s_resamp, add = TRUE, lwd = 0.25, col = spec_pal[6])

## -----------------------------------------------------------------------------
length(fwhm) == length(new_bands)   # one value per new band
range(fwhm)

## -----------------------------------------------------------------------------
too_wide = seq(330, 2525, 5)
s_wide   = suppressWarnings(resample(s, new_bands = too_wide, fwhm = 5))

range(bands(s))                  # what we actually measured
range(bands(s_wide))             # the full requested grid is preserved ...
sum(is.na(value(s_wide)[1, ]))   # ... but the out-of-range bands are NA

## -----------------------------------------------------------------------------
s_norm = normalize(s)

## ----fig.height=4, fig.width=8------------------------------------------------
cols = species_cols(s)

oldpar = par(no.readonly = TRUE)
par(mfrow = c(1, 2))

plot(s, lwd = 0.6, col = cols, main = "Raw (brightness scatter)")

plot(s_norm, lwd = 0.6, col = cols, main = "Vector normalized (shape only)")

par(oldpar)

## ----fig.height=3.5, fig.width=7----------------------------------------------
sp        = names(s)[1]
one_sp    = s[names(s) == sp, ]
one_sp_n  = s_norm[names(s_norm) == sp, ]

oldpar = par(no.readonly = TRUE)
par(mfrow = c(1, 2))

plot_quantile(one_sp, total_prob = 0.95, col = spec_region_tints[2],
              border = spec_pal[2], main = paste(sp, "- raw"))
plot_quantile(one_sp_n, total_prob = 0.95, col = spec_region_tints[1],
              border = spec_pal[1], main = paste(sp, "- normalized"))

par(oldpar)

