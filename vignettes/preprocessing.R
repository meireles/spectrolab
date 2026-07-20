## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse   = TRUE,
    comment    = "#>",
    fig.align  = "center",
    fig.retina = 2,
    dpi        = 150
)


## ----message = FALSE----------------------------------------------------------
library(spectrolab)

spec = as_spectra(spec_matrix_example, name_idx = 1)
one  = spec[1, ]   # a single spectrum, used for the "before/after" plots below


## ----fig.height=3.5, fig.width=7----------------------------------------------
s_gauss = smooth(one, method = "gaussian")
s_sg    = suppressMessages(smooth(one, method = "sgolay"))

oldpar = par(no.readonly = TRUE)
par(mfrow = c(1, 2))

plot(one, lwd = 1, col = "grey60", main = "Gaussian (default)")
plot(s_gauss, add = TRUE, col = "firebrick", lwd = 1)

plot(one, lwd = 1, col = "grey60", main = "Savitzky-Golay")
plot(s_sg, add = TRUE, col = "steelblue", lwd = 1)

par(oldpar)


## ----fig.height=3.5, fig.width=7----------------------------------------------
s_narrow = smooth_sgolay(one, p = 3, n = 5)    # default-sized window
s_wide   = smooth_sgolay(one, p = 3, n = 51)   # a much wider window

oldpar = par(no.readonly = TRUE)
par(mfrow = c(1, 2))

plot(one, lwd = 1, col = "grey60", main = "n = 5 (default-sized)")
plot(s_narrow, add = TRUE, col = "steelblue", lwd = 1)

plot(one, lwd = 1, col = "grey60", main = "n = 51")
plot(s_wide, add = TRUE, col = "steelblue", lwd = 1)

par(oldpar)


## ----fig.height=3.5, fig.width=7----------------------------------------------
d1 = suppressMessages(deriv_spectra(one, order = 1))
d2 = suppressMessages(deriv_spectra(one, order = 2))

oldpar = par(no.readonly = TRUE)
par(mfrow = c(1, 2))

plot(d1, main = "1st derivative", col = "darkgreen", ylab = "dReflectance/dWavelength")
abline(h = 0, col = "grey", lty = 2)

plot(d2, main = "2nd derivative", col = "purple", ylab = "d2Reflectance/dWavelength2")
abline(h = 0, col = "grey", lty = 2)

par(oldpar)


## ----fig.height=3.5, fig.width=7----------------------------------------------
one_cr = continuum_removal(one)

# Compute the hull the same way continuum_removal() does internally, purely
# to visualize it -- this is NOT required to use continuum_removal() itself.
w = bands(one)
r = as.numeric(value(one))
h = grDevices::chull(w, r)
chord = r[[1]] + (r[[length(r)]] - r[[1]]) * (w[h] - w[[1]]) / (w[[length(w)]] - w[[1]])
h = sort(h[r[h] >= chord - sqrt(.Machine$double.eps)])
hull = stats::approx(w[h], r[h], xout = w)$y

oldpar = par(no.readonly = TRUE)
par(mfrow = c(1, 2))

plot(one, lwd = 1, col = "grey30", main = "Spectrum and its continuum")
lines(w, hull, col = "firebrick", lwd = 1.5)

plot(one_cr, lwd = 1, col = "darkorange", main = "Continuum-removed")
abline(h = 1, col = "grey", lty = 2)

par(oldpar)


## -----------------------------------------------------------------------------
quantity(one) = "reflectance"
quantity(suppressMessages(deriv_spectra(one, order = 1)))

