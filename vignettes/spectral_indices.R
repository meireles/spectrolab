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


## -----------------------------------------------------------------------------
names(spectral_index)

# Call an entry directly on a spectra object
head(spectral_index$ndvi(spec))
head(spectral_index$pri(spec))


## -----------------------------------------------------------------------------
idx = spectral_indices(spec)   # defaults to all of names(spectral_index)
head(idx)


## ----fig.height=4, fig.width=4.5----------------------------------------------
plot(idx$ndvi, idx$pri, xlab = "NDVI", ylab = "PRI", pch = 16,
     col = as.factor(names(spec)))


## -----------------------------------------------------------------------------
# A water-band index, e.g. R970 vs a reference band at R900
wbi = make_spectral_index(970, 900)
head(wbi(spec))


## ----error = TRUE-------------------------------------------------------------
try({
# spec is at 1nm resolution. Asking for an off-grid band (970.5) with
# tolerance = 0 fails rather than silently snapping to the nearest one...
off_grid = make_spectral_index(970.5, 900)
off_grid(spec, tolerance = 0)

# ...while the default tolerance (1) is enough to snap to band 970 and succeed
head(off_grid(spec))
})

