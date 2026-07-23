## ----setup, include=FALSE-----------------------------------------------------
source("_common.R")
library("spectrolab")

## ----hook, fig.height=4.2, fig.width=7, echo=TRUE-----------------------------
# SIMULATED illustrative data: 50 samples x 2101 bands (400-2500 nm),
# grouped into 10 "species". Not real measurements.
s = as_spectra(spec_matrix_example, name_idx = 1)

plot(s, col = species_cols(s), lwd = 0.8, cex.axis = 0.8,
     main = "Simulated illustrative spectra, colored by species")
plot_regions(s, col = spec_region_tints, add = TRUE)

## ----read-acer----------------------------------------------------------------
dir_path = system.file("extdata/Acer_example", package = "spectrolab")

# Field crews often flag files: "_WR" for white references, "_BAD" for bad
# scans. `exclude_if_matches` drops any file whose name contains these tokens.
acer = read_spectra(dir_path, exclude_if_matches = c("BAD", "WR"))

acer

## ----read-meta, message=FALSE-------------------------------------------------
acer_m = read_spectra(dir_path,
                      exclude_if_matches = c("BAD", "WR"),
                      extract_metadata   = TRUE)

# A few header fields for the first three scans
meta(acer_m)[1:3, c("instrument", "integration1", "units")]

## ----as-spectra---------------------------------------------------------------
csv_path = system.file("extdata/spec_matrix_meta.csv", package = "spectrolab")

# check.names = FALSE keeps numeric band headers intact; otherwise R prepends
# an "X" (e.g. "X650"), which breaks the band labels.
d = read.csv(csv_path, check.names = FALSE)

# Column 3 holds sample names; columns 1-2 are metadata.
achillea = as_spectra(d, name_idx = 3, meta_idxs = c(1, 2))
achillea

## ----inspect------------------------------------------------------------------
dim(acer)          # samples x bands
nrow(acer)         # samples
head(names(acer))  # file names became sample names
range(bands(acer)) # first and last wavelength

## ----subset-------------------------------------------------------------------
# Samples by index; bands by label
vis = achillea[, 400:700]

# All samples whose name matches "ACHMI_7"
one = achillea["ACHMI_7", ]

# For non-integer band grids, select a range with bands(x, min, max)
acer_trim = acer[, bands(acer, 400, 2400)]

## ----subset-by----------------------------------------------------------------
# Keep only species with at least 3 samples (all 10 qualify here)
common = subset_by(s, by = names(s), n_min = 3, n_max = Inf)
dim(common)

## ----qa-envelope, fig.height=4.2, fig.width=7---------------------------------
plot(s, col = spec_muted, lwd = 0.5, cex.axis = 0.8,
     main = "Raw spectra with an 80% quantile envelope")
plot_quantile(s, total_prob = 0.8, col = spec_seq(3)[2], border = FALSE, add = TRUE)

## ----interactive, eval=FALSE--------------------------------------------------
# plot_interactive(s)

## ----out----------------------------------------------------------------------
# 1. A plain matrix: bands in columns, samples in rownames. Drops metadata.
m = as.matrix(achillea, fix_names = "none")
m[1:3, 1:3]

# 2. A data.frame, keeping metadata columns by default.
df = as.data.frame(achillea, fix_names = "none")
df[1:3, 1:4]

# 3. Long/tidy form: one row per sample-and-band (ideal for ggplot2).
lng = to_long(s)
head(lng, 3)

## ----indices------------------------------------------------------------------
ndvi = spectral_index$ndvi(s)   # (R800 - R680) / (R800 + R680)
pri  = spectral_index$pri(s)    # (R570 - R531) / (R570 + R531)

summary(ndvi)

## ----custom-index-------------------------------------------------------------
gndvi = make_spectral_index(800, 550)
head(round(gndvi(s), 3))

