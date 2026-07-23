## ----setup, include=FALSE-----------------------------------------------------
source("_common.R")
library("spectrolab")

## -----------------------------------------------------------------------------
s = as_spectra(spec_matrix_example, name_idx = 1)
dim(s)                     # 50 samples x 2101 bands (400-2500 nm)
length(unique(names(s)))   # 10 groups, carried in the sample names

## ----fig.width = 7, fig.height = 4.5------------------------------------------
gp = group_palette(s)

plot(s, col = species_cols(s), lwd = 1, ylab = "reflectance",
     xlab = "wavelength (nm)")
plot_regions(s, col = spec_region_tints)
legend("topleft", legend = names(gp$pal), col = gp$pal,
       lwd = 2, bty = "n", cex = 0.8, ncol = 2)

## ----fig.width = 7, fig.height = 4.5------------------------------------------
grp_of = stats::setNames(names(gp$pal)[match(gp$cols, gp$pal)], names(s))

plot(s, type = "n", ylab = "reflectance", xlab = "wavelength (nm)")
plot_regions(s, col = spec_region_tints)

for(g in names(gp$pal)){
    idx = which(grp_of == g)
    if(length(idx) >= 2){
        plot_quantile(s[idx, ], total_prob = 0.9, add = TRUE, border = FALSE,
                      col = grDevices::adjustcolor(gp$pal[[g]], 0.25))
    }
}
legend("topleft", legend = names(gp$pal), fill = gp$pal, bty = "n",
       cex = 0.8, ncol = 2, border = NA)

## ----eval = requireNamespace("ggplot2", quietly = TRUE), fig.width = 7, fig.height = 4----
long         = to_long(s)
long$id      = rep(seq_len(nrow(s)), times = ncol(s))
long$species = rep(grp_of, times = ncol(s))

ggplot2::ggplot(long, ggplot2::aes(band, value, group = id, colour = species)) +
    ggplot2::geom_line(alpha = 0.6, linewidth = 0.3) +
    ggplot2::scale_colour_manual(values = group_palette(s)$pal) +
    ggplot2::labs(x = "wavelength (nm)", y = "reflectance", colour = NULL) +
    theme_spectra()

## ----eval = requireNamespace("ggplot2", quietly = TRUE), fig.width = 7, fig.height = 5----
ggplot2::ggplot(long, ggplot2::aes(band, value, group = id, colour = species)) +
    ggplot2::geom_line(alpha = 0.7, linewidth = 0.3, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = group_palette(s)$pal) +
    ggplot2::facet_wrap(~ sample_name) +
    ggplot2::labs(x = "wavelength (nm)", y = "reflectance") +
    theme_spectra()

## ----eval = requireNamespace("ggplot2", quietly = TRUE), fig.width = 7, fig.height = 4----
qt  = quantile(s, probs = c(0.05, 0.5, 0.95))
qv  = value(qt)
fan = data.frame(band = bands(qt), lo = qv[1, ], mid = qv[2, ], hi = qv[3, ])

ggplot2::ggplot(fan, ggplot2::aes(band)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi),
                         fill = spec_pal[1], alpha = 0.25) +
    ggplot2::geom_line(ggplot2::aes(y = mid), colour = spec_pal[1], linewidth = 0.6) +
    ggplot2::labs(x = "wavelength (nm)", y = "reflectance",
                  title = "Median and 5-95% band") +
    theme_spectra()

## ----eval = requireNamespace("ggplot2", quietly = TRUE), fig.width = 7, fig.height = 3.5----
ggplot2::autoplot(s) + theme_spectra()

## ----fig.width = 6, fig.height = 5.5------------------------------------------
sc = resample(s, seq(400, 2400, by = 10), fwhm = 10)   # ~200 bands
M  = cor(as.matrix(sc))
wl = bands(sc)

op = graphics::par(mar = c(4, 4, 1, 1))
image(x = wl, y = wl, z = M, zlim = c(-1, 1),
      col = spec_div(255), useRaster = TRUE,
      xlab = "wavelength (nm)", ylab = "wavelength (nm)")
graphics::par(op)

## -----------------------------------------------------------------------------
pca = prcomp(as.matrix(s), center = TRUE)
imp = summary(pca)$importance
round(imp[, 1:5], 4)

## -----------------------------------------------------------------------------
ve  = round(imp[2, 1:2] * 100, 1)   # % variance explained by PC1, PC2
ve

## ----fig.width = 6, fig.height = 5--------------------------------------------
lab = paste0(c("PC1", "PC2"), " (", ve, "%)")

op = graphics::par(mar = c(4, 4, 1, 1))
plot(pca$x[, 1], pca$x[, 2], col = species_cols(s), pch = 19,
     xlab = lab[1], ylab = lab[2])
legend("topright", legend = names(gp$pal), col = gp$pal, pch = 19,
       bty = "n", cex = 0.8, ncol = 2)
graphics::par(op)

## ----fig.width = 7, fig.height = 4.5------------------------------------------
rot = pca$rotation[, 1:2]
wl2 = bands(s)

op = graphics::par(mar = c(4, 4, 1, 1))
matplot(wl2, rot, type = "n", xlab = "wavelength (nm)", ylab = "loading")
plot_regions(s, col = spec_region_tints)
graphics::abline(h = 0, col = spec_muted, lty = 3)
graphics::matlines(wl2, rot, col = c(spec_pal[1], spec_pal[6]), lty = 1, lwd = 2)
legend("topright", legend = lab, col = c(spec_pal[1], spec_pal[6]),
       lwd = 2, bty = "n", cex = 0.8)
graphics::par(op)

