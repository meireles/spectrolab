## ----setup, include=FALSE-----------------------------------------------------
source("_common.R")
library("spectrolab")

## ----read-acer----------------------------------------------------------------
acer = read_spectra(system.file("extdata/Acer_example", package = "spectrolab"),
                    type               = "target_radiance",
                    exclude_if_matches = c("BAD", "WR"))

# Turn the filename design into metadata columns we can group by later.
parts = strsplit(names(acer), "_")
meta(acer, "plot")     = sapply(parts, `[`, 2)
meta(acer, "position") = sapply(parts, `[`, 4)

acer

## ----heterogeneity------------------------------------------------------------
serbin = read_spectra(system.file("extdata/svc_raw_and_overlap_matched_serbin/SVC_Files",
                                   package = "spectrolab"),
                      format = "sig")                        # SVC reflectance

psr    = read_spectra(system.file("extdata/psr_DN_brett", package = "spectrolab"),
                      format = "sed", type = "target_radiance")   # PSR digital numbers

# Same campaign spirit, three different band grids:
data.frame(
    dataset  = c("acer (radiance)", "serbin (reflectance)", "psr (DN)"),
    n_bands  = c(length(bands(acer)), length(bands(serbin)), length(bands(psr))),
    min_band = c(min(bands(acer)),    min(bands(serbin)),    min(bands(psr))),
    max_band = c(max(bands(acer)),    max(bands(serbin)),    max(bands(psr)))
)

## ----match, message=FALSE-----------------------------------------------------
acm = match_sensors(acer, splice_at = guess_splice_at(acer))

## ----resample, message=FALSE--------------------------------------------------
new_bands = seq(400, 2400, by = 5)                       # one common target grid
acer_rs   = resample(acm, new_bands = new_bands, fwhm = make_fwhm(acm, new_bands))

acer_rs

## ----combine-guard, error=TRUE------------------------------------------------
try({
# acm is on the native (997-band) grid; acer_rs is on the 5-nm grid.
combine(acm, acer_rs)
})

## ----combine------------------------------------------------------------------
batch_d2 = acer_rs[meta(acer_rs)$plot == "D2", ]
batch_f3 = acer_rs[meta(acer_rs)$plot == "F3", ]

campaign = combine(batch_d2, batch_f3)
campaign

# c() and rbind() are the idiomatic shorthands -- both just repeatedly
# apply combine(), so they preserve the spectra class and merge metadata.
identical(dim(c(batch_d2, batch_f3)), dim(campaign))

## ----aggregate----------------------------------------------------------------
agg = aggregate(acer_rs,
                by       = meta(acer_rs)$position,
                FUN      = mean,
                FUN_meta = try_keep_txt(mean))

agg
meta(agg)

## ----payoff, fig.width=7.2, fig.height=3.8, message=FALSE---------------------
gp = group_palette(acer_rs, by = "position")   # named colors, by position level

op = par(mfrow = c(1, 2), mar = c(4, 4, 2.5, 0.5))

## Before: raw scans, overlapping detectors, colored by position
plot(acer, col = species_cols(acer, by = "position"), lwd = 0.7,
     cex.axis = 0.8, main = "Before: 7 raw scans")

## After: harmonized per-position means with quantile ribbons
plot(acer_rs, type = "n", cex.axis = 0.8, main = "After: harmonized means")
for(p in names(gp$pal)){
    sp = acer_rs[meta(acer_rs)$position == p, ]
    plot_quantile(sp, col = adjustcolor(gp$pal[[p]], 0.25),
                  border = FALSE, add = TRUE)
}
plot(agg, col = gp$pal[names(agg)], lwd = 2, add = TRUE)
legend("topright", legend = names(gp$pal), col = gp$pal,
       lwd = 2, bty = "n", cex = 0.8)

par(op)

