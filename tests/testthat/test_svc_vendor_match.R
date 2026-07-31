library("testthat")
library("spectrolab")

################################################################################
# Validation of match_sensors() against the vendor's own overlap-matched output.
#
# inst/extdata/svc_raw_and_overlap_matched_serbin/ ships the same 14 scans twice:
# SVC_Files/     -- raw, "[Overlap: Preserve, Matching Type: None]"
# SVC_Files_moc/ -- the identical scans reprocessed by SVC's software,
#                   "[Overlap: Remove @ 970,1901, Matching Type: Radiance @ 976 - 1010]"
#
# That makes the _moc set ground truth, and it is what every design decision in
# R/splice.R was measured against:
#
#   join only, no matching                                RMSE 0.00771
#   flat factor, no taper                                      0.00346
#   tapered factor, straight ramp, no crossfade                0.00141
#   + convex taper (power 1.3)                                 0.00117
#   + crossfade across the crossover window                    0.00040
#   + amplitude solved through the taper (current)             0.00013
#
# These tests pin that result, and would have caught each of the regressions
# above on the day it landed.
################################################################################

svc_raw_dir = function(){
    system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files",
                package = "spectrolab")
}
svc_moc_dir = function(){
    system.file("extdata", "svc_raw_and_overlap_matched_serbin", "SVC_Files_moc",
                package = "spectrolab")
}

read_quiet = function(d){
    suppressWarnings(suppressMessages(read_spectra(d, format = "sig")))
}

matched = function(x, splice_at = c(970, 1901)){
    suppressWarnings(suppressMessages(match_sensors(x, splice_at = splice_at)))
}

## the bare join: same cut, no gain match, no crossfade
joined = function(x, splice_at = c(970, 1901)){
    spectrolab:::i_trim_sensor_overlap(x, splice_at)[["spectra"]]
}

## RMSE of a matched object against the vendor's, over a wavelength range.
## Bands are compared on the vendor's grid; the caller checks grid equality
## separately so a mismatch shows up as its own failure, not as a bad number.
rmse_vs = function(fit, vendor, lo = -Inf, hi = Inf){
    fb  = round(bands(fit), 4)
    vb  = round(bands(vendor), 4)
    k   = match(vb, fb)
    sel = vb >= lo & vb <= hi
    d   = value(fit)[, k[sel], drop = FALSE] - value(vendor)[, sel, drop = FALSE]
    sqrt(mean(d^2))
}


test_that("the fixture pair is what these tests assume it is", {
    skip_if(svc_raw_dir() == "" || svc_moc_dir() == "", "serbin extdata not installed")

    raw = read_quiet(svc_raw_dir())
    moc = read_quiet(svc_moc_dir())

    ## raw still carries the physical detector overlap; moc is already joined
    expect_false(spectrolab:::i_is_increasing(bands(raw)))
    expect_true(spectrolab:::i_is_increasing(bands(moc)))
    expect_equal(unname(nrow(raw)), unname(nrow(moc)))

    ## the vendor recorded both what it removed and where it matched
    si = sensor_info(moc)
    expect_true(all(si$splice_1 == 970 & si$splice_2 == 1901))
    expect_true(all(si$match_lo == 976 & si$match_hi == 1010))
})


test_that("match_sensors reproduces the vendor's band grid exactly", {
    skip_if(svc_raw_dir() == "" || svc_moc_dir() == "", "serbin extdata not installed")

    raw = read_quiet(svc_raw_dir())
    moc = read_quiet(svc_moc_dir())

    ## "Remove @ 970" means the left detector stops BELOW 970, so the 970.8 nm
    ## point it also sampled is deleted. Trimming at the right sensor's first
    ## wavelength instead used to keep it -- 983 bands against the vendor's 982.
    expect_equal(bands(matched(raw)), bands(moc))
})


test_that("match_sensors lands on the vendor's own result", {
    skip_if(svc_raw_dir() == "" || svc_moc_dir() == "", "serbin extdata not installed")

    raw = read_quiet(svc_raw_dir())
    moc = read_quiet(svc_moc_dir())

    e_join = rmse_vs(joined(raw), moc)
    e_fit  = rmse_vs(matched(raw), moc)

    ## The regression guard: matching must be an improvement over the bare join.
    expect_lt(e_fit, e_join)

    ## It is in fact a ~60x improvement. The bound is loose enough not to be
    ## brittle and tight enough that dropping the crossfade (0.00105), the solved
    ## amplitude (0.00040) or the convex taper (0.00088) all fail it.
    expect_lt(e_fit, 0.0003)
    expect_gt(e_join, 0.005)
})


test_that("every detector lands on the vendor's, not just the corrected one", {
    skip_if(svc_raw_dir() == "" || svc_moc_dir() == "", "serbin extdata not installed")

    raw = read_quiet(svc_raw_dir())
    moc = read_quiet(svc_moc_dir())
    fit = matched(raw)

    ## Detector 1 carries the gain correction ...
    expect_lt(rmse_vs(fit, moc, hi = 969.9), 0.0003)
    ## ... detector 2 is untouched above the crossover but crossfaded below it,
    ## which is where a plain cut is worst (0.00218 there) ...
    expect_lt(rmse_vs(fit, moc, lo = 970, hi = 1900.9), 0.0003)
    ## ... and detector 3 comes through the join untouched, as the vendor's does:
    ## a single band out of 255 x 14 differs, by one unit of the file's own
    ## two-decimal rounding. This is the assertion that fails if the far junction
    ## is ever gain-matched: doing that puts the overall RMSE at 0.02263.
    expect_lt(rmse_vs(fit, moc, lo = 1901), 1e-5)
    expect_identical(value(fit)[ , bands(fit) >= 1901],
                     value(joined(raw))[ , bands(fit) >= 1901])
})


test_that("the seam is as smooth as the vendor's", {
    skip_if(svc_raw_dir() == "" || svc_moc_dir() == "", "serbin extdata not installed")

    raw = read_quiet(svc_raw_dir())
    moc = read_quiet(svc_moc_dir())

    step = function(x, w){
        b = bands(x)
        i = max(which(b <  w))
        j = min(which(b >= w))
        mean(abs(value(x)[ , j] / value(x)[ , i] - 1))
    }

    ## Across the matched junction: a bare cut leaves ~10%, the vendor ~0.2%.
    expect_gt(step(joined(raw), 970), 0.05)
    expect_lt(step(moc, 970),         0.01)
    expect_lt(step(matched(raw), 970), 2 * step(moc, 970))

    ## Across the unmatched one the vendor leaves a large step of its own -- both
    ## detectors are inside the 1900 nm water band and genuinely disagree. We
    ## reproduce that rather than papering over it.
    expect_gt(step(moc, 1901), 0.15)
    expect_equal(step(matched(raw), 1901), step(moc, 1901))
})


test_that("the vendor's taper is convex, which is why ours is", {
    skip_if(svc_raw_dir() == "" || svc_moc_dir() == "", "serbin extdata not installed")

    raw = read_quiet(svc_raw_dir())
    moc = read_quiet(svc_moc_dir())

    ## The vendor's correction to detector 1, read straight off the fixture pair:
    ## the ratio of its output to its input, over a window of bands (single-band
    ## ratios are not usable -- a few raw reflectances are exactly 0).
    ratio_at = function(w0, half_width = 15){
        sel = bands(moc) >= w0 - half_width & bands(moc) <= w0 + half_width &
              bands(moc) < 970
        k   = match(round(bands(moc)[sel], 4), round(bands(raw), 4))
        rowMeans(value(moc)[ , sel, drop = FALSE]) /
        rowMeans(value(raw)[ , k,   drop = FALSE])
    }

    ## no correction at the blue end, a real one near the junction
    expect_equal(mean(ratio_at(355)), 1, tolerance = 2e-3)
    expect_lt(mean(ratio_at(960)), 0.98)

    ## and convex in between: a straight ramp would put the correction at the
    ## detector's midpoint near 0.51 of the one at 960 nm; the vendor puts it at
    ## 0.44 (a power of ~1.3 on the wavelength fraction).
    frac = mean((ratio_at(654) - 1) / (ratio_at(960) - 1))
    expect_lt(frac, 0.48)
    expect_gt(frac, 0.40)
})


test_that("the 1901 nm junction gain is noise, which is why it is not applied", {
    skip_if(svc_raw_dir() == "", "serbin extdata not installed")

    raw = read_quiet(svc_raw_dir())
    b   = bands(raw)
    v   = value(raw)

    ## Reconstruct the factor the engine would compute at the SWIR1/SWIR2
    ## overlap, to document WHY that junction is excluded rather than just that
    ## it is.
    bounds = spectrolab:::i_find_sensor_overlap_bounds(b)
    s2 = seq.int(bounds[["begin", 2]], bounds[["end", 2]])
    s3 = seq.int(bounds[["begin", 3]], bounds[["end", 3]])
    lo = max(min(b[s2]), min(b[s3]))
    hi = min(max(b[s2]), max(b[s3]))

    lf  = s2[b[s2] >= lo & b[s2] <= hi]
    rf  = s3[b[s3] >= lo & b[s3] <= hi]
    fac = rowMeans(v[, rf, drop = FALSE]) / rowMeans(v[, lf, drop = FALSE])

    ## A handful of bands, all in the 1900 nm water band, at very low signal.
    expect_lt(length(c(lf, rf)), 12)
    expect_lt(mean(v[, c(lf, rf)]), 0.10)

    ## and the resulting factors are nowhere near a real detector gain
    expect_true(all(fac < 0.85))
})


test_that("match_sensors matches only the first junction when sensors overlap", {
    skip_if(svc_raw_dir() == "", "serbin extdata not installed")

    raw = read_quiet(svc_raw_dir())
    fit = matched(raw)
    cut = joined(raw)
    b   = bands(fit)

    ## detector 1 is rescaled ...
    expect_false(isTRUE(all.equal(value(fit)[, b < 970], value(cut)[, b < 970])))
    ## ... and everything above the physical overlap is left as joined. Looping
    ## over every junction here scaled detector 3 by a ramp starting at 1.50.
    expect_equal(value(fit)[, b > 1016.6], value(cut)[, b > 1016.6])
})
