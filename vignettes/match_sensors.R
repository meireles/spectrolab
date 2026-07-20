## ----setup, include=FALSE-----------------------------------------------------
library("spectrolab")
knitr::opts_chunk$set(
    echo       = TRUE,
    collapse   = TRUE,
    comment    = "#>",
    fig.align  = "center",
    fig.retina = 2,
    dpi        = 150
)


## ----fig.height=8, fig.width=5, fig.align='center', echo=TRUE, out.width="100%"----
# Path to raw (unmatched) spectra
path_raw = system.file("extdata/svc_raw_and_overlap_matched_serbin/SVC_Files/",
                       package = "spectrolab")

# Read spectra as reflectance and radiance
reflect_raw  = read_spectra(path = path_raw, type = "target_reflectance")

radiance_raw = read_spectra(path = path_raw, type = "target_radiance")

# Sensor overlaps marked with vertical dashed lines
lwd = 0.5
cex = 0.7

oldpar = par(no.readonly = TRUE)
par(mfrow = c(2, 1))

plot(reflect_raw, main = "Reflectance",
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)
abline(v = c(990, 1900), col = "red", lty = 2, lwd = lwd)

plot(radiance_raw, main = "Radiance",
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)
abline(v = c(990, 1900), col = "red", lty = 2, lwd = lwd)

par(oldpar)


## -----------------------------------------------------------------------------
sensor_info(reflect_raw)[1:2, ]


## ----fig.height=8, fig.width=5, fig.align='center', echo=TRUE, out.width="100%"----
# Spectrolab's guess of what the splice bands are.
# However, you should also visually inspect the spectra to determine what the
# boundaries between sensors are.
splice_bands_guess = guess_splice_at(reflect_raw)
splice_bands_guess

# Finally, if you know what those sensor bounds should be (say, they're given by
# the manufacturer, or read off the instrument's own header), use those numbers
# instead of spectrolab's guess. This file's own header records 970/1901 (see
# sensor_info() of the matched reference file below) rather than the nominal
# 990/1900 from the manual -- always prefer what the file says over the manual.
splice_bands = c(990, 1900)


## ----fig.height=8, fig.width=5, fig.align='center', echo=TRUE, out.width="100%"----
# Match the reflectance and radiance data
reflect_matched = match_sensors(x = reflect_raw, splice_at = splice_bands,
                                interpolate_wvl = c(5, 1))

radiance_matched = match_sensors(x = radiance_raw, splice_at = splice_bands,
                                 interpolate_wvl = c(5, 1))

lwd = 0.5
cex = 0.7

oldpar = par(no.readonly = TRUE)

par(mfrow = c(2, 1))

plot(reflect_raw, main = "Reflectance",
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)

plot(reflect_matched, col = "red", add = TRUE,
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)

plot(radiance_raw, main = "Radiance",
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)

plot(radiance_matched, col = "red", add = TRUE,
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)

par(oldpar)


## ----fig.height=8, fig.width=5, fig.align='center', echo=TRUE, out.width="100%"----
path_moc = system.file("extdata/svc_raw_and_overlap_matched_serbin/SVC_Files_moc/",
                       package = "spectrolab")

reflect_moc  = read_spectra(path = path_moc, type = "target_reflectance")

radiance_moc = read_spectra(path = path_moc, type = "target_radiance")

lwd = 0.5
cex = 0.7

oldpar = par(no.readonly = TRUE)

par(mfrow = c(2, 1))

plot(reflect_moc, main = "Reflectance", col = "black",
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)

plot(reflect_matched, col = "red", add = TRUE,
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)

plot(radiance_moc, main = "Radiance", col = "black",
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)

plot(radiance_matched, col = "red", add = TRUE,
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)

par(oldpar)


## -----------------------------------------------------------------------------
reflect_svc_preset = match_sensors(reflect_raw, splice_at = c(970, 1901), method = "svc")

cat("legacy join kept", ncol(reflect_matched), "bands\n")
cat("svc preset kept ", ncol(reflect_svc_preset), "bands (vendor-matched file has", ncol(reflect_moc), ")\n")


## -----------------------------------------------------------------------------
custom_cfg = splice_config(gain_type = "multiplicative", reference = "right",
                           clamp = c(0.8, 1.2), graded = TRUE, join = "cut")
custom_cfg

reflect_custom = match_sensors(reflect_raw, splice_at = c(970, 1901), config = custom_cfg)


## -----------------------------------------------------------------------------
match_sensors(reflect_moc)


## ----error=TRUE---------------------------------------------------------------
try({
match_sensors(reflect_raw)
})

