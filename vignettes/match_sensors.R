## ----setup, include=FALSE-----------------------------------------------------
source("_common.R")
library("spectrolab")

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

plot(reflect_raw, main = "Reflectance", col = spec_pal[1],
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)
abline(v = c(990, 1900), col = spec_pal[6], lty = 2, lwd = lwd)

plot(radiance_raw, main = "Radiance", col = spec_pal[1],
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)
abline(v = c(990, 1900), col = spec_pal[6], lty = 2, lwd = lwd)

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

plot(reflect_raw, main = "Reflectance", col = spec_pal[1],
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)
plot(reflect_matched, col = spec_pal[6], add = TRUE, lwd = lwd)
legend("topleft", legend = c("raw", "matched"),
       col = spec_pal[c(1, 6)], lwd = 1, bty = "n", cex = cex)

plot(radiance_raw, main = "Radiance", col = spec_pal[1],
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)
plot(radiance_matched, col = spec_pal[6], add = TRUE, lwd = lwd)
legend("topright", legend = c("raw", "matched"),
       col = spec_pal[c(1, 6)], lwd = 1, bty = "n", cex = cex)

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

plot(reflect_moc, main = "Reflectance", col = spec_pal[4],
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)
plot(reflect_matched, col = spec_pal[6], add = TRUE, lwd = lwd)
legend("topleft", legend = c("vendor (SVC)", "spectrolab legacy"),
       col = spec_pal[c(4, 6)], lwd = 1, bty = "n", cex = cex)

plot(radiance_moc, main = "Radiance", col = spec_pal[4],
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)
plot(radiance_matched, col = spec_pal[6], add = TRUE, lwd = lwd)
legend("topright", legend = c("vendor (SVC)", "spectrolab legacy"),
       col = spec_pal[c(4, 6)], lwd = 1, bty = "n", cex = cex)

par(oldpar)

## -----------------------------------------------------------------------------
reflect_svc_preset = match_sensors(reflect_raw, splice_at = c(970, 1901), method = "svc")

cat("legacy join kept", ncol(reflect_matched), "bands\n")
cat("svc preset kept ", ncol(reflect_svc_preset), "bands (vendor-matched file has", ncol(reflect_moc), ")\n")

## ----fig.height=4.2, fig.width=7, echo=TRUE, out.width="100%"-----------------
lwd = 0.6
cex = 0.7

# Three colors from the shared palette, reused in both panels
col_raw    = spec_pal[1]   # blue   -- raw, unmatched
col_preset = spec_pal[6]   # red    -- spectrolab "svc" preset
col_vendor = spec_pal[4]   # green  -- vendor (SVC) matched reference

oldpar = par(no.readonly = TRUE)
par(mfrow = c(1, 2))

# --- Full range -----------------------------------------------------------
plot(reflect_raw, main = "Full range", col = col_raw,
     lwd = lwd, cex.main = cex, cex.lab = cex, cex.axis = cex)
plot(reflect_moc,        col = col_vendor, add = TRUE, lwd = lwd)
plot(reflect_svc_preset, col = col_preset, add = TRUE, lwd = lwd)
legend("topleft", legend = c("raw", "svc preset", "vendor (SVC)"),
       col = c(col_raw, col_preset, col_vendor), lwd = 1, bty = "n", cex = cex)

# --- Splice zoom (first junction, ~970 nm) --------------------------------
plot(reflect_raw, main = "Splice zoom (~970 nm)", col = col_raw,
     xlim = c(930, 1010), lwd = lwd,
     cex.main = cex, cex.lab = cex, cex.axis = cex)
plot(reflect_moc,        col = col_vendor, add = TRUE, lwd = lwd)
plot(reflect_svc_preset, col = col_preset, add = TRUE, lwd = lwd)
abline(v = 970, col = spec_muted, lty = 2, lwd = lwd)

par(oldpar)

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

