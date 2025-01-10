## ----setup, include=FALSE-----------------------------------------------------
library("spectrolab")
knitr::opts_chunk$set(echo = TRUE)

## ----fig.height=8, fig.width=5, fig.align='center', echo=TRUE-----------------
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

## ----fig.height=8, fig.width=5, fig.align='center', echo=TRUE-----------------

# Spectrolab's guess of what the splice bands are.
# However, you should also visually inspect the spectra to determine what the
# boundaries between sensors are.
splice_bands_guess = guess_splice_at(reflect_raw)
splice_bands_guess

# Finally, if you know what those sensor bounds should be (say, they're given by
# the manufacturer), just use those numbers instead of spectrolab's guess.
splice_bands = c(990, 1900)

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

## ----fig.height=8, fig.width=5, fig.align='center', echo=TRUE-----------------
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

