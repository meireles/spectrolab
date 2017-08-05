## ----setup, include=FALSE------------------------------------------------
library("spectrolab")
knitr::opts_chunk$set(echo = TRUE)

## ---- fig.height=3.5, fig.width=4, fig.align='center'--------------------
plot(spec_with_jump, cex.lab = 0.5, cex.axis = 0.6)

sensors  = matrix(data     = c(400, 970, 971, 1909, 1910, 2500), 
                  ncol     = 3,
                  dimnames = list(c("begin", "end"), c("S1", "S2", "S3"))) 
plot_regions(spec_with_jump, regions = sensors, border = F, lwd = 0.15, lty = 3,
             col = adjustcolor(c("green", "blue", "orange"), 0.035), cex_label = 0.75)

## ---- warning=FALSE------------------------------------------------------
a = match_sensors(spec_with_jump, splice_at = c(971, 1910), fixed_sensor = 2,
                  interpolate_wvl = c(2, 2))
b = match_sensors(spec_with_jump, splice_at = c(971, 1910), fixed_sensor = 1,
                  interpolate_wvl = c(2, 2))
c = match_sensors(spec_with_jump, splice_at = c(971),       fixed_sensor = 2,
                  interpolate_wvl = c(2, 2))
d = match_sensors(spec_with_jump, splice_at = c(971),       fixed_sensor = 1,
                  interpolate_wvl = c(2, 2))
e = match_sensors(spec_with_jump, splice_at = c(1910),      fixed_sensor = 2,
                  interpolate_wvl = c(2, 2))
f = match_sensors(spec_with_jump, splice_at = c(1910),      fixed_sensor = 1,
                  interpolate_wvl = c(2, 2))

## ---- fig.height=6, fig.width=4, fig.align='center', echo=FALSE----------

par(mfrow = c(3, 2))

plot(spec_with_jump, main = "splice_at = c(971, 1910)\nfixed_sensor = 2 ", cex.main = 1, col = "grey35")
plot(a, add = TRUE, col = "red")
plot_regions(spec_with_jump, regions = sensors, border = F, lwd = 0.15, lty = 3,
             col = adjustcolor(c("green", "blue", "orange"), 0.035), cex_label = 0.65)

plot(spec_with_jump, main = "splice_at = c(971, 1910)\nfixed_sensor = 1 ", cex.main = 1, col = "grey35")
plot(b, add = TRUE, col = "red")
plot_regions(spec_with_jump, regions = sensors, border = F, lwd = 0.15, lty = 3,
             col = adjustcolor(c("green", "blue", "orange"), 0.035), cex_label = 0.65)


plot(spec_with_jump, main = "splice_at = 971\nfixed_sensor = 2 ", cex.main = 1, col = "grey35")
plot(c, add = TRUE, col = "red")
plot_regions(spec_with_jump, regions = sensors, border = F, lwd = 0.15, lty = 3,
             col = adjustcolor(c("green", "blue", "orange"), 0.035), cex_label = 0.65)


plot(spec_with_jump, main = "splice_at = 971\nfixed_sensor = 1 ", cex.main = 1, col = "grey35")
plot(d, add = TRUE, col = "red")
plot_regions(spec_with_jump, regions = sensors, border = F, lwd = 0.15, lty = 3,
             col = adjustcolor(c("green", "blue", "orange"), 0.035), cex_label = 0.65)

plot(spec_with_jump, main = "splice_at = 1910\nfixed_sensor = 2 ", cex.main = 1, col = "grey35",
     ylim = c(0, 0.65))
plot(e, add = TRUE, col = "red")
plot_regions(spec_with_jump, regions = sensors, border = F, lwd = 0.15, lty = 3,
             col = adjustcolor(c("green", "blue", "orange"), 0.035), cex_label = 0.65)


plot(spec_with_jump, main = "splice_at = 1910\nfixed_sensor = 1 ", cex.main = 1, col = "grey35",
     ylim = c(0, 0.65))
plot(f, add = TRUE, col = "red")
plot_regions(spec_with_jump, regions = sensors, border = F, lwd = 0.15, lty = 3,
             col = adjustcolor(c("green", "blue", "orange"), 0.035), cex_label = 0.65)


