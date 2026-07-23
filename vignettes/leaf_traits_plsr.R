## ----setup, include=FALSE-----------------------------------------------------
source("_common.R")
library("spectrolab")

## ----echo=FALSE---------------------------------------------------------------
if(!requireNamespace("pls", quietly = TRUE)){
    knitr::asis_output("The **pls** package is required to build this vignette. Install it with `install.packages(\"pls\")`.")
    knitr::knit_exit()
}

## -----------------------------------------------------------------------------
library("pls")

## -----------------------------------------------------------------------------
s = as_spectra(spec_matrix_example, name_idx = 1)
s

## -----------------------------------------------------------------------------
## #############################################################################
## ## SIMULATED TRAITS -- replace this block with your measured LMA/Chl        ##
## ## The weights and noise below are ILLUSTRATIVE, not real calibrations.     ##
## #############################################################################
simulate_traits = function(s){
    set.seed(1)
    X = as.matrix(s)                                   # 50 x 2101, cols named by band
    b = function(nm) X[, which.min(abs(as.numeric(colnames(X)) - nm))]
    n = nrow(X)

    ## LMA (g/m^2): SWIR dry-matter / water bands
    lma = 120 + 900*(b(1730) - mean(b(1730))) +
                700*(b(1980) - mean(b(1980))) + rnorm(n, 0, 6)

    ## Chl (ug/cm^2): green vs red-edge contrast
    chl =  45 + 2100*(b(550) - mean(b(550))) -
                2800*(b(705) - mean(b(705))) + rnorm(n, 0, 4)

    data.frame(LMA = lma, Chl = chl)
}

traits = simulate_traits(s)
summary(traits)

## -----------------------------------------------------------------------------
set.seed(42)
train = sort(sample(nrow(s), 35))
test  = setdiff(seq_len(nrow(s)), train)

Xtrain = as.matrix(s[train, ])       # 35 x 2101, bands as columns
Ytrain = as.matrix(traits[train, ])  # 35 x 2   (LMA, Chl)
Xtest  = as.matrix(s[test, ])
Ytest  = as.matrix(traits[test, ])

fit = pls::plsr(Ytrain ~ Xtrain, ncomp = 15, validation = "LOO")

## ----fig.height = 4.2---------------------------------------------------------
pls::validationplot(fit, val.type = "RMSEP", legendpos = "topright")

cv = pls::RMSEP(fit, estimate = "CV")$val[1, , ]   # traits x (0..ncomp)
k  = which.min(colMeans(cv / cv[, 1])) - 1
k

## -----------------------------------------------------------------------------
pred_test = predict(fit, ncomp = k, newdata = Xtest)[, , 1]   # 15 x 2

perf = function(obs, prd){
    c(R2   = cor(obs, prd)^2,
      RMSE = sqrt(mean((obs - prd)^2)),
      bias = mean(prd - obs))
}
round(t(sapply(colnames(Ytest), function(j) perf(Ytest[, j], pred_test[, j]))), 3)

## ----fig.width = 7, fig.height = 3.8------------------------------------------
op = par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
for(j in seq_along(colnames(Ytest))){
    tr = colnames(Ytest)[j]
    o  = Ytest[, tr]; p = pred_test[, tr]
    lim = range(o, p)
    plot(o, p, xlim = lim, ylim = lim, pch = 19, col = spec_pal[j],
         xlab = paste("observed", tr), ylab = paste("predicted", tr),
         main = sprintf("%s  (R2 = %.2f)", tr, cor(o, p)^2))
    abline(0, 1, col = spec_muted, lwd = 2)   # 1:1 line
}
par(op)

## ----fig.width = 7, fig.height = 5--------------------------------------------
B = coef(fit, ncomp = k)          # 2101 x 2 x 1 array (no intercept)
wl = bands(s)

div_cols = function(v){
    rng = max(abs(v))
    idx = round((v / rng + 1) / 2 * 254) + 1     # -max -> 1 (blue), +max -> 255 (red)
    spec_div(255)[idx]
}

op = par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
for(j in seq_len(dim(B)[2])){
    tr = dimnames(B)[[2]][j]
    bc = B[, j, 1]
    plot(wl, bc, type = "n", xlab = "wavelength (nm)",
         ylab = paste(tr, "coefficient"), main = tr)
    plot_regions(s, col = spec_region_tints)     # shaded VIS/NIR/SWIR1/SWIR2
    abline(h = 0, col = spec_muted)
    points(wl, bc, col = div_cols(bc), pch = 19, cex = 0.35)
}
par(op)

## -----------------------------------------------------------------------------
newspec = s[test, ]                              # pretend these are "new" leaves
Bi   = coef(fit, ncomp = k, intercept = TRUE)    # (2101+1) x 2 x 1
Bmat = Bi[, , 1]                                 # 2102 x 2 ; row 1 = intercept
dim(Bmat)

## -----------------------------------------------------------------------------
Xnew = as.matrix(newspec)
pred = cbind(1, Xnew) %*% Bmat                   # n x 2

## Verify it is byte-for-byte what predict() computes.
stopifnot(all.equal(unname(pred),
                    unname(predict(fit, ncomp = k, newdata = Xnew)[, , 1]),
                    tolerance = 1e-8))

## -----------------------------------------------------------------------------
pred_native = newspec %*% Bmat[-1, ]             # spectra %*% slopes  -> matrix
pred_native = sweep(pred_native, 2, Bmat[1, ], "+")   # add per-trait intercept

stopifnot(all.equal(unname(pred_native), unname(pred), tolerance = 1e-8))
class(pred_native)

## -----------------------------------------------------------------------------
coarse = resample(newspec, seq(400, 2500, by = 5), fwhm = 5)  # 5 nm grid
c(coarse = length(bands(coarse)), model = length(bands(s)))

## ----error = TRUE-------------------------------------------------------------
try({
cbind(1, as.matrix(coarse)) %*% Bmat
})

## -----------------------------------------------------------------------------
fixed      = resample(coarse, bands(s), fwhm = 1)   # back to the model's 1 nm grid
pred_fixed = cbind(1, as.matrix(fixed)) %*% Bmat
round(cor(Ytest[, "LMA"], pred_fixed[, "LMA"])^2, 3)

## -----------------------------------------------------------------------------
rownames(pred) = names(newspec)
predicted = data.frame(species = names(newspec), pred, row.names = NULL)
head(predicted)

## ----fig.width = 7, fig.height = 4--------------------------------------------
sp   = factor(predicted$species)
pal  = species_cols(newspec)[!duplicated(predicted$species)]
names(pal) = predicted$species[!duplicated(predicted$species)]

op = par(mfrow = c(1, 2), mar = c(6, 4, 2, 1))
for(tr in c("LMA", "Chl")){
    boxplot(predicted[[tr]] ~ sp, col = pal[levels(sp)],
            xlab = "", ylab = tr, main = paste("predicted", tr),
            las = 2, cex.axis = 0.7)
}
par(op)

## -----------------------------------------------------------------------------
pred_no_intercept = as.matrix(newspec) %*% Bmat[-1, ]     # forgot the 1s column
round(mean(pred_no_intercept[, "LMA"] - Ytest[, "LMA"]), 1)   # huge bias

