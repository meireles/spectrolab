#' Match overlap function
#'
#' \code{match_overlap} Alternative to match_sensors for spectra with multiple
#' repeated wavelengths
#'
#' @param x spectra object
#' @param first Numeric. The first spectrum to plot. Corresponding to a row in a data.frame.
#' @param last Numeric. The last spectrum to plot. Corresponding to a row in a data.frame.
#' @return spectra object
#' @export

#' @author Anna K. Schweiger

match_overlap <- function (x) {
  dat <- as.matrix(x)
  wvl <- wavelengths(x)
  dat1 <- dat[,1:which.max(wvl < cummax(wvl))-1] ### cut when wvls start decreasing
  wvl1 <- as.numeric(colnames(dat1))
  datx <- dat [,which.max (wvl < cummax(wvl)):ncol(dat)]
  wvlx <- as.numeric(colnames(datx))
  dat2 <- datx[,1:which.max (wvlx < cummax(wvlx))-1]
  wvl2 <- as.numeric(colnames(dat2))
  dat3 <- datx [,which.max (wvlx < cummax(wvlx)):length(wvlx)]
  wvl3 <- as.numeric(colnames(dat3))

  wavi <- c(ceiling(wvl1)[1]:floor(wvl1)[length(wvl1)])
  dat11 <- as.data.frame(prospectr::resample(dat1, wvl1, wavi, interpol="spline"))
  wvl11 <- as.numeric(names(dat11))
  wavi <- c(ceiling(wvl2)[1]:ceiling(wvl2)[length(wvl2)])
  dat22 <- as.data.frame(prospectr::resample (dat2, wvl2, wavi, interpol="spline"))
  wvl22 <- as.numeric(names(dat22))
  wavi <- c(floor(wvl3)[1]:floor(wvl3)[length(wvl3)])
  dat33 <- as.data.frame(prospectr::resample (dat3, wvl3, wavi, interpol="spline"))
  wvl33 <- as.numeric(names(dat33))

  datsub1 <- dat11[,which(names(dat11)%in%names(dat22))]
  datsub2 <- dat22 [,which(names(dat22)%in%names(dat11))]
  diff <- colMeans(abs(datsub2-datsub1))
  dat111 <- dat11 [,1:which(colnames(dat11)==names(which(diff== min(diff))))]
  dat222 <- dat22 [,which(diff== min(diff)):ncol(dat22)]
  a <- dat111[,ncol(dat111)]/dat222[,1]
  dat111adj <- dat111/a

  datsub2 <- dat222[,which(names(dat222)%in%names(dat33))]
  datsub3 <- dat33 [,which(names(dat33)%in%names(dat222))]
  diff <- colMeans(abs(datsub3-datsub2))
  dat222 <- dat222 [,1:which(colnames(dat222)==names(which(diff== min(diff))))]
  dat333 <- dat33 [,which(diff== min(diff)):ncol(dat33)]
  b <- dat222[,ncol(dat222)]/ dat333[,1]
  dat333adj <- dat333*b

  spec_corr <- cbind (dat111adj[,1:ncol(dat111adj)-1], dat222, dat333adj[,-1])
  spec_corr <- as.spectra(spec_corr)
  return(spec_corr)
}
