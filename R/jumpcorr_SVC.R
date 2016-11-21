#' Jump correction of spectra
#'
#' @param spec spectra object with sensor jump between VIS/NIR and NIR/SWIR
#'
#' @return spectra object: jump corrected and interpolated to 1nm resolution
#'
#' @examples
#'
#' \dontrun{
#' jump_corr("raw_spectra_Acer")
#' }
#'
#' @export

jump_corr <- function (spec) {
    if( !is_spectra(spec) ){
        stop("Object must be of class spectra")
    }

    dat <- reflectance(spec)
    wvl <- wavelengths(spec)

    # split dat into three junks with increasing wavelengths
    dat1 <- dat [,1:which.max (wvl < cummax(wvl))-1]
    wvl1 <- wvl [1:which.max (wvl < cummax(wvl))-1]

    # "rest"
    datx <- dat [,which.max (wvl < cummax(wvl)):length(wvl)]
    wvlx <- wvl [which.max (wvl < cummax(wvl)): length(wvl)]

    dat2 <- datx[,1:which.max (wvlx < cummax(wvlx))-1]
    wvl2 <- wvlx[1:which.max (wvlx < cummax(wvlx))-1]

    dat3 <- datx [,which.max(wvlx < cummax(wvlx)):length(wvlx)]
    wvl3 <- wvlx [which.max(wvlx < cummax(wvlx)):length(wvlx)]

    # interpolate to 1 nm spectral resolution
    library(prospectr)
    devtools::use_package("prospectr",type = "Imports", pkg=".")
    wavi <- c(ceiling(wvl1)[1]:floor(wvl1)[length(wvl1)])
    dat11 <- prospectr::resample (dat1, wvl1, wavi, interpol="spline")
    wvl11 <- wavi
    wavi <- c(ceiling(wvl2)[1]:ceiling(wvl2)[length(wvl2)])
    dat22 <- prospectr::resample (dat2, wvl2, wavi, interpol="spline")
    wvl22 <- wavi
    wavi <- c(floor(wvl3)[1]:floor(wvl3)[length(wvl3)])
    dat33 <- prospectr::resample (dat3, wvl3, wavi, interpol="spline")
    wvl33 <- wavi

    # select overlap regions with duplicated wavelenghts
    datsub1 <- dat11[,which(colnames(dat11)%in%colnames(dat22))]
    datsub2 <- dat22 [,which(colnames(dat22)%in%colnames(dat11))]

    # find wavelength of closest approach
    diff <- colMeans(abs(datsub2-datsub1))

    # cut at closest approach
    dat111 <- dat11 [,1:which(colnames(dat11)==names(which(diff== min(diff))))]
    dat222 <- dat22 [,which(diff== min(diff)):ncol(dat22)]

    # find multiplier to stitch pieces together, leaving NIR unchanged
    a <- dat111[,ncol(dat111)]/dat222[,1]

    # lower VIS junk to NIR level
    dat111adj <- dat111/a

    # ... now the same for NIR and SWIR
    # select overlap regions with duplicated wavelenghts
    datsub2 <- dat222[,which(colnames(dat222)%in%colnames(dat33))]
    datsub3 <- dat33 [,which(colnames(dat33)%in%colnames(dat222))]

    # find wavelength of closest approach
    diff <- colMeans(abs(datsub3-datsub2))
    dat222 <- dat222 [,1:which(colnames(dat222)==names(which(diff== min(diff))))]
    dat333 <- dat33 [,which(diff== min(diff)):ncol(dat33)]

    # find multiplier to stitch pieces together, leaving NIR unchanged
    b <- dat222[,ncol(dat222)]/ dat333[,1]

    # lift SWIR junk to NIR level
    dat333adj <- dat333*b

    # combine three pieces
    spec_corr <- cbind (dat111adj[,1:ncol(dat111adj)-1], dat222, dat333adj[,-1])

    # rebuild spectra object
    spec_corri <- spectra(reflectance = spec_corr,
                        wavelengths = colnames(spec_corr),
                        names = names(spec))
    return(spec_corri)
    }

