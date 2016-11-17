##############################################
###### Jump corrector SVC ####################
####### VIS/SWIR adjusted (same as ASD) ######
###### Oct 18 2016 ##########################

#' Jump corrector for SVC data
#'
#' Reads .sig files and corrects the jump at the sensor overlap regions,
#'     leaving the NIR unchanged
#'
#'
#' @param path Path to the input directory containing .sig files.
#' Higher level directories are supported.
#' @param filename Logical. Should the filename be included as the first column?
#' @return The output is a dataframe.
#' @examples
#' \dontrun{
#' jump_corr("path_to_your_folder", filename=TRUE)
#' }
#' @export

jump_corr <- function (path, filename=FALSE) {
  dat <- read_spec(path, read = T, filename = F)
  wvl <- read_wvl(path)
  dat1 <- dat [,1:which.max (wvl < cummax(wvl))-1]
  wvl1 <- as.numeric(names(dat1))
  datx <- dat [,which.max (wvl < cummax(wvl)):ncol(dat)]
  wvlx <- as.numeric(names(datx))
  dat2 <- datx[,1:which.max (wvlx < cummax(wvlx))-1]
  wvl2 <- as.numeric(names(dat2))
  dat3 <- datx [,which.max (wvlx < cummax(wvlx)):512]
  wvl3 <- as.numeric(names(dat3))
  library(prospectr)
  wavi <- c(ceiling(wvl1)[1]:floor(wvl1)[length(wvl1)])
  dat11 <- as.data.frame(resample (dat1, wvl1, wavi, interpol="spline"))
  wvl11 <- as.numeric(names(dat11))
  wavi <- c(ceiling(wvl2)[1]:ceiling(wvl2)[length(wvl2)])
  dat22 <- as.data.frame(resample (dat2, wvl2, wavi, interpol="spline"))
  wvl22 <- as.numeric(names(dat22))
  wavi <- c(floor(wvl3)[1]:floor(wvl3)[length(wvl3)])
  dat33 <- as.data.frame(resample (dat3, wvl3, wavi, interpol="spline"))
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

  if(filename==TRUE){
    ID <- read_filenames(path)
    spec_corr <- cbind(ID, spec_corr)
      }
  return(spec_corr)
}

