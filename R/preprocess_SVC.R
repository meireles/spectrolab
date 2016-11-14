##############################################
###### Preprocessing SVC data  ###############
#############################################
###### 10-20-2016 ##########################

###### remove start and end of spectrum ####
clip.svc <- function (spec){
  range <- seq(400,2400,1)
  return (clipspec <- spec[,as.numeric(names(spec)) %in% range])
  
}

#### ALL EXCLUSION FUNCTIONS WORK ON SPECTRA SCALED TO 0-1 reflectance
#### exclude spectra with NIR shoulder <0.30, >0.65 @761 nm

excl.hilo.svc <- function(x){
  sub_spec <- subset(x,x$`761`>0.30 & x$`761`<0.65)
  
}

### bad measurements high in vis
excl.hi.vis.svc <- function(x){
  sub_spec <- subset(x,x$`450`< 0.2)
}

excl.hi.start.svc <- function(x){
  sub_spec <- subset(x,x$`400`< 0.15)
}


### exclude spectra with bad start ###
excl.badstart.svc <- function (x) {
  sub_spec <- subset(x, abs(x$`400` - x$`420`) < 0.01)
}

### exclude dips in NIR ###
excl.dip.nir.svc <- function (x) {
  sub_spec <- subset(x, x$`800` - x$`770` < 0.02)
}

### exclude smalldips and bumps in NIR ###
excl.minidip.nir.svc <- function (x) {
  sub_spec <- subset(x, abs(x$`780` - x$`760`) < 0.013)
}
  
### exclude spectra shifted up ###
excl.ups.svc <- function (x) {
  sub_spec <- subset(x, x$`670` < 0.15)
}


### exclude dips in SWIR  ###
excl.dip.swir.svc <- function (x) {
  sub_spec <- subset(x, x$`1685` - x$`1660` < 0.0002)
}


### exclude high red ###
excl.hi.red.svc <- function (x) {
  sub_spec <- subset(x, x$`650` - x$`550` < 0)
}

### exclude spectra with max in red not in green
excl.maxvis.shift.svc <- function(x){
  sub_spec <- subset(x,x$`610` - x$`550` < 0)
}


### exclude drop at the end of NIR ###
excl.drop.nir.svc <- function (x) {
  sub_spec <- subset(x, x$`1250` - x$`1300` < 0.03)
}

### exclude jump between VIS NIR sensor ###
excl.visnir.jump.svc <- function (x) {
  sub_spec <- subset(x, abs(x$`1100` - x$`900`) < 0.025)
}


