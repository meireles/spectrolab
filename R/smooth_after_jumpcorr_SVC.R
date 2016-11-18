# #######################################################################
# ###### Optional smoothing after jump correction SVC data ###############
# #######################################################################
# ###### 10-20-2016 ####################################################
#
# #### VIS / NIR #################################
# #### moving average between 970 - 1025 nm #####
#
# smoo.visnir.svc <- function(x){
#   library(prospectr)
#   subs <- subset(x, select = which(colnames(x)=="970"):which(colnames(x)=="1025"))
#   mov <- as.data.frame(movav(subs, w=11))
#   x [,which(colnames(x)==names(mov)[1]):
#        which(colnames(x)==names(mov)[length(mov)])]<- mov
#   return(x)
# }
#
# #### NIR / SWIR ######################################
# #### linear interpolation between 1903 - 1917 nm #####
#
# smoo.nirswir.svc <- function(x){
#   library(prospectr)
#   subs <- subset(x, select = which(colnames(x)=="1903"):which(colnames(x)=="1917"))
#   newwav <- c(as.numeric(names(subs)[1]),as.numeric(names(subs)[length(subs)]))
#   xx <- as.data.frame(resample(subs,as.numeric(names(subs)), newwav,
#                                interpol="linear"))
#   reswav <- seq(as.numeric(names(xx)[1]),as.numeric(names(xx)[length(xx)]), 1)
#   xx <- as.data.frame(resample(xx,newwav, reswav))
#   x [,which(colnames(x)==names(xx)[1]):
#        which(colnames(x)==names(xx)[length(xx)])] <- xx
#   return(x)
# }
