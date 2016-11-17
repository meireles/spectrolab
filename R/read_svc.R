#' Read SVC files
#'
#' Function to read .sig files and combine them in a data.frame
#'
#'
#' @param inputdir Input directory containing .sig files.
#' Higher level directories are supported.
#' @param inclbad Logical. Should files flagged "bad" be included?
#' @param inclwr Logical. Should white reference files, flagged
#' "WR" be included?
#' @param read Logical. If TRUE files are read and combined in a data.frame.
#' If FALSE the filenames are returned.
#' @param filename Logical. Should the filename be included as the first column?
#' @return The output is a dataframe.
#' @examples
#' \dontrun{
#' read_spec("path_to_your_folder")
#' }
#' @export

###### 10-18-2016 ##########################
##### READ SVC DATA ############### only 2 cases implemented: GOOD, WR&BAD

read_spec <- function (inputdir, inclbad=FALSE, inclwr=FALSE, read=FALSE,
                      filename=TRUE) {
  if (inclbad & inclwr) {
    files <- dir(inputdir, recursive=TRUE, full.names=TRUE, pattern="\\.sig$")
  } else {
    temp <- dir(inputdir, recursive=TRUE, full.names=TRUE, pattern="\\.sig$")
    files <- temp[!grepl("BAD|WR", temp)]
  }
  if (inclbad) {
    temp <- dir(inputdir, recursive=TRUE, full.names=TRUE, pattern="\\.sig$")
    files <- temp[!grepl("WR", temp)]
  }
  if (inclwr) {
    temp <- dir(inputdir, recursive=TRUE, full.names=TRUE, pattern="\\.sig$")
    files <- temp[!grepl("BAD", temp)]
  }
  if (read==TRUE & inclbad==TRUE & inclwr==TRUE) {
    temp <- dir(inputdir, recursive=TRUE, full.names=TRUE, pattern="\\.sig$")
    files <- as.data.frame(t(do.call (cbind, lapply (temp, function (x) read.delim (x, sep=" ", skip=25, header=F)
                                                     [,c(7)]))))
    files <- files/100
    wave <- read_wvl(inputdir)
    names(files) <- wave
    if(filename==TRUE){
      ID <- read_filenames(inputdir, inclbad = T, inclwr = T)
      files <- cbind(ID, files)
    }
  } else if (read==TRUE) {
    temp <- dir(inputdir, recursive=TRUE, full.names=TRUE, pattern="\\.sig$")
    temp <- temp[!grepl("BAD|WR", temp)]
    files <- as.data.frame(t(do.call (cbind, lapply (temp, function (x) read.delim (x, sep=" ", skip=25, header=F)
                                                     [,c(7)]))))
    files <- files/100
    wave <- read_wvl(inputdir)
    names(files) <- wave
    if(filename==TRUE){
      ID <- read_filenames(inputdir)
      files <- cbind(ID, files)
    }
  }
  if (read==TRUE & inclbad==TRUE & inclwr==FALSE) {
    temp <- dir(inputdir, recursive=TRUE, full.names=TRUE, pattern="\\.sig$")
    temp <- temp[!grepl("WR", temp)]
    files <- as.data.frame(t(do.call (cbind, lapply (temp, function (x) read.delim (x, sep=" ", skip=25, header=F)
                                                     [,c(7)]))))
    files <- files/100
    wave <- read_wvl(inputdir)
    names(files) <- wave
    if(filename==TRUE){
      ID <- read_filenames(inputdir, inclbad = T)
      files <- cbind(ID, files)
    }
  }
  if (read==TRUE & inclbad==FALSE & inclwr==TRUE) {
    temp <- dir(inputdir, recursive=TRUE, full.names=TRUE, pattern="\\.sig$")
    temp <- temp[!grepl("BAD", temp)]
    files <- as.data.frame(t(do.call (cbind, lapply (temp, function (x) read.delim (x, sep=" ", skip=25, header=F)
                                                     [,c(7)]))))
    files <- files/100
    wave <- read_wvl(inputdir)
    names(files) <- wave
    if(filename==TRUE){
      ID <- read_filenames(inputdir, inclwr = T)
      files <- cbind(ID, files)
    }
  }
  return(files)
}



