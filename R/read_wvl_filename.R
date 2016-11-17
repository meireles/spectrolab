#' Read SVC filenames and wavelengths.
#'
#' Function to read filenames and wavelengths from .sig files
#'
#' @param inputdir Input directory containing .sig files.
#' Higher level directories are supported.
#' @param inclbad Logical. Should files flagged "bad" be included?
#' @param inclwr Logical. Should white reference files, flagged
#' "WR" be included?
#' @return The output is a dataframe.
#' @examples
#' \dontrun{
#' read_filenames("path_to_your_folder")
#' read_wvl("path_to_your_folder")
#' }
#' @name filenames_wvl
NULL

## NULL

###### 10-18-2016 ##########################
##### READ FILENAMES ######
#' @rdname filenames_wvl

read_filenames <- function (inputdir, inclbad=FALSE, inclwr=FALSE) {
  if (inclbad==TRUE & inclwr==TRUE) {
    files <- dir(inputdir, recursive=TRUE, full.names=FALSE, pattern="\\.sig$")
  } else {
    temp <- dir(inputdir, recursive=TRUE, full.names=FALSE, pattern="\\.sig$")
    files <- temp[!grepl("BAD|WR", temp)]
  }
  if (inclbad==TRUE & inclwr==FALSE) {
    temp <- dir(inputdir, recursive=TRUE, full.names=FALSE, pattern="\\.sig$")
    files <- temp[!grepl("WR", temp)]
  }
  if (inclwr==TRUE & inclbad ==FALSE) {
    temp <- dir(inputdir, recursive=TRUE, full.names=FALSE, pattern="\\.sig$")
    files <- temp[!grepl("BAD", temp)]
  }
  return(files)
}



#### READ WAVELENGTHS ###
#' @rdname filenames_wvl

read_wvl <- function (inputdir) {
  indir <- dir(inputdir, recursive=TRUE, full.names=TRUE, pattern="\\.sig$")[1]
  wvl <- read.delim(indir, sep=" ",header=F, skip=25)[,1]
  return(wvl)
}


