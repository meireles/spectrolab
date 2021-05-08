#' Read files from various formats into `spectra`
#'
#' @param path Path to directory or input files
#' @param format file formats. "asd" (for ASD); "sig" or "svc" (for SVC);
#'               "sed" or "psr" (for SpecEvo PSR).
#' @param type Data type to read. "target_reflectance", "target_radiance", or
#'             "reference_radiance". Defaults to "target_reflectance".
#' @param recursive read files recursively
#' @param exclude_if_matches excludes files that match this regular expression.
#'                           Example: "BAD"
#' @param ignore_extension boolean. If TRUE, the parser will try to read every
#'                         file in path regardless of the expected extension.
#' @param ... nothing yet
#' @return a single `spectra` or a list of `spectra` (in case files have
#'         incompatible band number or bands values)
#'
#' @author Jose Eduardo Meireles
#' @export
#'
#' @examples
#' library(spectrolab)
#' dir_path = system.file("extdata", "Acer_example", package = "spectrolab")
#'
#' # Relative reflectance is re
#' spec     = read_spectra(path = dir_path, format = "sig")
read_spectra = function(path,
                        format,
                        type               = "target_reflectance",
                        recursive          = FALSE,
                        exclude_if_matches = NULL,
                        ignore_extension   = FALSE,
                        ...) {

    #########################################
    ## match formats
    #########################################
    format_lookup = c(sig = "sig",
                      svc = "sig",
                      sed = "sed",
                      psr = "sed",
                      asd = "asd")

    if(missing(format)){
        stop("please provide the the file format.")
    }

    format_match  = pmatch(tolower(format), names(format_lookup))

    ## Error if format isn't found
    if(length(format_match) == 0){ stop("Format not supported") }

    ## create regexpr to select files
    if(ignore_extension){
        regexpr_ext = ""
    } else {
        regexpr_ext = paste0("\\.", format_lookup[format_match], "$")
    }

    #########################################
    ## validate paths
    #########################################
    i_path   = path
    f_exists = file.exists(i_path)
    is_dir   = dir.exists(i_path)

    ## Error if paths don't exist
    if(!all(f_exists)){ stop("Path not found:", i_path[!f_exists]) }

    ## Error if user mixed file names and dir names
    if(any(is_dir) && any(!is_dir)){ stop("Cannot mix directory and file paths!") }

    #########################################
    ## Read file names
    #########################################
    if( all(is_dir) ){                            ## if dir
        i_path = dir(path        = i_path,
                     pattern     = regexpr_ext,
                     full.names  = TRUE,
                     ignore.case = TRUE)
    } else {                                      ## if files
        m      = grep(regexpr_ext, i_path)
        i_path = i_path[m]
    }

    ## error if no files of the right extension are found
    if(length(i_path) == 0){stop("No file path matched", format_lookup[format_match])}

    #########################################
    ## filter files that match bad spectra
    #########################################
    if(!is.null(exclude_if_matches)){
        bad_tag = exclude_if_matches
        m       = grepl(paste(bad_tag,collapse = "|"), i_path)
        i_path  = i_path[!m]
    }

    ## error if bad spectra fiters out all file names
    if(length(i_path) == 0){
        stop("No paths left after removing bad spectra. Check your `exclude_if_ends_with` param")
    }


    #############################################################
    ## Read spectra with the appropriate function
    #############################################################

    if(format_lookup[format_match] == "sig"){

        if(type == "target_reflectance"){
            refl_cols = 4
        } else if (type == "target_radiance") {
            refl_cols = 3
        } else if (type == "reference_radiance") {
            refl_cols = 2
        } else {
            stop("type must be either target_reflectance, target_radiance or reference_radiance")
        }

        result = i_read_ascii_spectra(i_path,
                                      skip_until_tag    = "data=",
                                      skip_first_n      = NULL,         # 25
                                      sep_char          = "",
                                      header            = FALSE,
                                      wl_col            = 1,
                                      refl_cols         = refl_cols,
                                      divide_refl_by    = 100)
        return(result)
    }

    if(format_lookup[format_match] == "sed"){

        if(type == "target_reflectance"){
            refl_cols      = c("Reflect. %", "Reflect. [1.0]")
            divide_refl_by = c(100, 1)
        } else if (type == "target_radiance") {
            refl_cols      = "Rad. (Target)"
            divide_refl_by = 1
        } else if (type == "reference_radiance") {
            refl_cols      = "Rad. (Ref.)"
            divide_refl_by = 1
        } else {
            stop("type must be either target_reflectance, target_radiance or reference_radiance")
        }

        result = i_read_ascii_spectra(i_path,
                                      skip_until_tag    = "Data:",
                                      skip_first_n      = NULL,        # 26
                                      sep_char          = "\t",
                                      header            = TRUE,
                                      wl_col            = "Wvl",
                                      refl_cols         = refl_cols,
                                      divide_refl_by    = divide_refl_by,
                                      check.names       = FALSE)
        return(result)
    }

    if(format_lookup[format_match] == "asd"){
        result = i_read_asd_spectra(i_path,
                                    type              = type,
                                    format            = "binary",
                                    divide_refl_by    = 1,
                                    ...)
        return(result)
    }
}


#' Internal parser for ASCII format
#'
#' Generic parser for SVC's `.sig` and PSR's `.sed`
#'
#' @param file_paths paths for files already parsed by `read_spectra`
#' @param skip_until_tag Tag that precedes the table of value data,
#'                       indicating that lines until that tag should be skipped.
#'                       Tag is matched with a regexpr.
#' @param skip_first_n skip the first n lines. Only used if `skip_until_tag` is
#'                     not given
#' @param sep_char separator
#' @param header boolean. keep header?
#' @param wl_col idx or name of band column
#' @param refl_cols idx or name of value columns. MULTIPLE
#' @param divide_refl_by divide values by this. MULTIPLE
#' @param ... additional arguments passed to read table
#' @return single `spectra` or list of `spectra`
#'
#' @importFrom utils read.delim
#'
#' @keywords internal
#' @author Jose Eduardo Meireles and Anna Schweiger
i_read_ascii_spectra = function(file_paths,
                                skip_until_tag = NULL,
                                skip_first_n   = NULL,
                                sep_char,
                                header,
                                wl_col,
                                refl_cols,
                                divide_refl_by,
                                ...){

    ############################################################
    ## Internal function to read table
    ############################################################

    parse_skip_n   = function(x, skip = skip_first_n) {
        utils::read.delim(x, skip = skip, sep = sep_char, header = header, check.names = FALSE)
    }

    parse_skip_tag = function(x, tag = skip_until_tag) {
        max_l = 100
        skip  = grep(tag, trimws(readLines(x, n = max_l)))

        if(length(skip) == 1){
            return(utils::read.delim(x, skip = skip, sep = sep_char, header = header, check.names = FALSE))
        } else if (length(skip) == 0){
            stop(paste0("No match found for skip_until_tag in the first ", max_l, " lines"))
        } else {
            stop(paste0("More than one match found for skip_until_tag in the first ", max_l, " lines"))
        }
    }

    if( ! is.null(parse_skip_tag) ){
        parse = parse_skip_tag
    } else if ( ! is.null(skip_first_n) ){
        parse = parse_skip_n
    } else {
        stop("Must give a skip_until_tag or skip_first_n args")
    }

    ############################################################
    ## Requirements and param checks


    if(length(refl_cols) > 1 && any(i_is_whole(refl_cols)) ){
        stop("refl_cols cannot be a vector of indices.")
    }

    ## Deal with cases where multiple value columns or multiple value
    ## scalars (divide_refl_by) are given
    if(length(refl_cols) < length(divide_refl_by)) {
        warning("Length of divide_refl_by should be either 1 or equals to the length of refl_cols. divide_refl_by has been prunned to length", length(refl_cols), ".")
        divide_refl_by = rep(divide_refl_by, length.out = length(refl_cols))
    }

    ############################################################
    ## Parse data
    ############################################################
    data        = lapply(file_paths, parse)
    names(data) = file_paths

    ############################################################
    ## Choose right value columns from parsed data
    ## Updates refl_cols to an INDEX
    if(length(refl_cols) > 1) {

        d = data[[1]]                                 ## OK to use the first file as representative of the whole thing?
        m = colnames(d) %in% refl_cols
        n = which(refl_cols %in% colnames(d))

        if(all( !m )){
            stop("refl_cols did not match any columns.")
        }

        if( sum(m) > 1 ){
            stop("refl_cols matched more than one column.")
        }

        # Update refl cols and divide by
        # subset 1st as a safeguard in case m matches more than one column
        refl_cols      = which(m)
        divide_refl_by = divide_refl_by[n]
    }


    ## there mabye files with different number of bands or band values
    ## check for them and split the data if needed
    wl_factor = unlist(
        lapply(data, function(x){
            paste0(x[ , wl_col], collapse = "")
        })
    )

    data = unname(split(data, wl_factor))

    ## Construct spectra
    spec = lapply(data, function(x) {
        rf = lapply(x, function(y){ y[ , refl_cols ] })
        rf = do.call(rbind, rf)
        rf = rf / divide_refl_by
        wl = x[[1]][ , wl_col ]
        nm = basename(names(x))

        spectra(rf, wl, nm)
    })

    if(length(spec) > 1){
        warning("Returning a list of `spectra` because some files had different number of bands or band values. If you want to make those data compatible, consider resampling (with resample) and then combining them (with combine)")
        return(spec)
    } else {
        return(spec[[1]])
    }
}


#' Parser for ASD's `.asd`
#'
#' @param file_paths paths for files already parsed by `read_spectra`
#' @param type Data type to read. "target_refl", "target_rad", "reference_rad".
#'             Defaults to "target_refl".
#' @param format choice of ASD format. Either "binary" or "txt"
#' @param divide_refl_by divide values by this
#' @param ... NOT USED YET
#' @return spectra object
#'
#' @importFrom prospectr readASD
#'
#' @keywords internal
#' @author Jose Eduardo Meireles
i_read_asd_spectra = function(file_paths,
                              type   = "target_refl",
                              format = c("binary", "txt"),
                              divide_refl_by,
                              ...){

    if(type == "target_reflectance"){
        rf = prospectr::readASD(fnames = file_paths, out_format = "matrix")
        wl = colnames(rf)
        nm = gsub(".asd$", "",rownames(rf))

        return(spectra(rf, wl, nm))
    } else if (type == "target_radiance"){
        l   = prospectr::readASD(fnames = file_paths, out_format = "list")
        rf  = do.call(rbind, lapply(l, `[[`, "radiance"))
        nm  = gsub(".asd$", "",rownames(rf))
        wl  = l[[1]][["band"]]

        return(spectra(rf, wl, nm))

    } else if (type == "reference_radiance"){
        l   = prospectr::readASD(fnames = file_paths, out_format = "list")
        rf  = do.call(rbind, lapply(l, `[[`, "reference"))
        nm  = gsub(".asd$", "",rownames(rf))
        wl  = l[[1]][["band"]]

        return(spectra(rf, wl, nm))
    } else {
        stop("type must be either target_reflectance, target_radiance or reference_radiance")
    }
}

