i_is_whole = function(x){
    as.integer(x) == x
}

i_is_index = function(x, max_length, verbose = FALSE){
    if(!verbose){
        w = suppressWarnings(i_is_whole(x))
    } else {
        w = i_is_whole(x)
    }
    p = (x > 0 && x <= max_length)
    all(w) && all(p)
}
