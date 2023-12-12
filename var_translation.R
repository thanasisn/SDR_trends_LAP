
#### dictionary ####
dict <- list(DIR_att    = 'Dir. Beam Irrad.',
             HOR_att    = 'Dir. Irrad. horizontal plane',
             DIR_transp = 'Transp. for Dir. Irrad.',
             wattGLB    = "Global Radiation raw",
             GLB_att    = "SDR",
             tsi1au_att = "TSI at 1au",
             ALL        = "All sky cond.",
             CLEAR        = "Clear sky cond.",
             near_tcc_att = "tcc nearest",
             near_tcc_des = "tcc nearest deseas",
             CLOUD      = "Cloudy sky cond.")
## function to translate objects names
# translate <- function(...) as.vector(unlist(dict[c(...) == names(dict)]))

translate <- function(x) {
    res <- c()
    for (ax in x) {
        ## get match
        amatch <- as.vector(unlist(
            dict[stringr::str_detect(ax, names(dict))]
        ))
        ## return same if not found
        if (is.null(amatch)) {
            amatch <- ax
        }
        ## gather results
        res <- c(res, amatch)
    }
    return(res)
}


