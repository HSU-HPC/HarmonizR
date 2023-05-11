#' Rebuilding
#'
#' The rebuild function rebuilds the sub-dataframes to one big output
#' data.frame.
#'
#' @param cured_subdfs a list of data.frames, which are the result from
#' splitting().
#' @return The rebuild() function returns the adjusted data.frame and writes
#' out cured_data.tsv


# ----- rebuild(cured_subdf) -------------------------------------
# This function takes the result of the splitting() function and binds all of
# the sub-dataframes together to one big dataframe, which is returned
rebuild <- function(cured_subdfs) {
    prot <- unlist(lapply(cured_subdfs, row.names))
    cured_subdfs <- do.call(plyr::rbind.fill, cured_subdfs)
    row.names(cured_subdfs) <- prot
    
    return(cured_subdfs)
}
