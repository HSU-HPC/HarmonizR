#' Reading description
#'
#' The read_description function reads in a file via its file path and converts
#' it to a for the rest of the workflow readable format.
#'
#' @param description_source Usually the path to the description file. It can
#' also be a correctly formatted data.frame.
#' @return Description as data.frame


# ----- read_description(description_source) ----------------------------------
# The function expects a .csv with the sample name in col.1,
# the sample number in col.2 and the batch-affiliation in col.3
read_description <- function(description_source) {
    batch_data <- utils::read.csv(description_source, sep = ",", header = TRUE)
    
    return(batch_data)
}


#' Fetching batch list
#'
#' The fetch_batch_overview function extracts the overview over the batch
#' distribution in list format.
#'
#' @param batch_data This is a data.frame and simultaneously the result from
#' read_description()
#' @return Batch distribution as list


# ----- fetch_batch_overview(batch_data) --------------------------------------
# Reads the third column of batch_data,
# which holds information about which row of batch_data is in which batch
fetch_batch_overview <- function(batch_data) {
    batch_list <- c(batch_data[3]) # 3 corresponds to the column number
    batch_list <- as.numeric(batch_list[[1]])
    
    return(batch_list)
}
