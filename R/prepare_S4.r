#' Format data taken from S4
#'
#' This function converts passed S4 summarized experiment data to HarmonizR
#' input
#'
#' @param data Data (S4 format) passed by the user. No description file is 
#' needed when using S4 data
#' @return Data and description as data.frames


# ----- format_from_S4(data) --------------------------------------------------
# The function expects S4 input and turns it into HarmonizR input
format_from_S4 <- function(data){
    
    # Get the main data from assay
    data_as_input <- data.frame(SummarizedExperiment::assay(data))
    
    # Get all building blocks for the description file
    ID <- colnames(data_as_input)
    batch <- SummarizedExperiment::colData(data)$Batch
    sample <- seq(1, length(batch))
    
    # Create the descriptive dataframe
    description_as_input <- data.frame(ID, sample, batch)
    
    return(list(data_as_input, description_as_input))
}


#' Format data taken from HarmonizR back to S4
#'
#' This function converts passed HarmonizR output to a S4 summarized experiment
#' data structure
#'
#' @param cured_data The HarmonizR output
#' @param s4_saved The original S4 input
#' @return The HarmonizR output formatted as S4 data


# ----- format_to_s4(cured_data, s4_saved) ------------------------------------
# Reads in HarmonizR output plus the originally passed S4 input and returns the
# HarmonizR output as a S4 summarized experiment
format_to_s4 <- function(cured_data, s4_saved){
    
    # Fetch the row names which we can sort by
    original_assay_rows <- rownames(
        data.frame(SummarizedExperiment::assay(s4_saved)))
    
    # Sort the 'cured_data' HarmonizR output data.frame back to its original
    # order
    cured_s4_assay <- cured_data[original_assay_rows, ]
    
    # Turning it into a matrix and deleting row/column names
    cured_s4_assay <- as.matrix(cured_s4_assay)
    rownames(cured_s4_assay) <- NULL
    colnames(cured_s4_assay) <- NULL
    
    # Overwrite the existing S4 assay with the cured one
    SummarizedExperiment::assay(s4_saved) <- cured_s4_assay
    
    cured_s4 <- s4_saved
    
    return(cured_s4)
}