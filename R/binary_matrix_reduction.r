#' Creating a binary existence matrix
#'
#' This function reduces its input matrix to a binary existence matrix based on
#' the given description file (and information on how many values a batch needs)
#' for proper adjustment.
#'
#' @param binary_data The input data.frame that should become binary.
#' @param batch_list Information about the sample's batch affiliations.
#' @param needed_values Information, how many values are needed to render a
#' a batch 'valid'.
#' @return A binary existence matrix returned as a data.frame


# ----- binary_matrix_reduction(binary_data, batch_list, needed_values) -------
# This function expects a binary matrix itself as input. This usually 
# means that all values are turned to 1 and all NAs are turned to 0. 
# This function returns a binary data.frame reduced to information for full 
# batches. The function is similar but not identical to what is found in 
# 'spotting_missing_values.r'.
binary_matrix_reduction <- function(binary_data, batch_list, needed_values) {
    # The initialization
    too_many_zeroes <- FALSE
    value_existence_counter <- 0
    temp_vec <- vector()
    binary_information_list <- list()
    
    # Iterate through the entire dataset "binary_data"
    for (i in seq_len(nrow(binary_data))) {
        for (j in seq_len(ncol(binary_data))) {
            # If a value is found, increase "value_existence_counter"
            if (binary_data[i, j] == 1) {
                value_existence_counter <- value_existence_counter + 1
            }
            # The following if-statement means:
            # The next iteration will look into a new batch
            if (is.na(batch_list[j + 1]) || 
                batch_list[j + 1] != batch_list[j]) {
                # Needed_values is either 2 or 1 based on the chosen ComBat_mode
                if (value_existence_counter < needed_values) {
                    # Too many zeroes within this batch, marking it as invalid
                    too_many_zeroes <- TRUE
                }
                # We found a batch with enough ones
                if (too_many_zeroes != TRUE) {
                    # Now we need to note down a 1 for the entire batch
                    temp_vec <- append(temp_vec, 1)
                }
                # We found a batch with too few ones
                if (too_many_zeroes == TRUE) {
                    # Now we need to note down a 0 for the entire batch
                    temp_vec <- append(temp_vec, 0)
                }        
                # Reset "too_many_zeroes" to FALSE
                # and "value_existence_counter" to 0
                too_many_zeroes <- FALSE
                value_existence_counter <- 0
            }
        }
        # Append the vector to "binary_information_list"
        binary_information_list[[i]] <- temp_vec
        # Reset "temp_vec"
        temp_vec <- vector()
    }
    
    # A list of vectors ("binary_information_list"). Each vector in the list is 
    # correspondent to a row in the input binary matrix. A data.frame gets 
    # created based on this list (which then will be the return value)
    binary_df <- do.call(rbind, binary_information_list)
    return(binary_df)
}
