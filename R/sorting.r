#' Creation of keys
#'
#' Calculates a list of usable keys based on the passed batch listings
#'
#' @param batch_list The list with information about batch-affiliations for 
#' every sample.
#' @return A list element with usable keys


# ----- build_key_list(batch_list) --------------------------------------------
# Calculates a list of usable keys based on the passed batch listings
build_key_list <- function(batch_list){
    
    sub_list <- c()
    sub_list_counter <- 1
    key_list <- list()
    key_list_counter <- 1
    
    for(i in seq_len(length(batch_list))){
        
        sub_list[sub_list_counter] <- i
        sub_list_counter <- sub_list_counter + 1
        
        if(batch_list[i+1] != batch_list[i] || is.na(batch_list[i+1])){
            
            key_list[[key_list_counter]] <- sub_list
            sub_list <- c()
            sub_list_counter <- 1
            key_list_counter <- key_list_counter + 1
            
        }
        
    }
    
    return(key_list)
}


#' Finding NAs for the sorting process
#'
#' Creates an overview of NAs based on both the passed input data.frame and 
#' the batch list
#' @param df The data.frame passed initially by the user.
#' @param batch_list The list with information about batch-affiliations for 
#' every sample.
#' @return An overview of the NA-distribution


# ----- find_na(df, batch_list) -----------------------------------------------
# Finds NA
find_na <- function(df, batch_list){
    
    na_overview <- c()
    na_overview_counter <- 1
    na_sum <- 0
    
    # looping over all columns
    for(i in seq_len(ncol(df))){
        
        # column na stores the amount of missing values in the current column
        column_na <- sum(is.na(df[ , i]))
        na_sum <- na_sum + column_na
        
        # this if statement is TRUE whenever "i" is the last column of a batch
        if(batch_list[i+1] != batch_list[i] || is.na(batch_list[i+1])){
            
            na_overview[na_overview_counter] <- na_sum
            na_overview_counter <- na_overview_counter + 1
            
            # reset na_sum since we finished the current batch
            na_sum <- 0
            
        }
        
    }
    
    return(na_overview)
}


#' Sorting the input data.frame
#'
#' Creates an overview of NAs based on both the passed input data.frame and 
#' the batch list
#' @param df The data.frame passed initially by the user.
#' @param batch_list The list with information about batch-affiliations for 
#' every sample.
#' @param batch_data The full data.frame passed as description by the user.
#' @param order_to_go_by The template to sort by.
#' @param verbosity Toggles the amount of information printed out by 
#' the HarmonizR algorithm during execution. Passed on from the main function.
#' @return Correctly sorted data and description as two elements of a list


# ----- sorting(df, batch_list, batch_data, order_to_go_by, verbosity) --------
# For this function we need main_data (as df) and batch_list (as batch_list) 
# from parsing as input
sorting <- function(df, batch_list, batch_data, order_to_go_by, verbosity){
    
    # Make sure that colnames are set correctly in batch_data
    colnames(batch_data) <- c("ID", "sample", "batch")
    
    # "key_list"
    key_list <- build_key_list(batch_list)
    
    # In the following, the sorting takes place.
    # The type of "order_to_go_by" should either be integer or double
    # (for seriation/jaccard) or logical i.e. FALSE (for sparsity)
    if (is.integer(order_to_go_by) || is.double(order_to_go_by)){
        
        # Print to console
        if (verbosity >= 3){
            message("New batch order after sorting:")
            message(order_to_go_by)
        }
        
        # Reorder "key_list" (saved in "updated_key_list", same structure)
        updated_key_list <- list()
        append_counter <- 1
        for (element in order_to_go_by){
            updated_key_list[[append_counter]] <- key_list[[element]]
            append_counter <- append_counter + 1
        }
        
        target <- unlist(updated_key_list)
    } else {
        na_overview <- find_na(df, batch_list)
        order_for_printing <- seq(1, length(na_overview))
        order_for_printing <- order_for_printing[order(
            na_overview, 
            decreasing=FALSE)]
        
        # Print to console
        if (verbosity >= 3){
            message("New batch order after sorting:")
            message(order_for_printing)
        }
        
        target <- unlist(key_list[order(na_overview, decreasing=FALSE)])
    }
    
    # Print to console
    if (verbosity >= 3){
        message("New order of samples after sorting:")
        message(target)
    }
    
    # Restructure data.frame
    df <- df[ , target]
    
    # Reorder the description
    batch_data <- batch_data[match(target, batch_data$sample),]
    
    new_order <- list()
    for (i in seq_len(nrow(batch_data[3]))) {
        new_order[[i]] <- batch_data[i,3]
    }
    
    new_desc_after <- list()
    order_count <- 1
    standard_count <- 1
    old_value <- new_order[1]
    
    for (value in new_order) {
        if (value != old_value) {
            order_count <- order_count + 1
        }
        new_desc_after[[standard_count]] <- order_count
        standard_count <- standard_count + 1
        old_value <- value
    }
    
    # Assign new numbers to 'batch' column
    batch_data$batch <- new_desc_after
    
    return(list(df, batch_data))
}