#' Jaccard index on ones (existence)
#'
#' Calculates the Jaccard index for two given lists a and b based on common ones
#'
#' @param a First list with either 0 or 1 entries to be compared against the
#' second list.
#' @param b Second list with either 0 or 1 entries to be compared against the
#' first list.
#' @return The Jaccard similarity based on existing values


# ----- jaccard_index_existence(a, b) -----------------------------------------
# Calculates the Jaccard index for two given lists a and b based on common ones
jaccard_index_existence <- function(a, b) {
    sim <- 0
    diff <- 0
    for (current_position in seq_len(length(a))){
        if (a[current_position] == 1 && b[current_position] == 1){
            sim <- sim + 1
        }
        if (a[current_position] == 1 && b[current_position] == 0){
            diff <- diff + 1
        }
        if (a[current_position] == 0 && b[current_position] == 1){
            diff <- diff + 1
        }
    }
    jaccard_similarity <- sim/(sim+diff)
    return(jaccard_similarity)
}


#' Jaccard index on zeroes (absence)
#'
#' Calculates the Jaccard index for two given lists a and b based on common
#' zeroes
#'
#' @param a First list with either 0 or 1 entries to be compared against the
#' second list.
#' @param b Second list with either 0 or 1 entries to be compared against the
#' first list.
#' @return The Jaccard similarity based on absent values


# ----- jaccard_index_absence(a, b) -------------------------------------------
# Calculates the Jaccard index for two given lists a and b based on common 
# zeroes
jaccard_index_absence <- function(a, b) {
    sim <- 0
    diff <- 0
    for (current_position in seq_len(length(a))){
        # The only change to "jaccard_index_existence" is found in the following
        # if-statement
        if (a[current_position] == 0 && b[current_position] == 0){
            sim <- sim + 1
        }
        if (a[current_position] == 1 && b[current_position] == 0){
            diff <- diff + 1
        }
        if (a[current_position] == 0 && b[current_position] == 1){
            diff <- diff + 1
        }
    }
    jaccard_similarity <- sim/(sim+diff)
    return(jaccard_similarity)
}


#' Jaccard-based sorting
#'
#' Calculates a order to sort by based on the Jaccard similarity of all given
#' batches
#'
#' @param binary_df The input matrix passed by the user reduced to presence and
#' absence of features in batches (binary)
#' @return A template for batch-sorting based on Jaccard similarity


# ----- jaccard(binary_df) ----------------------------------------------------
# Calculates a order to sort by based on the Jaccard similarity of all given
# batches
jaccard <- function(binary_df){
    
    binary_df <- as.data.frame(binary_df)
    
    # Three lists to append to
    combo_first <- list()
    combo_second <- list()
    all_jaccards <- list()
    
    i_counter <- 1
    j_counter <- 2
    where <- 1
    
    for (i in seq_len((ncol(binary_df) - 1))) {
        where <- where + 1
        j_counter <- where
        for (j in where:ncol(binary_df)){
            jaccard_result_existence <- jaccard_index_existence(
                unlist(binary_df[i]), 
                unlist(binary_df[j]))

            # Append to 3 lists
            combo_first <- append(combo_first, i_counter)
            combo_second <- append(combo_second, j_counter)
            all_jaccards <- append(all_jaccards, jaccard_result_existence)
            
            j_counter <- j_counter + 1
        }
        i_counter <- i_counter + 1
    }
    
    # Turn lists in correct shape
    combo_first <- unlist(combo_first)
    combo_second <- unlist(combo_second)
    all_jaccards <- unlist(all_jaccards)
    
    # Build data.frame to extract the sort-by list ("order_to_go_by")
    jac_overview_df <- data.frame(
        first = combo_first, 
        second = combo_second, 
        jaccard = all_jaccards)
    # Order the dataframe by jaccard (descending) 
    jac_overview_df <- jac_overview_df[order(-jac_overview_df$jaccard),]
    #print(jac_overview_df)
    
    # Logic to create "order_to_go_by"
    order_to_go_by <- list()
    for (current_row in seq_len(nrow(jac_overview_df))){
        #print(jac_overview_df$jaccard[current_row])
        if ((jac_overview_df$first[current_row] %in% order_to_go_by) == FALSE &&
            (jac_overview_df$second[current_row] %in% order_to_go_by) == FALSE){
            order_to_go_by <- append(
                order_to_go_by, 
                jac_overview_df$first[current_row])
            order_to_go_by <- append(
                order_to_go_by, 
                jac_overview_df$second[current_row])
        }
    }
    
    # At this point, the "order_to_go_by"-list should be complete for even batch
    # amounts. In case the batch amount is uneven, one batch will end up being
    # not in the list. The following logic appends that very batch to the end
    # of the "order_to_go_by"-list
    if (ncol(binary_df) != length(unlist(order_to_go_by))){
        all_batches <- seq(1, ncol(binary_df))
        for (element in all_batches){
            if ((element %in% order_to_go_by) == FALSE){
                order_to_go_by <- append(order_to_go_by, element)
            }
        } 
    }
    
    order_to_go_by <- unlist(order_to_go_by)
    
    # Return "order_to_go_by", which holds the information to sort by
    return(order_to_go_by)
}
