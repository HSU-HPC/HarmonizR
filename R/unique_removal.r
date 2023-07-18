# DOCUMENT!
unique_removal <- function(affiliation_list){
  # Create "new_affiliation_list" which will contain no unique entries (except 
  # single-batch-proteins and empty proteins)
  # Initialized with the original affiliation_list
  new_affiliation_list <- affiliation_list
  
  # Initialize a list of indexes, that have already been matched to
  already_matched_to_these <- list()
  unique_ind <- which(!(duplicated(affiliation_list)|duplicated(affiliation_list, fromLast=TRUE)))
  
  for (element in unique_ind){
    
    # If the unique combination we look at next has already been matched to in 
    # the past, it is no longer unique and thus has not to be changed at all
    no_need_to_check_this_one <- FALSE
    if (element %in% already_matched_to_these){
      no_need_to_check_this_one <- TRUE
    }
    
    # Make sure to only look at unique combinations with vecs of length > 2
    # and only look at those that no other unique has been matched to already
    if (length(unlist(new_affiliation_list[element])) > 1 
        && no_need_to_check_this_one == FALSE){
      # Initialize
      current_best_subvector <- vector()
      
      # The following variable is used to find the index of the found best
      # subvector
      current_entry_index <- 1
      
      # new_affiliation_list is used right of the bat to always work on the 
      # updated list in each iteration
      for (entry in new_affiliation_list){
        # Check all entries of the (always updated) new_affiliation_list to
        # find a vector that is
        # a) a subset of the unique combination we currently look at in the 
        #    outer loop
        # b) shorter than the mentioned unique combination to prevent it finding
        #    itself
        # c) better than any found subset before it
        # If all three conditions are met, mark it as 'current_best_subvector'.
        # If this statement is never TRUE, then 'current_best_subvector' will
        # simply stay empty
        if (all(entry %in% unlist(new_affiliation_list[element])) 
            && length(entry) < length(unlist(new_affiliation_list[element])) 
            && length(entry) > length(current_best_subvector)){
          current_best_subvector <- entry
          current_best_subvector_index <- current_entry_index
        }
        current_entry_index <- current_entry_index + 1
      }
      
      if (length(current_best_subvector) > 1){
        # Remember all indices that something has been matched to
        already_matched_to_these <- append(already_matched_to_these, current_best_subvector_index)
      }
        
      # Now we update new_affiliation_list on the fly
      new_affiliation_list[[element]] <- current_best_subvector
    }
  }
  return(new_affiliation_list)
}