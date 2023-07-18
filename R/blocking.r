#' Blocking
#'
#' This function performs blocking on the given description and therefore 
#' influences how the dataset will be split later down the pipeline.
#'
#' @param bacth_list The list with information about batch-affiliations for 
#' every sample.
#' @param block The blocking parameter (how many batches should always get
#' blocked together).
#' @return Returns an updated 'batch_list' with blocking included.
#' @export


# ----- BLOCKING --------------------------------------------------------------
# Contents: blocking()


# ----- blocking(batch_list, block) -------------------------------------------
# This function creates the later needed information regarding how to split
blocking <- function(batch_list, block){
  # Initializing resulting vector and counters
  building_block_list <- c()
  counter_big_block <- 1
  current_blocking_entry <- 1
  # Looping over all entries of "batch_list"
  for (i in 1:length(batch_list)){
    
    building_block_list[i] <- current_blocking_entry
    
    # Check if upcoming entry in "batch_list" is a new batch or the end
    if (is.na(batch_list[i + 1]) || batch_list[i] != batch_list[i+1]){
      
      counter_big_block <- counter_big_block + 1
    
      if (counter_big_block > block) {
        
        counter_big_block <- 1
        current_blocking_entry <- current_blocking_entry + 1
        
      }
    }
  }
  return(building_block_list)
}