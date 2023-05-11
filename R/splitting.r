#' Splitting
#'
#' This function splits the data.frame. The data is very sensitive to its
#' specific input. Only to be called via harmonizR()
#'
#' @param affiliation_list An overview of which protein has which missing value
#' distribution.
#' @param main_data This is the input data.frame read in by the HarmonizR.
#' @param batch_data This is the description data.frame read in by the
#' HarmonizR.
#' @param block_list An overview of the batch groupings in list form. If the 
#' block parameter was used, the groupings are changed accordingly.
#' @param algorithm Either "ComBat" or "limma". Based on the selected algorithm
#' for the harmonizR() function.
#' @param ComBat_mode The chosen ComBat mode influences the parameters the
#' ComBat algorithm is using. Based on the ComBat_mode parameter given to the
#' harmonizR() function. Not active during limma execution.
#' @param block The block parameter is here used to determine whether there are
#' single-batch dataframes at all present.
#' @param verbosity Toggles the amount of stuff printed out by the HarmonizR 
#' algorithm during execution.
#' @param cores Manually sets the number of cores the user wants to be used 
#' during HarmonizR's execution. A positive integer.
#' @return Returns a list of 'chopped up' data.frames


# -- splitting(affiliation_list, main_data, batch_data, batch_list, ComBat_mode)
# The first part of the function function splits the dataframe (main_data)
# based on 'affiliation_list' and stores them in corresponding variables
# in the second part of the function, the ComBat algorithm is called for all
# suitable sub-dataframes, which are afterwards combined and written into a
# .csv file, yielding the result and terminating the HarmonizR algorithm
splitting <- function(
        affiliation_list,
        main_data,
        batch_data,
        block_list,
        algorithm,
        ComBat_mode,
        block,
        verbosity,
        cores
) {
    
    # ----- Configuring data --------------------------------------------------
    
    # 'integer_affil' is build for faster comparisons within the following loop
    # and is only used to get the correct row indexes in a way shorter time
    integer_affil <- as.integer(
        factor(affiliation_list, levels = unique(affiliation_list)))
    
    # Create a new, sorted integer_affil
    sorted_integer_affil <- sort(integer_affil)
    
    # Add new column to main_data (to sort by)
    main_data["temporary_affiliation_list"] <- integer_affil
    
    # Order main_data based on integer_affil
    ordered_main_data <- main_data[order(main_data$temporary_affiliation_list),]
    # Drop the last column
    ordered_main_data <- ordered_main_data[seq_len(length(ordered_main_data)-1)]
    
    # List to iterate over with foreach. 
    indices_to_care_for <- c(1,1+which(diff(sorted_integer_affil) != 0))
    
    
    # ----- Parallel dissection into sub-dataframes ---------------------------
    
    # Print to console
    if (verbosity >= 4) {
        message("Dissection starting time (parallel): ", Sys.time())
    }
    
    numCores <- parallel::detectCores()
    
    # This line checks, if the user wants a certain amount of cores
    if (cores != FALSE && cores <= numCores) {
        numCores <- cores
    }
    
    # Here, parallelization is activated and set to 'numCores'
    doParallel::registerDoParallel(cores = numCores)
    
    `%dopar%` <- foreach::`%dopar%`
    `%do%` <- foreach::`%do%`
    
    current_ind <- 0
    
    # Parallel: For every index in the indices_to_care_for-list
    created_subdfs <- foreach::foreach(
        current_ind = indices_to_care_for, .combine = c) %dopar% {
        
        # This line should give us all indices
        row_indices_to_use <- which(
            sorted_integer_affil %in% sorted_integer_affil[current_ind])
        # These lines should give us all indices for the columns we need to use
        vec_in_affil <- unlist(affiliation_list[match(
            sorted_integer_affil[current_ind], integer_affil)])
        col_indices_to_use <- which(block_list %in% vec_in_affil)
        
        # Here we should now have a ready sub-dataframe
        some_subdf <- ordered_main_data[row_indices_to_use, col_indices_to_use]
        
        return(list(some_subdf))
        
    }
    
    doParallel::stopImplicitCluster()
    
    # Print to console
    if (verbosity >= 4) {
        message("Dissection finishing time (parallel): ", Sys.time())
    }
    
    # Print to console
    if (verbosity >= 5) {
        message(
            "Amount of sub-dataframes to-be-adjusted: ",
            length(created_subdfs) - 1
            )
        for (current_subdf in created_subdfs) {
            if (ncol(current_subdf) == 0) {
                message(
                    "Features within the empty sub-dataframe (discraded): ",
                    nrow(current_subdf)
                    )
            }
        }
    }
    
    
    # ----- Parallel adjustment -----------------------------------------------
    
    # Print to console
    if (verbosity >= 4) {
        message("Adjustment starting time (parallel): ", Sys.time())
    }
    
    # This line checks how many cores are available and saves them in 'numCores'
    numCores <- parallel::detectCores()
    
    # This line checks, if the user wants a certain amount of cores
    if (cores != FALSE && cores <= numCores) {
        numCores <- cores
    }
    
    # Here, parallelization is activated and set to 'numCores'
    doParallel::registerDoParallel(cores = numCores)
    
    `%dopar%` <- foreach::`%dopar%`
    `%do%` <- foreach::`%do%`
    
    # In this parallelized loop the ComBat algorithm is used for batch
    # adjustment
    cured_subdfs <- foreach::foreach(
        current_subdf = created_subdfs, .combine = c) %dopar% {
        
        description <- data.frame()
        
        # This if-statement is important, since the empty sub-dataframe would
        # crash the loop since no description can by build for a dataframe with
        # 0 columns
        if (ncol(current_subdf) > 0) {
            # The description for each sub-dataframe is fetched in this loop
            for (row in seq_len(nrow(batch_data))) {
                # The respective description file is build with the current
                # sub-dataframe (current_subdf) as a template
                if (batch_data[row, 1] %in% names(current_subdf) == TRUE) {
                    description <- rbind(description, batch_data[row, ])
                }
            }
            # The sub-dataframe and the description are changed into type double
            # in order to feed the ComBat algorithm properly
            # it is now type 'double'
            ComBat_input_batch <- c(description[3])
            ComBat_input_batch <- as.numeric(ComBat_input_batch[[1]])
        } else {
            # The following line only executes, when we look at the empty 
            # sub-dataframe and the variable will be set to be of length = 0
            ComBat_input_batch <- list()
        }
        
        # it is now type 'double'
        ComBat_input_dat <- as.matrix(current_subdf)
        
        # If there are ERRORS in the future with ComBat/limma: The inclusion of
        # one-batch subdfs is likely the culprit. In that case: DO NOT fusion 
        # 'two_or_more_subdf' and 'one_subdf' beforehand!
        # This if-statement applies whenever we insert a one_batch_subdf into
        # the system. This may happen during blocking when there is a leftover
        # batch in a scenario where there is an uneven amount of batches to
        # begin with
        if (length(unique(ComBat_input_batch)) == 1){
            return(list(as.data.frame(ComBat_input_dat)))
        } else {
            
            # All 'one protein' files are skipped due to a ComBat limitation.
            # This is only a failsave. The second argument ensures trashing
            # empty dataframes
            if (nrow(ComBat_input_dat) > 1 &&
                length(unique(ComBat_input_batch)) > 1) {
                
                # If limma is chosen, the adjustment takes place in this
                # if-statement
                if (algorithm == "limma") {
                    ComBat_result <- as.data.frame(limma::removeBatchEffect(
                        ComBat_input_dat,
                        ComBat_input_batch
                    ))
                } else {
                    # Here the actual ComBat call takes place
                    # An if-else-statement is used to execute the chosen
                    # ComBat_mode
                    if (ComBat_mode == 1) {
                        # This is the default ComBat mode
                        ComBat_result <- as.data.frame(sva::ComBat(
                            dat = ComBat_input_dat,
                            batch = ComBat_input_batch
                        ))
                    } else if (ComBat_mode == 2) {
                        ComBat_result <- as.data.frame(sva::ComBat(
                            dat = ComBat_input_dat,
                            batch = ComBat_input_batch,
                            par.prior = TRUE,
                            mean.only = TRUE
                        ))
                    } else if (ComBat_mode == 3) {
                        ComBat_result <- as.data.frame(sva::ComBat(
                            dat = ComBat_input_dat,
                            batch = ComBat_input_batch,
                            par.prior = FALSE,
                            mean.only = FALSE
                        ))
                    } else if (ComBat_mode == 4) {
                        ComBat_result <- as.data.frame(sva::ComBat(
                            dat = ComBat_input_dat,
                            batch = ComBat_input_batch,
                            par.prior = FALSE,
                            mean.only = TRUE
                        ))
                    }
                }
                
                # In the list 'cured_subdfs' the ComBat-adjusted dataframes are
                # stored this return defines what will be gotten back from the
                # foreach-loop
                return(list(ComBat_result))
            } else {
                return(list())
            }
        }
    }
    
    doParallel::stopImplicitCluster()
    
    
    # Print to console
    if (verbosity >= 4) {
        message("Adjustment finishing time (parallel): ", Sys.time())
    }
    
    return(cured_subdfs)
}
