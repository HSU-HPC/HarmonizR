#' Main function
#'
#' This function executes the entire HarmonizR program and executes all other
#' functions found in this package. Therefore, this is the only function in
#' need of calling.
#'
#' @param data_as_input Path to input data. Additionally, the input can be a 
#' data.frame with proper row- and column names.
#' @param description_as_input Path to input description. Additionally, the 
#' input can be a data.frame with three columns total.
#' @param ... Unsettable parameter. Used to make all parameters below optional.
#' Documented to adhere with Bioconductor guidelines.
#' @param algorithm Optional. Pass either "ComBat" or "limma" to select the
#' preferred adjustment method. Defaults to ComBat.
#' @param ComBat_mode Optional. Pass a number between 1 and 4 to select the
#' desired ComBat parameters. Can only be set when ComBat is used. For
#' information on the meaning of the numbers, please view the SOP. Defaults to 
#' 1.
#' @param plot Optional. Takes either "samplemeans" for sample specific means,
#' "featuremeans" for feature specific means or "CV" for the coefficient of
#' variation as input and creates before/after plots for the given data. When
#' set, additionally writes out a .pdf file. Defaults to FALSE -> Turned off.
#' @param sort Optional. Method to sort by. Either FALSE or "sparsity_sort", 
#' "seriation_sort" or "jaccard_sort".
#' @param block Optional. How many batches should be treated as one during 
#' blocking. Greatly affects the number of sub-dataframes produced and reduces 
#' runtime. Turned off by default.
#' @param output_file Optional. Takes a string as input for the .tsv file name.
#' This can also be a path. Defaults to "cured_data", hence yielding a
#' "cured_data.tsv" file in the work directory from which it was called.
#' Can be turned of by passing FALSE.
#' @param verbosity Optional. Toggles the amount of information printed out by 
#' the HarmonizR algorithm during execution. Takes a number from 0 (also "mute)
#' to any positive number. The higher, the more information will be printed. For
#' the standard user, anything above 2 is rarely needed. Defaults to 1.
#' @param cores Optional. Manually sets the number of cores the user wants to
#' be used during HarmonizR's execution. Takes a positive integer. Defaults to
#' the amount of available cores.
#' @param ur Optional. Toggles the functionality of the removal of unique
#' combinations for increased data rescue. Defaults to TRUE. Not recommended to 
#' set to FALSE, as it exists for testing and reproducibility purposes.
#' @return The batch effect adjusted data.frame. Additionally, a .tsv file by 
#' default called "cured_data.tsv" will be written out as a result
#' @examples
#' # create a dataframe with 3 rows and 6 columns filled with random numbers
#' df <- data.frame(matrix(rnorm(n = 3*6), ncol = 6))
#' # set the column names
#' colnames(df) <- c("A", "B", "C", "D", "E", "F")
#' # create a vector of row names
#' row_names <- c("F1", "F2", "F3")
#' # set the row names
#' rownames(df) <- row_names
#' 
#' # create a vector of batch numbers
#' batch <- rep(1:3, each = 2)
#' # create a dataframe with 6 rows and 3 columns
#' des <- data.frame(ID = colnames(df), sample = 1:6, batch = batch)
#' 
#' # use the harmonizR() function; turning off creation of an output .tsv file
#' harmonizR(df, des, output_file = FALSE, cores = 1)
#' @export


# ----- harmonizR(
#         data_as_input,
#         description_as_input,
#         algorithm,
#         ComBat_mode,
#         plot,
#         sort,
#         block,
#         output_file,
#         verbosity,
#         cores,
#         ur
#       ) ---------------------------------------------------------------------
# This function will be called by the User and will execute the entire program
harmonizR <- function(
        data_as_input = NULL,
        description_as_input = NULL,
        ...,
        algorithm = "ComBat",
        ComBat_mode = 1,
        plot = FALSE,
        sort = FALSE,
        block = NULL,
        output_file = "cured_data",
        verbosity = 1,
        cores = FALSE,
        ur = TRUE
) {
    
    # Check verbosity input
    if (verbosity == "mute") {
        verbosity <- 0
    }
    if (verbosity < 0) {
        message(
            "Please set the verbosity parameter to an integer. ",
            "0 being mute, 1 being standard, higher numbers leading ",
            "to more console prints. Setting to default now (1).")
        verbosity <- 1
    }
    
    # Print to console
    if (verbosity >= 3) {
        message("Starting time: ", Sys.time())
    }
    
    # Print to console
    if (verbosity >= 1) {
        message("Initializing HarmonizR...")
    }
    
    # Check whether data and description are present
    if (is.null(data_as_input) && is.null(description_as_input)) {
        stop(
            "No parameters given. ",
            "Usage: harmonizR(\"path/to/data\", \"path/to/description\")")
    } else if (is.null(description_as_input)) {
        stop(
            "Not enough parameters. ",
            "Usage: main(\"path/to/data\", \"path/to/description\")")
    }
    
    # Check algorithm input
    if (algorithm != "ComBat" && algorithm != "limma") {
        message(
            "Please set the algorithm parameter to either ComBat or limma ",
            "to choose the prefered adjustment. Parameter is now being ",
            "set to default (ComBat).")
        algorithm <- "ComBat"
    }
    
    # Check ComBat_mode input
    if (ComBat_mode < 1 || ComBat_mode > 4) {
        message(
            "Invalid ComBat Mode chosen. Select 1, 2, 3 or 4. ",
            "Parameter is now being set to default (1).")
        ComBat_mode <- 1
    }
    
    # Check plot input
    if (plot != FALSE &&
        plot != "samplemeans" &&
        plot != "featuremeans" &&
        plot != "CV") {
        message(
            "Please set the plot parameter to either samplemeans, ",
            "featuremeans, CV or FALSE.")
        plot <- FALSE
    }
    
    # Check sort input
    # The first three if-statements are just for ease-of-use
    if (sort == "sp") {
        sort <- "sparsity_sort"
    } else if (sort == "se") {
        sort <- "seriation_sort"
    } else if (sort == "j") {
        sort <- "jaccard_sort"
    }
    # Here is the actual input catch
    if (sort != FALSE && 
        sort != "sparsity_sort" && 
        sort != "seriation_sort" && 
        sort != "jaccard_sort"){
        message(
            "Please set the sort parameter to either: 'sparsity_sort', ",
            "'seriation_sort', 'jaccard_sort' or FALSE. Parameter is now ",
            "set to default (FALSE)")
        sort <- FALSE
    }
    if (sort != FALSE && is.null(block)){
        message(
            "You are sorting the matrix without using the ",
            "block-parameter. In this case, sorting yields no benefit. ",
            "Still, continuing with sorting.")
    }
    
    # Check block input... not here. Read this comment:
    # Important: Blocking input is checked later in the respective section since
    # we need information about number of batches, which will earliest be gained
    # after reading in the input data
    
    # Check output_file input
    if (is.character(output_file) != TRUE && output_file != FALSE) {
        message(
            "Please only pass a string via the output_file parameter. ",
            "Alternatively, disable it with FALSE")
        output_file <- "cured_data"
    }
    
    # Check cores input
    if (cores != FALSE && is.double(cores) == FALSE){
        message(
            "Please pass a valid number as the cores argument. ",
            "Maximum cores will now be used.")
        cores <- FALSE
    }
    if (is.double(cores) == TRUE){
        # Get the maximum number of cores
        max_avail_cores <- parallel::detectCores()
        # User specifies more cores than there are available
        if (cores > max_avail_cores){
            message(
                "Your system does not provide the desired ", 
                cores, 
                " cores. Amount is now set to the maximum (", 
                max_avail_cores, 
                ").")
            cores <- max_avail_cores
        }
    }
    
    # ----- OPERATING SYSTEM CHECK SECTION ------------------------------------
    # Print to console
    if (verbosity >= 2) {
        # This line checks the OS
        message("Current operating system: ", toupper(.Platform$OS.type))
    }
    # In the current build, the used OS does not make a difference. I still
    # leave this section in for now despite it only being a priority-2-print
    
    
    # ----- READING SECTION ---------------------------------------------------
    # During reading, R already removes completely empty rows in the data.frame
    # Print to console
    if (verbosity >= 1) {
        message("Reading the files...")
    }
    
    # Logic for the Perseus plugin
    if (is.character(data_as_input)) {
        # Read in the data
        main_data <- read_main_data(data_as_input)
    } else {
        # Read in the data
        main_data <- as.data.frame(data_as_input)
    }
    
    # Logic for the Perseus plugin
    if (is.character(description_as_input)) {
        # Read in the batch-descriptions
        batch_data <- read_description(description_as_input)
    } else {
        # Read in the batch-descriptions
        batch_data <- description_as_input
    }
    
    # Information about which row of batch_data is in which batch
    batch_list <- fetch_batch_overview(batch_data)
    
    # Print to console (whenever this if-statement is TRUE)
    # Check if main_data matches the given description
    if (length(batch_list) != ncol(main_data)) {
        message(
            "The amount of samples specified in the description ",
            "does not match the amount of samples found in the given ",
            "matrix. This might cause a crash. Are you sure your input ",
            "is correctly formatted?")
    }
    
    # A backup later to be used for visualize since I never re-sort the
    # description file
    backup_batch_list <- batch_list
    
    number_features_before <- dim(main_data)[1]
    
    # Remove duplicates
    main_data <- unique(main_data)
    
    number_features_after <- dim(main_data)[1]
    
    # Print to console
    if (verbosity >= 1) {
        message("Preparing...")
    }
    
    # Print to console
    if (verbosity >= 2) {
        duplicates <- number_features_before - number_features_after
        message("Duplicate features found and removed:")
        message(duplicates)
    }
    
    order_to_go_by <- FALSE
    
    
    # ----- SORTING SECTION ---------------------------------------------------
    # This is toggleable by the user
    if (sort == "sparsity_sort" || 
        sort == "seriation_sort" || 
        sort == "jaccard_sort") {
        
        # Print to console
        if (verbosity >= 1) {
            message("Sorting with ", sort, "...")
        }
        
        order_to_go_by <- FALSE
        
        
        # - - - BINARY REDUCTION SUBSECTION - - - - - - - - - - - - - - - - - -
        # Needed for seriation- and jaccard-sorting
        if (sort == "seriation_sort" || sort == "jaccard_sort"){
            # Here, binary matrix reduction is used to get "binary_df"
            bin_input <- main_data
            bin_input[!is.na(bin_input)] <- 1
            bin_input[is.na(bin_input)] <- 0
            needed_val <- 1
            
            if (ComBat_mode == 1 ||
                ComBat_mode == 3 ||
                algorithm == "limma") {
                needed_val <- 2
            }
            
            # Function call to reduce the binary matrix
            binary_df <- binary_matrix_reduction(
                bin_input, 
                batch_list, 
                needed_val)
            
            if (sort == "seriation_sort") {
                # Usage of seriation sorting 
                # "seriate" and "get_order" come from the "seriation" package
                seriation_result <- seriation::seriate(binary_df, margin = 2)
                #print(seriation_result)
                order_to_go_by <- seriation::get_order(seriation_result[2])
            }
            
            if (sort == "jaccard_sort"){
                order_to_go_by <- jaccard(binary_df)
            }
        }
        
        
        # - - - ACTUAL SORTING SUBSECTION - - - - - - - - - - - - - - - - - - -
        # Get original names for later
        saved_colnames <- colnames(main_data)
        
        # 'sorting()' returns a list containing sorted main_data and sorted 
        # batch_data. It takes "order_to_go_by" as an argument. This is FALSE if
        # "sparsity_sort" is used and a list of length equal to the amount of
        # batches if another sorting approach is chosen
        main_desc <- sorting(
            main_data, 
            batch_list, 
            batch_data, 
            order_to_go_by, 
            verbosity
        )
        
        # Note: data.frame is important since elsewise it will be treated as a
        # list element
        # Update main_data
        main_data <- data.frame(main_desc[1], check.names = FALSE)
        # Update batch_data
        batch_data <- data.frame(main_desc[2])
        # Update batch_list
        batch_list <- fetch_batch_overview(batch_data)
    }
    
    
    # ----- BLOCKING SECTION --------------------------------------------------
    # Create empty "block_list" to be filled by 'blocking()' function
    block_list <- c()
    
    # Set "block_list" the same as "batch_list" in case blocking is not used
    if (is.null(block)) {
        block_list <- batch_list
    } 
    # Else create "block_list" based on user input
    else {
        number_batches <- utils::tail(batch_list, n=1)
        # For example: valid input for "block" with 5 batch data would be 2, 3 
        # or 4
        if (is.double(block) && block < number_batches && block > 1) {
            
            # Print to console
            if (verbosity >= 1) {
                message("Blocking...")
            }
            
            calculated_block_list <- blocking(batch_list, block)
            # Update "block_list" with the calculated result
            block_list <- calculated_block_list
            
        } else {
            message(
                "Please enter a valid number to be blocked by. ",
                "Continuing without blocking.")
            block_list <- batch_list
            block <- NULL
        }
        
        # Print to console
        if (verbosity >= 3 && is.null(block) == FALSE) {
            message("The given batch listings for all samples:")
            message(batch_list)
            message("Blocked (block = ",block ,"):", sep = "")
            message(block_list)
        }
    }
    
    
    # ----- SPOTTING SECTION --------------------------------------------------
    # Create the "affiliation_list", the list of vectors
    if (ComBat_mode == 1 ||
        ComBat_mode == 3 ||
        algorithm == "limma") {
        affiliation_list <- spotting_missing_values(
            main_data, 
            batch_list, 
            block_list, 
            2, 
            verbosity
        )
    }
    if (ComBat_mode == 2 || ComBat_mode == 4) {
        affiliation_list <- spotting_missing_values(
            main_data, 
            batch_list, 
            block_list, 
            1, 
            verbosity
        )
    }
    
    orig_copy <- affiliation_list
    
    
    # ----- REMOVING UNIQUE COMBINATIONS SECTION ------------------------------
    # Removing all unique combinations from "affiliation_list"
    if (ur != FALSE){
        new_affiliation_list <- unique_removal(affiliation_list)
        
        # Update "affiliation_list"
        affiliation_list <- new_affiliation_list
    }
    
    # Print to console
    if (verbosity >= 2) {
        
        killcount <- 0
        for (element in affiliation_list) {
            if (length(element) == 0){
                killcount <- killcount + 1
            }
        }
        message("Features with insufficient data to be considered:")
        message(killcount)
        
        new_orig <- list()
        for (element in orig_copy) {
            new_orig <- append(new_orig, toString(element))
        }
        
        string_affiliation_list <- list()
        for (element in affiliation_list) {
            string_affiliation_list <- append(
                string_affiliation_list, 
                toString(element))
        }
        message("Sub-dataframes produced in total:")
        message(length(unique(string_affiliation_list)))
    }
    
    
    # ----- SPLITTING SECTION -------------------------------------------------
    # Print to console
    if (verbosity >= 1) {
        message("Splitting the data using ", algorithm, " adjustment...")
    }
    
    # Split up the "main_data" dataframe into the sub-dataframes
    cured_subdfs <- splitting(
        affiliation_list,
        main_data,
        batch_data,
        block_list,
        algorithm,
        ComBat_mode,
        block,
        verbosity,
        cores
    )
    
    
    # ----- REBUILD SECTION ---------------------------------------------------
    # Print to console
    if (verbosity >= 1) {
        message("Rebuilding...")
    }
    
    # Build the result file by rebuilding the cured matrix from all viable
    # sub-dataframes
    cured <- rebuild(cured_subdfs)
    
    
    # ----- RESORT SECTION ----------------------------------------------------
    # Resorting both main_data and cured
    if (sort == "sparsity_sort" || 
        sort == "seriation_sort" || 
        sort == "jaccard_sort") {
        
        # Print to console
        if (verbosity >= 1) {
            message("Sorting back to normal...")
        }
        
        main_data <- main_data[ , saved_colnames]
        cured <- cured[ , saved_colnames]
    }
    
    
    # ----- WRITE-OUT SECTION -------------------------------------------------
    # The cured_data.tsv file is written, (if not turned off, so: != FALSE)
    if (output_file != FALSE){
        message("Writing file...")
        outfilename <- paste(output_file, "tsv", sep = ".")
        # 'unlink()' makes sure that a file with that ecaxt name gets deleted
        # to avoid conflicts/errors
        unlink(outfilename)
        utils::write.table(cured, outfilename, sep = "\t", col.names = NA)
    }
    
    
    # ----- VISUALIZATION SECTION ---------------------------------------------
    # Save a version of the logged cured for the harmonizR() return (since 
    # 'visual()', 'visual2()' and 'visual3' change it)
    original_cured <- cured
    
    # Use the original "backup_batch_list" for the visualize functions
    batch_list <- backup_batch_list
    
    # Reverse the log for visualizing
    main_data <- 2^main_data
    cured <- 2^cured
    
    if (plot == "featuremeans" || plot == "samplemeans" || plot == "CV"){
        message("Saving plot to pdf...")
        
        # Define the input data and batch list
        original <- NULL
        corrected <- NULL
        if (plot == "featuremeans") {
            original <- visual(main_data, batch_list)
            corrected <- visual(cured, batch_list)
        } else if (plot == "samplemeans") {
            original <- visual2(main_data, batch_list)
            corrected <- visual2(cured, batch_list)
        } else if (plot == "CV") {
            original <- visual3(main_data, batch_list)
            corrected <- visual3(cured, batch_list)
        }
        
        # Set the y-axis limit based on the original and corrected data
        lmts <- range(original, corrected)
        
        # Print to console
        if (verbosity >= 1) {
            message("Visualizing ", plot, "...")
        }
        
        # Create a boxplot in the plot panel and save it to a PDF file
        graphics::par(mfrow = c(1, 2))
        graphics::boxplot(original, main = "Original", las = 2, ylim = lmts)
        graphics::boxplot(corrected, main = "Corrected", las = 2, ylim = lmts)
        
        plotfilename <- paste(output_file, "pdf", sep = ".")
        # Width/height of the plot in inches
        grDevices::pdf(file = plotfilename, width = 4, height = 4) 
        graphics::boxplot(original, main = "Original", las = 2, ylim = lmts)
        graphics::boxplot(corrected, main = "Corrected", las = 2, ylim = lmts)
        grDevices::dev.off()
        
    }
    
    
    # ----- END ---------------------------------------------------------------
    # Print to console
    if (verbosity >= 1) {
        message("Termination.")
    }
    
    # Print to console
    if (verbosity >= 3) {
        message("Finishing time: ", Sys.time())
    }
    
    return(original_cured)
}