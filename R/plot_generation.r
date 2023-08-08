#' Calling the plotting functions for visualization purposes
#'
#' This function prepares the data and calls the underlying visualization
#' functions
#'
#' @param backup_batch_list Batch overview, which was backed up earlier.
#' @param main_data This is the input data.frame read in by the HarmonizR.
#' @param cured The HarmonizR-adjusted data.frame.
#' @param plot Passed by the user to indicate if and what to plot.
#' @param verbosity Optional. Toggles the amount of information printed out by 
#' the HarmonizR algorithm during execution. Takes a number from 0 (also "mute)
#' to any positive number. The higher, the more information will be printed. For
#' the standard user, anything above 2 is rarely needed. Defaults to 1.
#' @param output_file Optional. Takes a string as input for the .tsv file name.
#' This can also be a path. Defaults to "cured_data", hence yielding a
#' "cured_data.tsv" file in the work directory from which it was called.
#' Can be turned of by passing FALSE.
#' @return There is no return in this function.


# ----- plot_generation(backup_batch_list, main_data, cured, plot) ------------
# This function calls the underlying visualizing functions.
plot_generation <- function(
        backup_batch_list,
        main_data,
        cured,
        plot,
        verbosity,
        output_file
) {
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
    
}