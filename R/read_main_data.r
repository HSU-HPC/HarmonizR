#' Reading main data
#'
#' The read_main_data function reads in a file via its file path and converts
#' it to a for the rest of the workflow readable format.
#'
#' @param data_source Usually the path to the input data. It can also be passed 
#' directly as a correctly formatted data.frame.
#' @return To-be-adjusted data as data.frame


# ----- read_main_data(data_source) -------------------------------------------
# The data is read and then every column is turned into either a numeric or NA
read_main_data <- function(data_source) {
    main_data <- utils::read.table(
        data_source,
        sep = "\t", header = TRUE,
        row.names = 1, check.names = FALSE,
        comment.char = "", stringsAsFactors = FALSE
    )
    
    # Janitor package: remove all empty rows and columns from the data.frame
    # rows aren't really a problem, but extra empty columns screw everything up
    main_data <- janitor::remove_empty(
        main_data,
        which = c("rows", "cols"), 
        quiet = TRUE
    )
    
    # Make numerical (I'd like to suppress the 'introduced NAs by coercion' 
    # warnings, which appear once a character is found, since it is the exact
    # behavior I want). Yet, removed to avoid a note. At least the user is now
    # somewhat informed about something within their input being turned to NA.
    cols.num <- names(main_data)
    main_data[cols.num] <- vapply(
        main_data[cols.num], as.numeric, numeric(nrow(main_data)))
    
    return(main_data)
}
