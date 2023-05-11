test_that("hamonizR encountered a problem", {
    # given correctly formatted input:
    
    # create a dataframe with 3 rows and 6 columns filled with random numbers
    df <- data.frame(matrix(rnorm(n = 3*6), ncol = 6))
    # set the column names
    colnames(df) <- c("A", "B", "C", "D", "E", "F")
    # create a vector of row names
    row_names <- c("F1", "F2", "F3")
    # set the row names
    rownames(df) <- row_names
    
    # create a vector of batch numbers
    batch <- rep(1:3, each = 2)
    # create a dataframe with 6 rows and 3 columns
    des <- data.frame(ID = colnames(df), sample = 1:6, batch = batch)
    
    # the actual test; turning off creation of an output .tsv file
    # The underlying adjustment algorithms do not always yield the exact same
    # results. Hence, a comparison of two dataframes (result/expected) can't be
    # done here.
    expect_true(is.list(harmonizR(
        df, des, cores = 1, output_file = FALSE, verbosity = "mute")))
})