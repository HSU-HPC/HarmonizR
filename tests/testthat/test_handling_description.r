test_that("handling the description encountered a problem", {
    # given correctly formatted input:
    
    # create a dataframe with 3 rows and 6 columns filled with random numbers
    # (uniformity reasons throughout the tests)
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
    
    
    # test for numeric
    expect_true(is.numeric(fetch_batch_overview(des)))
    
    # manually extract from description to check against
    batch_list <- as.numeric(c(des[3])[[1]])
    
    # test against initial input
    expect_equal(fetch_batch_overview(des), batch_list)
})