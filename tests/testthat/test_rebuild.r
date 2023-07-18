test_that("rebuild encountered a problem", {
    
    # build the dataframes we need (need to be data.frame)
    df1 <- cbind(c(1,8,3), c(2,9,3))
    df2 <- cbind(c(5,5,6), c(0,7,1))
    
    # set the column/row names
    colnames(df1) <- c("A", "B")
    colnames(df2) <- c("A", "B")
    row_names <- c("F1", "F2", "F3")
    rownames(df1) <- row_names
    row_names <- c("F4", "F5", "F6")
    rownames(df2) <- row_names
    
    scattered_dfs <- list(as.data.frame(df1), as.data.frame(df2))
    
    # create the output we expect (needs to be data.frame)
    expected_dataframe <- cbind(c(1,8,3,5,5,6), c(2,9,3,0,7,1))
    colnames(expected_dataframe) <- c("A", "B")
    row_names <- c("F1", "F2", "F3", "F4", "F5", "F6")
    rownames(expected_dataframe) <- row_names
    expected_dataframe <- as.data.frame(expected_dataframe)
    
    expect_equal(rebuild(scattered_dfs), expected_dataframe)
})