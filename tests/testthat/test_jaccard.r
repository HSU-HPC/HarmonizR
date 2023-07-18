test_that("a jaccard function encountered a problem", {
    # create binary input (output of binary_matrix_reduction)
    binary_df <- cbind(c(1,0,0), c(1,1,1))
    
    # create the order we expect
    expected_order <- unlist(list(1,2))
    
    expect_equal(jaccard(binary_df), expected_order)
})