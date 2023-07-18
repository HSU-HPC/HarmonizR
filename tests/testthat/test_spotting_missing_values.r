test_that("spotting encountered a problem", {
    # all input
    df <- cbind(c(5.3,5.5,6.3), c(NA,7.2,1.1), c(2.2,4.5,9.2), c(2.1,2.3,NA))
    batch_list <- unlist(list(1,1,2,2))
    block_list <- batch_list
    needed_values <- 2
    
    # expected result / result
    expected_affiliations <- list(2,c(1,2),1)
    
    result <- spotting_missing_values(
        df, batch_list, block_list, needed_values, 0)
    
    expect_equal(result, expected_affiliations)
})