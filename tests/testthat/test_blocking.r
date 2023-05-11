test_that("block encountered a problem", {
    # input-creation
    batch_list <- unlist(list(1,1,2,2,3,3,4,4))
    block <- 2
    
    # compare the result against this
    expected_result <- unlist(list(1,1,1,1,2,2,2,2))
    
    expect_equal(blocking(batch_list, block), expected_result)
})