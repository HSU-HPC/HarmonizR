test_that("binary matrix reduction encountered a problem", {
    # input-creation
    binary_data <- cbind(c(1,0,0), c(1,1,0), c(1,1,1), c(1,1,1))
    batch_list <- unlist(list(1,1,2,2))
    needed_values <- 2
    
    # compare the result against this
    expected_result <- cbind(c(1,0,0), c(1,1,1))
    
    expect_equal(
        binary_matrix_reduction(binary_data, batch_list, needed_values),
        expected_result
        )
})