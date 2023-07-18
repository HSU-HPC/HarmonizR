test_that("visual encountered a problem", {
    # input
    binary_data <- cbind(c(1,0,0), c(1,1,0), c(1,1,0), c(1,1,1))
    batch_list <- unlist(list(1,1,2,2))
    
    # result
    result <- visual(binary_data,batch_list)
    
    # expected result
    expected_plotting_data <- cbind(c(1.0,0.5,0.0), c(1,1,0.5))
    # set the column names
    colnames(expected_plotting_data) <- c("Batch_1", "Batch_2")
    expected_plotting_data <- as.data.frame(expected_plotting_data)
    
    # test
    expect_equal(result, as.data.frame(expected_plotting_data))
})