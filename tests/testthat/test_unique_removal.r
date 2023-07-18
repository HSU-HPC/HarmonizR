test_that("ur encountered a problem", {
    # input
    affiliation_list <- list()
    
    affiliation_list[[1]] <- c(1,2,3)
    affiliation_list[[2]] <- c(1,3,4) # should go to c(1,3)
    affiliation_list[[3]] <- c(1,3)
    affiliation_list[[4]] <- c(1,2,3)
    affiliation_list[[5]] <- c(1,3)
    affiliation_list[[6]] <- c(2,4)
    affiliation_list[[7]] <- c(2,4)
    affiliation_list[[8]] <- c(1,2,4) # should go to c(2,4)
    affiliation_list[[9]] <- c(1) # length < 2
    affiliation_list[[10]] <- vector() # length < 2
    affiliation_list[[11]] <- c(1,4) # should go to c(1)
    affiliation_list[[12]] <- c(3,4) # should go to empty
    affiliation_list[[13]] <- c(1,2,3,4,5,6) # should go to c(1,2,3,4,5)
    affiliation_list[[14]] <- c(1,2,3,4,5) # should STAY THE SAME since it gets matched to!
    
    # expected output
    expected_aff_res <- list()
    
    expected_aff_res[[1]] <- c(1,2,3)
    expected_aff_res[[2]] <- c(1,3)
    expected_aff_res[[3]] <- c(1,3)
    expected_aff_res[[4]] <- c(1,2,3)
    expected_aff_res[[5]] <- c(1,3)
    expected_aff_res[[6]] <- c(2,4)
    expected_aff_res[[7]] <- c(2,4)
    expected_aff_res[[8]] <- c(2,4)
    expected_aff_res[[9]] <- c(1)
    expected_aff_res[[10]] <- vector()
    expected_aff_res[[11]] <- c(1)
    expected_aff_res[[12]] <- vector()
    expected_aff_res[[13]] <- c(1,2,3,4,5)
    expected_aff_res[[14]] <- c(1,2,3,4,5)
    
    expect_equal(unique_removal(affiliation_list), expected_aff_res)
})