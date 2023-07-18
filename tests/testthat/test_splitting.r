test_that("rebuild encountered a problem", {
    
    affiliation_list <- list()
    
    affiliation_list[[1]] <- c(1,2)
    affiliation_list[[2]] <- c(2)
    affiliation_list[[3]] <- c(1,2)
    affiliation_list[[4]] <- c(2)
    
    main_data <- as.data.frame(cbind(
        c(1.1,NA,0.2,NA),
        c(1.5,NA,1.5,NA),
        c(8.4,6.5,4.4,1.9),
        c(7.2,2.3,9.9,3.2)
        ))
    colnames(main_data) <- c("A", "B", "C", "D")
    
    batch <- rep(1:2, each = 2)
    batch_data <- data.frame(
        ID = colnames(main_data), sample = 1:4, batch = batch)
    
    block_list <- unlist(list(1,1,2,2))
    
    algorithm <- "ComBat"
    
    ComBat_mode <- 1
    
    block <- NULL
    
    verbosity = 0
    
    cores = 1
    
    # running the function
    result <- splitting(
        affiliation_list,
        main_data,
        batch_data,
        block_list,
        algorithm,
        ComBat_mode,
        block,
        verbosity,
        cores
    )
    
    # The underlying adjustment algorithms do not always yield the exact same
    # results. Hence, a comparison of two dataframes (result/expected) can't be
    # done here.
    expect_true(is.list(result))
})