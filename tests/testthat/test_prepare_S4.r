test_that("prepare_S4 encountered a problem", {
    # Creating example S4 data
    nrows <- 20
    ncols <- 8
    counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
    colData <- data.frame(Batch=c(1,1,1,1,2,2,2,2))
    SummExp = SummarizedExperiment::SummarizedExperiment(
        assays=list(counts=counts), 
        colData=colData)
    
    Harm_dfs = format_from_S4(SummExp)
    
    dimension_des = dim(Harm_dfs[[2]])
    
    # Since the actual data matrix stays the same, we check for the description
    expect_true(all(dimension_des==c(8,3)))
    
    
    extracted_assay <- data.frame(SummarizedExperiment::assay(SummExp))
    
    resulting_SummExp <- format_to_s4(extracted_assay, SummExp)
    
    expect_true(typeof(resulting_SummExp)=="S4")
})
