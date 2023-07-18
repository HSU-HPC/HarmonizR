# HarmonizR
## Overview
An implementation, which takes input data and makes it available for proper batch effect removal by ComBat or Limma. 
The implementation appropriately handles missing values by dissecting the input matrix into smaller matrices with sufficient data to feed the ComBat or Limma algorithm. 
The adjusted data is returned to the user as a rebuild matrix. 
The implementation is meant to make as much data available as possible with minimal data loss.

## Standard operating Procedure
For detailed instructions please refer to our [Standard operating Procedure](https://github.com/HSU-HPC/HarmonizR/blob/main/inst/HarmonizR_SOP.pdf).
Additionally, a [troubleshooting guide](https://github.com/HSU-HPC/HarmonizR/blob/main/inst/Troubleshooting.pdf) is provided.

## Installation
1. Download the release of the repository.

2. While having [devtools](https://www.r-project.org/nosvn/pandoc/devtools.html) installed, run
`devtools::install()` within the HarmonizR package.

Alternatively the newest package version can be installed directly from GitHub via the command `devtools::install_github("HSU-HPC/HarmonizR")`.

## Usage
Include `library(HarmonizR)` in your R script and execute it with your data and batch description files as demonstrated with the example files `harmonizR(“murine_medulloblastoma_data.tsv”, “murine_medulloblastoma_description.csv”)`. The files are found under HarmonizR/inst/extdata.

Optionally, HarmonizR is able to work with S4 `SummarizedExperiment` input data. S4 data with a single assay is required and the metadata (colData) must include batch information via "Batch". Example S4 data may be:

```R
# Creating example S4 data
nrows <- 20
ncols <- 8
counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
colData <- data.frame(Batch=c(1,1,1,1,2,2,2,2))
SummExp = SummarizedExperiment::SummarizedExperiment(assays=list(counts=counts), colData=colData)
```
