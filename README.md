# HarmonizR
## Overview
An implementation, which takes input data and makes it available for proper batch effect removal by ComBat or Limma. 
The implementation appropriately handles missing values by dissecting the input matrix into smaller matrices with sufficient data to feed the ComBat or Limma algorithm. 
The adjusted data is returned to the user as a rebuild matrix. 
The implementation is meant to make as much data available as possible with minimal data loss.


## Installation
This package has been published originally in a [different repository](https://github.com/SimonSchlumbohm/HarmonizR), where it has a stable zenodo ID. It can still be downloaded and installed from here using method 1..

1. Download this package. While having [devtools](https://www.r-project.org/nosvn/pandoc/devtools.html) installed, run `devtools::install("HarmonizR")` within the HarmonizR package.

2. The package can be installed directly from GitHub via the command `devtools::install_github("SimonSchlumbohm/HarmonizR/HarmonizR")`.


## Usage
Include `library(HarmonizR)` in your R script and execute it with your data and batch description files as demonstrated with the example files `harmonizR("murine_medulloblastoma_data.tsv", "murine_medulloblastoma_description.csv")`. 
-> files are found in HarmonizR/inst/extdata


## Standard operating Procedure
For a detailed instruction please refer to our [Standard operating Procedure](https://github.com/SimonSchlumbohm/HarmonizR/blob/main/HarmonizR_SOP.pdf).
