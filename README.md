# HarmonizR
## Overview
An implementation, which takes input data and makes it available for proper batch effect removal by ComBat or Limma. 
The implementation appropriately handles missing values by dissecting the input matrix into smaller matrices with sufficient data to feed the ComBat or Limma algorithm. 
The adjusted data is returned to the user as a rebuild matrix. 
The implementation is meant to make as much data available as possible with minimal data loss.

## Standard operating Procedure
For detailed instructions please refer to our [Standard operating Procedure](https://github.com/HSU-HPC/HarmonizR_v1.1/blob/main/inst/HarmonizR_SOP.pdf).

## Installation
1. Clone this repository

2. While having [devtools](https://www.r-project.org/nosvn/pandoc/devtools.html) installed, run
`devtools::install("HarmonizR")` within the HarmonizR package.

Alternatively the package can be installed directly from GitHub via the command devtools::install_github(“HSU-HPC/HarmonizR”).

## Usage
Include `library(HarmonizR)` in your R script and execute it with your data and batch description files as demonstrated with the example files `harmonizR(“murine_medulloblastoma_data.tsv”, “murine_medulloblastoma_description.csv”)`.
