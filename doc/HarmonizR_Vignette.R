## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(HarmonizR)

## -----------------------------------------------------------------------------
# create a dataframe with 3 rows and 6 columns filled with random numbers
df <- data.frame(matrix(rnorm(n = 3*6), ncol = 6))
# set the column names
colnames(df) <- c("A", "B", "C", "D", "E", "F")
# create a vector of row names
row_names <- c("F1", "F2", "F3")
# set the row names
rownames(df) <- row_names
# this is what it looks like:
df

## -----------------------------------------------------------------------------
# create a vector of batch numbers
batch <- rep(1:3, each = 2)
# create a dataframe with 6 rows and 3 columns
des <- data.frame(ID = colnames(df), sample = 1:6, batch = batch)
# this is what it looks like:
des

## -----------------------------------------------------------------------------
# use the harmonizR() function
result <- harmonizR(df, des)

# delete output file
unlink('cured_data.tsv')

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
sessionInfo()

