#
# IMPORTER.R
# Author: Yusuf Saib
# Date: Jul 18, 2017
# Copyright © 2017 CECL. All rights reserved.
#

# excel library
library(readxl)

# excel path
excel_path <- "data/Macro_Data.xlsx"

# Function: read excel data into data frame. Range is by col:col
importer_read <- function(range) {
    df <- read_excel(excel_path, range = cell_cols(range))
    df <- data.frame(df)
    return (df)
}

# Function: importer_get_header_name
importer_get_header_name <- function(column) {
    c1 <- paste(column, "1", sep = "")
    c2 <- paste(column, "5", sep = "")
    range <- paste(c1, c2, sep = ":")
    df <- read_excel(excel_path, range = range)
    cname <- colnames(df)
    return (cname)
}