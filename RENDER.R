#
# render.R
# Author: Canhnh
# Date: 08 17, 2017
# Copyright © 2017 CECL. All rights reserved.
#

library(rmarkdown)

# Load source file
source("VECM.R")

# markdown template
raw_vecm_input_path <- "vecm.Rmd"

# markdown output html file
raw_test_vecm_file <- "vecm.html"

# RAW VECM render
rmarkdown::render(raw_vecm_input_path, raw_test_vecm_file)