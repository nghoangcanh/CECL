#
# PROCESSER.R
# Author: Yusuf Saib
# Date: Jul 18, 2017
# Copyright © 2017 CECL. All rights reserved.
#

# Function: process_pad
process_pad <- function(x, n) {
    len.diff <- n - length(x)
    as.matrix(c(rep(NA, len.diff), x))
}

# Function: process log nature with matrix[n,m], but except column 1
process_log_nature_frame <- function(input_xy) {
    result <- apply(input_xy[-1], 2, function(x) log(x))
    return (result)
}

# Function: process log nature with matrix [n, 1],
process_log_nature_matrix_y <- function(input_y) {
    result <- apply(input_y, 2, function(x) log(x))
    return (as.matrix(result))
}

# Function: process_logarithm: 
# parmas @base_y: a column vector
# parmas @input_y: a maxtrix [n, 1], same number of rows with base_y
# return: a frame = [base_y, calculated of input_y]
process_logarithm <- function(base_y, input_y) {

    # length of input
    length_input <- length(input_y)

    # names
    name_input <- colnames(input_y)
    name_d_input <- paste("d", name_input, sep = "")
    name_ln_input <- paste("ln", name_input, sep = "")
    name_dln_input <- paste("dln", name_input, sep = "")
    name_d4_input <- paste("d4", name_input, sep = "")
    name_dln4_input <- paste("dln4", name_input, sep = "")

    # log/diff input_y
    d_input <-  process_pad(diff(input_y), n = length_input)
    ln_input <- process_log_nature_matrix_y(input_y)
    dln_input <-  process_pad(diff(ln_input), n = length_input)
    d4_input <- process_pad(diff(input_y, lag = 4), n = length_input)
    dln4_input <-  process_pad(diff(ln_input, lag = 4), n = length_input)

    # change column names
    colnames(d_input) <- c(name_d_input)
    colnames(ln_input) <- c(name_ln_input)
    colnames(dln_input) <- c(name_dln_input)
    colnames(d4_input) <- c(name_d4_input)
    colnames(dln4_input) <- c(name_dln4_input)

    # combine frames
    cbind(base_y, input_y, d_input, ln_input, dln_input, d4_input, dln4_input)
}

# Function: process_final: 
# parmas @input_xy: a maxtrix [x,y]
# parmas @cindex: apply at column index
# return: a final matrix with omited NAs
process_final <- function(input_xy, cindex) {
    names <- colnames(input_xy)
    input_xy <- input_xy[complete.cases(input_xy[, cindex]), ]
    colnames(input_xy) <- names
    rownames(input_xy) <- NULL
    return (input_xy)
}