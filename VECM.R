#
# VECM.R
# Author: Yusuf Saib
# Date: 08 19, 2017
# Copyright © 2017 CECL. All rights reserved.
#

#Johansen Cointegration in R
library(vars)   #for Varselect
library(readxl) # for read_excel function
library(rmarkdown) # for output html file
#library(tsDyn)  # for VECM function

# Test with data in excel file Macro_Data.xlsx
# excel path
excel_path <- "data/Macro_Data.xlsx"

# Function: read excel data into data frame. Range is by col:col
importer_read <- function(range) {
    df <- read_excel(excel_path, range = cell_cols(range))
    df <- data.frame(df)
    return(df)
}

# Function for cal log matrix
process_log <- function(input_xy) {
    result <- apply(input_xy, 2, function(x) log(x))
    return(result)
}

# Read data from file xlsx
df <- importer_read("B:T")

# Following order for Johansen cointegration test
print("
- RGDP
- UR
- CPI
- TR10
- TR3M
- SP500
- HPI
- CREPI
")

# Bind data by the list order as requirement
raw_data <- df[, c("RGDP", "UR", "CPI", "TR10", "TR3M", "SP500", "HPI", "CREPI")]

# ------------------------------VECM MODEL: IMPLEMENTATION-------------------------------

# Need to process data follow order for VECM
print("
- log RGDP
- UR
- dlog CPI
- TR10
- TR3M
- log SP500
- log HPI
- log CREPI
")

# 0. Processing data follow list series
data4VECM <- raw_data
# processing with log RGDP
data4VECM[, 1] <- log(data4VECM[, 1])
# processing with log CPI
data4VECM[, 3] <- log(data4VECM[, 3])
# processing with dlog CPI
data4VECM[2:length(data4VECM[, 3]), 3] <- diff(data4VECM[, 3]) # replace CPI without line 1 that NA
data4VECM[1, 3] <- 0 # Value is NA but VARselect will error with "NAs in y"
# processing with SP500, HPI, CREPI
data4VECM[, 6:8] <- log(data4VECM[, 6:8])

# 1. Varselect using max.lag=5 and type = const
varModel <- VARselect(data4VECM, lag.max = 5, type = "const")

# 2. K4VECM will be varModel$selection[1] - 1
K4VECM <- varModel$selection[1] - 1

#Conduct EIGEN test (Cointegration test) 
ct_eigen_trans_none <- ca.jo(data4VECM, K4VECM, type = "eigen", ecdet = "none", spec = "transitory")
ct_eigen_trans_const <- ca.jo(data4VECM, K4VECM, type = "eigen", ecdet = "const", spec = "transitory")
ct_eigen_trans_trend <- ca.jo(data4VECM, K4VECM, type = "eigen", ecdet = "trend", spec = "transitory")

ct_eigen_lr_none <- ca.jo(data4VECM, K4VECM, type = "eigen", ecdet = "none", spec = "longrun")
ct_eigen_lr_const <- ca.jo(data4VECM, K4VECM, type = "eigen", ecdet = "const", spec = "longrun")
ct_eigen_lr_trend <- ca.jo(data4VECM, K4VECM, type = "eigen", ecdet = "trend", spec = "longrun")


#Conduct TRACE test (Cointegration test)
ct_trace_trans_none <- ca.jo(data4VECM, K4VECM, type = "trace", ecdet = "none", spec = "transitory")
ct_trace_trans_const <- ca.jo(data4VECM, K4VECM, type = "trace", ecdet = "const", spec = "transitory")
ct_trace_trans_trend <- ca.jo(data4VECM, K4VECM, type = "trace", ecdet = "trend", spec = "transitory")

ct_trace_lr_none <- ca.jo(data4VECM, K4VECM, type = "trace", ecdet = "none", spec = "longrun")
ct_trace_lr_const <- ca.jo(data4VECM, K4VECM, type = "trace", ecdet = "const", spec = "longrun")
ct_trace_lr_trend <- ca.jo(data4VECM, K4VECM, type = "trace", ecdet = "trend", spec = "longrun")

# Run VECM in package urca
# Result is same with VECM function of tsDyn
# vecm_const <- VECM(data, lag = 1, LRinclude = "const", estim = "ML")

vecm_ct_eigen_trans_none <- cajorls(ct_eigen_trans_none) #convert in vecm
vecm_ct_eigen_trans_const <- cajorls(ct_eigen_trans_const) #convert in vecm
vecm_ct_eigen_trans_trend <- cajorls(ct_eigen_trans_trend) #convert in vecm
vecm_ct_eigen_lr_none <- cajorls(ct_eigen_lr_none) #convert in vecm
vecm_ct_eigen_lr_const <- cajorls(ct_eigen_lr_const) #convert in vecm
vecm_ct_eigen_lr_trend <- cajorls(ct_eigen_lr_trend) #convert in vecm

vecm_ct_trace_trans_none <- cajorls(ct_trace_trans_none) #convert in vecm
vecm_ct_trace_trans_const <- cajorls(ct_trace_trans_const) #convert in vecm
vecm_ct_trace_trans_trend <- cajorls(ct_trace_trans_trend) #convert in vecm
vecm_ct_trace_lr_none <- cajorls(ct_trace_lr_none) #convert in vecm
vecm_ct_trace_lr_const <- cajorls(ct_trace_lr_const) #convert in vecm
vecm_ct_trace_lr_trend <- cajorls(ct_trace_lr_trend) #convert in vecm

# Markdown template
raw_vecm_input_path <- "vecm.Rmd"

# RAW VECM render
rmarkdown::render(raw_vecm_input_path) # Need function to check if vecm.html exists

# ------------------------------VAR MODEL: IMPLEMENTATION-------------------------------


# Need to process data follow order for VAR()
print("
-dlog RGDP
-UR 
-First difference of dlog CPI  
-dTR10 
-dTR3M
-dlog SP500
-dlog HPI 
-dlog CREPI
")


# 0. Processing with data follow series above
data4VAR <- raw_data

# processing with RGDP
data4VAR[, 1] <- log(data4VAR[, 1])
# processing with log 3-8 column
data4VAR[, 3:8] <- log(data4VAR[, 3:8])

# Remove INF values
data4VAR[!is.finite(as.matrix(data4VAR))] <- 0

# Processing calculate dlog
data4VAR[, 1] <- c(NA, diff(data4VAR$RGDP))
data4VAR[, 3] <- c(NA, diff(data4VAR$CPI))
data4VAR[, 4] <- c(NA, diff(data4VAR$TR10))
data4VAR[, 5] <- c(NA, diff(data4VAR$TR3M))
data4VAR[, 6] <- c(NA, diff(data4VAR$SP500))
data4VAR[, 7] <- c(NA, diff(data4VAR$HPI))
data4VAR[, 8] <- c(NA, diff(data4VAR$CREPI))

#processing first difference for dlog CPI (diff twice)
data4VAR[, 3] <- c(NA, diff(data4VAR$CPI))

# Remove NA values
data4VAR[is.na(data4VAR)] <- 0

# 1. Run VARselect  
varModel4VAR <- VARselect(data4VAR, lag.max = 5, type = "const")

# 2. Calculate K
K4VAR = varModel4VAR$selection[1]

# 3. Use VAR(DATA, p = K, type = c("const"), season = NULL, exogen = NULL, lag.max = NULL). 
#VAR_Both <- VAR(data4VAR, K4VAR, type = c("both"), season = NULL, exogen = NULL, lag.max = NULL)
#VAR_Both <- VAR(data4VAR, K4VAR, type = c("none"), season = NULL, exogen = NULL, lag.max = NULL)
#VAR_Both <- VAR(data4VAR, K4VAR, type = c("trend"), season = NULL, exogen = NULL, lag.max = NULL)
VAR_Const <- VAR(data4VAR, K4VAR, type = c("const"), season = NULL, exogen = NULL, lag.max = NULL)

# Markdown template
raw_vecm_input_path <- "var.Rmd"
# RAW VECM render
rmarkdown::render(raw_vecm_input_path) # Need function to check if vecm.html exists