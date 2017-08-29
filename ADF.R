#
# ADF.R
# Author: Canhnh
# Date: 08 17, 2017
# Copyright © 2017 CECL. All rights reserved.
#

library(urca)
library(knitr)

data(nottem)
# Test root ADF with trend and lags=0
ADF_TREND <- ur.df(nottem, type = c("trend"), lags = 0)
ADF_DRIFT <- ur.df(nottem, type = c("drift"), lags = 0)
ADF_NONE <- ur.df(nottem, type = c("none"), lags = 0)

ResultList <- c("STATIONARY",
"NOT STATIONARY WITH TREND",
"NOT STATIONARY WITH TREND AND INTERCEPT",
"NOT STATIONARY WITH DRIFT",
"NOT STATIONARY WITHOUT DRIFT OR TREND"
    )

Value_tstatisticPhi3 <- abs(ADF_TREND@teststat[1, 3])
Value_tstatisticPhi2 <- abs(ADF_TREND@teststat[1, 2])
Value_tstatisticPhi1 <- abs(ADF_DRIFT@teststat[1, 2])
Value_tstatisticTau3 <- abs(ADF_TREND@teststat[1, 1])
Value_tstatisticTau2 <- abs(ADF_DRIFT@teststat[1, 2])
Value_tstatisticTau1 <- abs(ADF_NONE@teststat[1])

Value_5pctPhi3 <- abs(ADF_TREND@cval[3, 2])
Value_5pctTau3 <- abs(ADF_TREND@cval[1, 2])
Value_5pctPhi2 <- abs(ADF_TREND@cval[2, 2])
Value_5pctPhi1 <- abs(ADF_DRIFT@cval[2, 2])
Value_5pctTau2 <- abs(ADF_DRIFT@cval[1, 2])
Value_5pctTau1 <- abs(ADF_NONE@cval[1, 2])


if (Value_tstatisticPhi3 > Value_5pctPhi3) {
    #Step2
    if (Value_tstatisticTau3 > Value_5pctTau3) {
        #2.1
        print (ResultList[1])
    }
    else {
        if (Value_tstatisticPhi2 > Value_5pctPhi2) {
            #2.2.1
            print(ResultList[2])
        }
        else {
            #2.2.2
            print(ResultList[3])
        }
    }
} else {
    #Step3
    if (Value_tstatisticPhi1 < Value_5pctPhi1) {
        if (Value_tstatisticTau1 < Value_5pctTau1) {
            #3.1
            print(ResultList[4])
        }
        else {
            #5.1
            print("Case 5.1 Testing")
        }
        
    }
    else {
        #Step4)
        if (Value_tstatisticTau2 < Value_5pctTau2) {
            #4.1
            print(ResultList[5])
        }
        else {
            #4.2
            print("4.2" + ResultList[1])
        }
    }
}

#Step 1---------------------------------------------------------------------
# 1.1: the computed absolute t-statistic for Phi3 is larger than the absolute critical value at 5% confidence level
# GOTO Step 2
# 1.2: the computed absolute t-statistic for Phi3 is smaller than the absolute critical value at 5% confidence level
# GOTO Step 3
#EndofStep1

#Step 2----------------------------------------------------------------------
# 2.1: The computed absolute t-statistic for Tau3 is larger than the absolute critical value at 5% confidence level, then the series is STATIONARY
# 2.2: If the computed absolute t-statistic for Tau3 is smaller than the absolute critical value at 5% confidence level
# 2.2.1: If the computed absolute t-statistic for Phi2 is larger than the absolute critical value at 5% confidence level, then the series is NOT STATIONARY WITH TREND.
# 2.2.2: If the computed absolute t - statistic for Phi2 is smaller than the absolute critical value at 5 % confidence level, then the series is NOT STATIONARY WITH TREND AND INTERCEPT.
#End of Step2

#Step 3----------------------------------------------------------------------
#3.1: If the computed absolute t-statistic for Phi1 is smaller than the absolute critical value at 5% confidence level, then the series is NOT STATIONARY WITHOUT DRIFT OR TREND.
#3.2: If the computed absolute t - statistic for Phi1 is larger than the absolute critical value at 5 % confidence level, then GO TO STEP 4.
#End of Step3

#Step 4----------------------------------------------------------------------
#4.1.	If the computed absolute t-statistic for Tau2 is smaller than the absolute critical value at 5% confidence level, then the series is NOT STATIONARY WITH DRIFT.
#4.2.	If the computed absolute t-statistic for Tau2 is larger than the absolute critical value at 5% confidence level, then the series is STATIONARY.
#End of Step4

#Step 5----------------------------------------------------------------------
#5.1 . If you conclude as 3.1 ., then check the ADF test output without trend and drift. It should be consistent. In other words, the computed absolute t - statistic for
#Tau1 should be smaller than the absolute critical value at 5 % confidence level and therefore, the series should be NOT STATIONARY WITHOUT DRIFT OR TREND.
#End of Step5

#knitr::kable(list(ADF_TREND@cval, ADF_DRIFT@cval, ADF_NONE@cval), format = "html", caption = "ADF TEST ROOT")

options(knitr.table.format = "html")

t1 = knitr::kable(ADF_TREND@cval, caption = "ADF TREND")
t2 = knitr::kable(ADF_DRIFT@cval, caption = "ADF DRIFT")
t3 = knitr::kable(ADF_NONE@cval, caption = "ADF NONE")

t4 = knitr::kable(ADF_TREND@teststat, caption = "ADF TREND")
t5 = knitr::kable(ADF_DRIFT@teststat, caption = "ADF DRIFT")
t6 = knitr::kable(ADF_NONE@teststat, caption = "ADF NONE")

cat(c('<table><tr valign="top"><td>', t1, '</td>', '<td>', t2, '</td>', '<td>', t3, '</td>,
       <td>', t4, '</td>', '<td>', t5, '</td>', '<td>', t6, '</td></tr></table>'), sep = "\n")
