rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
setwd("C:/Users/eung.cho/Desktop/legdays/lendingclub")
source("src/getData.r")

# RejectStats probably not useful, even if we can match them up with an approved loan (i.e., the borrower reapplied)
# myRejectStats <- getRejectStats(aRejectStatsLinks) 

# aLink <- c("https://resources.lendingclub.com/LoanStats3a.csv.zip",
#            "https://resources.lendingclub.com/LoanStats3b.csv.zip",
#            "https://resources.lendingclub.com/LoanStats3c.csv.zip")
# aRejectStatsLinks <- c("https://resources.lendingclub.com/RejectStatsA.csv.zip")
aLoanLinks <- c("https://resources.lendingclub.com/LoanStats3a.csv.zip")

# Ignoring the non-policy loans for now, arbitrarily
myLoans <- transformLoans(getLoans(aLoanLinks)[[1]])

myFullyPaidLoans <- myLoans[myLoans$loan_status %in% c("Fully Paid"), ]
myDefaultedLoans <- myLoans[myLoans$loan_status %in% c("Default", "Charged Off"), ]

sapply(myLoans$issue_d)

# Sanity checks
myIsHealthyDTI <- as.numeric(myLoans$dti) < 1
myFullyPaidLoans[myIsHealthyDTI,]
any(mapply(function(x, y) x > y, myLoans$total_pymnt_inv, myLoans$total_pymnt))