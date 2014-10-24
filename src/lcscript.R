rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
setwd("C:/Users/eung.cho/Desktop/legdays/lendingclub")
source("src/getData.R")
source("src/functions.R")
source("src/enrichLCData.R")

# RejectStats probably not useful, even if we can match them up with an approved loan (i.e., the borrower reapplied)
# myRejectStats <- getRejectStats(myRejectStatsLinks) 
#dont own home and have no mortgage is good thing? but negatively affects grade

# myLoanLinks <- c("https://resources.lendingclub.com/LoanStats3a.csv.zip",
#            "https://resources.lendingclub.com/LoanStats3b.csv.zip",
#            "https://resources.lendingclub.com/LoanStats3c.csv.zip")
# myRejectStatsLinks <- c("https://resources.lendingclub.com/RejectStatsA.csv.zip")
myLoanLinks <- c("https://resources.lendingclub.com/LoanStats3a.csv.zip")

# Ignoring the non-policy loans for now, arbitrarily
myRawLoans <- getLoans(myLoanLinks)[[1]]
myLoans <- enrichLoans(myRawLoans)

myFullyPaidLoans <- myLoans[myLoans$loan_status %in% c("Fully Paid"), ]
myDefaultedLoans <- myLoans[myLoans$loan_status %in% c("Default", "Charged Off"), ]

# weight by principal
myDefaultedLoans$realizedRates %*% myDefaultedLoans$funded_amnt
# Sanity checks
myIsHealthyDTI <- as.numeric(myLoans$dti) < 1
myFullyPaidLoans[myIsHealthyDTI,]
any(mapply(function(x, y) x > y, myLoans$total_pymnt_inv, myLoans$total_pymnt)) #???? how?? same with funded_amnt