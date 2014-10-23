rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
setwd("C:/Users/eung.cho/Desktop/legdays/lendingclub")
source("src/getData.r")

# aLink <- c("https://resources.lendingclub.com/LoanStats3a.csv.zip",
#            "https://resources.lendingclub.com/LoanStats3b.csv.zip",
#            "https://resources.lendingclub.com/LoanStats3c.csv.zip")
aLoanLinks <- c("https://resources.lendingclub.com/LoanStats3a.csv.zip")
aRejectStatsLinks <- c("https://resources.lendingclub.com/RejectStatsA.csv.zip")

myOfferedNotesandNonPolicyLoans <- getOfferedNotesAndNonPolicyLoans(aLoanLinks)
myOfferedNotes <- myOfferedNotesandNonPolicyLoans[[1]]
myNonPolicyNotes <- myOfferedNotesandNonPolicyLoans[[2]]

# myRejectStats <- getRejectStats(aRejectStatsLinks)
