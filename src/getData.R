getLendingClubFilename <- function(aLink) {
  myPrefix <- "https://resources.lendingclub.com/"
  mySuffix <- ".zip"
  
  mySuffixedFileName <- unlist(strsplit(aLink, myPrefix))[2]
  theFileName <- unlist(strsplit(mySuffixedFileName, mySuffix))[1]
  
  return(theFileName)
}

getRawDataFromLCLink <- function(aLink) {
  temp <- tempfile()
  download.file(aLink, temp, mode="wb")
  myFilename <- getLendingClubFilename(aLink)
  unzip(temp, myFilename)
  theRawData <- read.table(myFilename, sep = ",", fill = T)
  return(theRawData)
}

getLoansFromSingleLink <- function(aLink) {
  myRawIssuedLoansData <- getRawDataFromLCLink(aLink)
  
  myIndexOfNotesOffered <- which(grepl("^Notes offered by Prospectus", myRawIssuedLoansData[, 1]))
  myIndexOfLoansNotMeeting <- which(grepl("^Loans that do not meet the credit policy", myRawIssuedLoansData[, 1]))
  myIndexOfTotalFunded <- which(grepl("^Total amount funded in policy code 1", myRawIssuedLoansData[, 1]))
  
  myLoansIndices <- (myIndexOfNotesOffered + 2):(myIndexOfLoansNotMeeting - 1)
  myNonPolicyLoansIndices <- (myIndexOfLoansNotMeeting + 1):(myIndexOfTotalFunded - 1)
  
  myColumnNames <- unlist(myRawIssuedLoansData[myIndexOfNotesOffered + 1,])
  
  theLoans <- myRawIssuedLoansData[myLoansIndices,]
  colnames(theLoans) <- myColumnNames
  
  theNonPolicyLoans <- myRawIssuedLoansData[myNonPolicyLoansIndices,]
  colnames(theNonPolicyLoans) <- myColumnNames
  
  return(list(theLoans, theNonPolicyLoans))
}

getLoans <- function(aLinks) {
  theLoans <- data.frame()
  theNonPolicyLoans <- data.frame()
  
  for(myLink in aLinks) {
    myLCLoanOutput <- getLoansFromSingleLink(myLink)
    theLoans <- rbind(theLoans, myLCLoanOutput[[1]])
    theNonPolicyLoans <- rbind(theNonPolicyLoans, myLCLoanOutput[[2]])
  }
  return(list(theLoans, theNonPolicyLoans))
}

getRejectStatsFromSingleLink <- function(aLink) {
  myRawRejectStats <- getRawDataFromLCLink(aLink)
  
  myIndexOfNotesOffered <- which(grepl("^Notes offered by Prospectus", myRawRejectStats[, 1]))
  
  myRejectStatsIndices <- (myIndexOfNotesOffered + 2):dim(myRawRejectStats)[1]
  
  myColumnNames <- unlist(myRawRejectStats[myIndexOfNotesOffered + 1,])
  
  theRejectStats <- myRawRejectStats[myRejectStatsIndices,]
  colnames(theRejectStats) <- myColumnNames
  
  return(theRejectStats)
}

getRejectStats <- function(aLinks) {
  theRejectStats <- data.frame()
  
  for(myLink in aLinks) {
    myRejectStats <- getRejectStatsFromSingleLink(myLink)
    theRejectStats <- rbind(theRejectStats, myRejectStats)
  }
  return(theRejectStats)
}

transformLoans <- function(aLoans) {
  aLoans$issue_d <- sapply(aLoans$issue_d, function(x) as.numeric(as.POSIXct(x, format="%Y-%m-%d")))
  aLoans$last_pymnt_d <- sapply(aLoans$last_pymnt_d, function(x) as.numeric(as.POSIXct(x, format="%Y-%m-%d")))
  aLoans$int_rate <- sapply(aLoans$int_rate, function(x) as.numeric(sub("%", "", x)) * .01)
  aLoans$term <- sapply(aLoans$term, function(x) as.numeric(sub(" months", "", x)) / 12)
  aLoans$funded_amnt <- sapply(aLoans$funded_amnt, function(x) as.numeric(x))
  aLoans$total_pymnt_inv <- sapply(aLoans$total_pymnt_inv, function(x) as.numeric(x))
  aLoans$total_pymnt <- sapply(aLoans$total_pymnt, function(x) as.numeric(x))
  return(aLoans)
}