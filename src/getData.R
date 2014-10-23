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

getOfferedNotesAndNonPolicyLoansFromSingleLink <- function(aLink) {
  myRawIssuedLoansData <- getRawDataFromLCLink(aLink)
  
  myIndexOfNotesOffered <- which(grepl("^Notes offered by Prospectus", myRawIssuedLoansData[, 1]))
  myIndexOfLoansNotMeeting <- which(grepl("^Loans that do not meet the credit policy", myRawIssuedLoansData[, 1]))
  myIndexOfTotalFunded <- which(grepl("^Total amount funded in policy code 1", myRawIssuedLoansData[, 1]))
  
  myOfferedNotesIndices <- (myIndexOfNotesOffered + 2):(myIndexOfLoansNotMeeting - 1)
  myNonPolicyLoansIndices <- (myIndexOfLoansNotMeeting + 1):(myIndexOfTotalFunded - 1)
  
  myColumnNames <- unlist(myRawIssuedLoansData[myIndexOfNotesOffered + 1,])
  
  theOfferedNotes <- myRawIssuedLoansData[myOfferedNotesIndices,]
  colnames(theOfferedNotes) <- myColumnNames
  
  theNonPolicyLoans <- myRawIssuedLoansData[myNonPolicyLoansIndices,]
  colnames(theNonPolicyLoans) <- myColumnNames
  
  return(list(theOfferedNotes, theNonPolicyLoans))
}

getOfferedNotesAndNonPolicyLoans <- function(aLinks) {
  theOfferedNotes <- data.frame()
  theNonPolicyLoans <- data.frame()
  
  for(myLink in aLinks) {
    myLCLoanOutput <- getOfferedNotesAndNonPolicyLoansFromSingleLink(myLink)
    theOfferedNotes <- rbind(theOfferedNotes, myLCLoanOutput[[1]])
    theNonPolicyLoans <- rbind(theNonPolicyLoans, myLCLoanOutput[[2]])
  }
  return(list(theOfferedNotes, theNonPolicyLoans))
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