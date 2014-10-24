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
  
  myLoans <- myRawIssuedLoansData[myLoansIndices,]
  colnames(myLoans) <- myColumnNames
  theLoans <- transformLoans(myLoans)
  
  myNonPolicyLoans <- myRawIssuedLoansData[myNonPolicyLoansIndices,]
  colnames(myNonPolicyLoans) <- myColumnNames
  theNonPolicyLoans <- transformLoans(myNonPolicyLoans)
  
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

transformColumnToNumeric <- function(aColumn) {
  theColumn <- as.numeric(as.vector(aColumn))
  return(theColumn)
}

transformColumnToPOSIX <- function(aColumn) {
  theColumn <- sapply(aColumn, function(x) as.numeric(as.POSIXct(x, format="%Y-%m-%d")))
  return(theColumn)
}

transformColumnToDateTimePOSIX <- function(aColumn) {
  theColumn <- sapply(aColumn, function(x) as.numeric(as.POSIXct(x, format="%Y-%m-%d %H:%M")))
  return(theColumn)
}

transformColumnFromPercentage <- function(aColumn) {
  theColumn <- sapply(aColumn, function(x) as.numeric(sub("%", "", x)) * .01)
  return(theColumn)
}

transformColumnFromMonth <- function(aColumn) {
  myMonthsToSecondsFactor <- 30.5 * 24 * 60 * 60
  theColumn <- sapply(aColumn, function(x) as.numeric(sub(" months", "", x)) * myMonthsToSecondsFactor)
  return(theColumn)
}

transformColumnFromYear <- function(aColumn) {
  myYearsToSecondsFactor <- 365.25 * 24 * 60 * 60
  theColumn <- sapply(aColumn, function(x) as.numeric(gsub("([0-9]+).*$", "\\1", x)) * myYearsToSecondsFactor)
  return(theColumn)
}

transformColumns <- function(aLoans, aColumnNames, aTransformation) {
  print(paste("Transforming", aTransformation, "columns..."))
  for(i in 1:length(aColumnNames)) {
    myColumnName <- aColumnNames[i]
    if(aTransformation == "numeric") {
      aLoans[, myColumnName] <- transformColumnToNumeric(aLoans[, myColumnName])
    } else if(aTransformation == "date") {
      aLoans[, myColumnName] <- transformColumnToPOSIX(aLoans[, myColumnName])
    } else if(aTransformation == "datetime") {
      aLoans[, myColumnName] <- transformColumnToDateTimePOSIX(aLoans[, myColumnName])
    } else if(aTransformation == "percentage") {
      aLoans[, myColumnName] <- transformColumnFromPercentage(aLoans[, myColumnName])
    } else if(aTransformation == "month") {
      aLoans[, myColumnName] <- transformColumnFromMonth(aLoans[, myColumnName])
    } else if(aTransformation == "year") {
      aLoans[, myColumnName] <- transformColumnFromYear(aLoans[, myColumnName])
    } else {
      stop("requested transformation is not available")
    }
    print(paste(i, "of", length(aColumnNames), "done."))
  }
  return(aLoans)
}

transformLoans <- function(aLoans) {
  myNumericColumnNames <- c("loan_amnt", "funded_amnt", "funded_amnt_inv", "installment", "annual_inc",
                            "acc_now_delinq", "dti", "delinq_2yrs", "delinq_amnt", "inq_last_6mths",
                            "mths_since_last_delinq", "open_acc", "pub_rec", "revol_bal", "total_acc",
                            "out_prncp", "out_prncp_inv", "total_pymnt", "total_pymnt_inv",
                            "total_rec_prncp", "total_rec_int", "total_rec_late_fee", "recoveries",
                            "collection_recovery_fee", "last_pymnt_amnt", "pub_rec_bankruptcies", 
                            "chargeoff_within_12_mths", "collections_12_mths_ex_med", "tax_liens")
  myDateColumnNames <- c("accept_d", "exp_d", "list_d", "issue_d", "last_pymnt_d", "next_pymnt_d",
                         "last_credit_pull_d")
  myPercentColumnNames <- c("int_rate", "revol_util")
  myDateTimeColumnNames <- c("earliest_cr_line")
  myMonthColumns <- c("term")
  myYearColumns <- c("emp_length")
  
  aLoans <- transformColumns(aLoans, myNumericColumnNames, "numeric")
  aLoans <- transformColumns(aLoans, myDateColumnNames, "date")
  aLoans <- transformColumns(aLoans, myPercentColumnNames, "percentage")
  aLoans <- transformColumns(aLoans, myDateTimeColumnNames, "datetime")
  aLoans <- transformColumns(aLoans, myMonthColumns, "month")
  aLoans <- transformColumns(aLoans, myYearColumns, "year")
  
  return(aLoans)
}