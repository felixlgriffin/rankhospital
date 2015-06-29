# rankhospital
Rank Hospital
rankhospital <- function(state, outcome, num = "best") {
  
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    states <- unique( outcomeData[,7] )
    if( !( state %in% states ) )
        stop("invalid state")
    if( !( outcome %in% c("heart attack","heart failure","pneumonia") ) )
        stop("invalid outcome")
    
    ## coerc the numeric fields
    suppressWarnings( outcomeData[,11] <- as.numeric(outcomeData[,11]) )
    suppressWarnings( outcomeData[,17] <- as.numeric(outcomeData[,17]) )
    suppressWarnings( outcomeData[,23] <- as.numeric(outcomeData[,23]) )
    
    ## filter on state
    outcomeData <- subset( outcomeData, State==state )
    
    if( outcome == "heart attack" )
        outcomeColumn <- 11
    if( outcome == "heart failure" )
        outcomeColumn <- 17 
    if( outcome == "pneumonia" ) 
        outcomeColumn <- 23 
    
    if( num == "worst" )
        outcomeDataSort <- outcomeData[order(outcomeData[outcomeColumn],outcomeData[2], na.last=FALSE) , ]
    else 
        outcomeDataSort <- outcomeData[order(outcomeData[outcomeColumn],outcomeData[2]) , ]
    
    ## figure out the top/bottom number 
    if( is.numeric(num) ) 
        numUse <- num
    else
        if( num == "best" )
            numUse <- 1
        else
            numUse <- nrow(outcomeDataSort)
    
    ## Return hospital name in that state with the given rank 30-day death rate
    outcomeDataSort[numUse,2]
    
    #outcomeDataSort[,c(1,2,11,17,23)]
