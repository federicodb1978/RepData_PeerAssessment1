fillData <- function(dataStep){    
        newData <- dataStep
        meanVect <- tapply(dataStep$steps, dataStep$date, FUN = mean, na.rm = TRUE)
        meanVect[is.na(meanVect)] <- 0
        max <- length(dataStep$steps)
        for (i in 1:max) {
                if (!is.na(dataStep$steps[i])) {
                        newData$steps[i] <- dataStep$steps[i]
                }else {
                        newData$steps[i] <- meanVect[which(as.character(names(meanVect)) == as.character(newData$date[i]))]
                }
        }
        return(newData)
}

