rankall <- function(outcome, num="best"){
        
        
        
        ## Read data from csv file
        data_read <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check if state and outcome appears in csv file
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop ("Invalid outcome.")
        }else if (num > length(data_read[,1])){
                return (NA)
        }else{
                ## Return best hospital for this outcome in the state
                
                if (outcome == "heart attack") {
                        outcome_column <- 13
                }
                else if (outcome == "heart failure") {
                        outcome_column <- 19
                }
                else {
                        outcome_column <- 25
                }
                
                
                data_read[ ,outcome_column] <- as.numeric(data_read[ ,outcome_column])
                
                data_read = data_read[!is.na(data_read[,outcome_column]),]
                
                splited = split(data_read, data_read$State)
                ans = lapply(splited, function(x, num) {
                        x = x[order(x[,outcome_column], x$Hospital.Name),]
                        
                        if(class(num) == "character") {
                                if(num == "best") {
                                        return (x$Hospital.Name[1])
                                }
                                else if(num == "worst") {
                                        return (x$Hospital.Name[nrow(x)])
                                }
                        }
                        else {
                                return (x$Hospital.Name[num])
                        }
                }, num)
                
                #Return data.frame with format
                return ( data.frame(hospital=unlist(ans), state=names(ans)) )
        }
        
        
}