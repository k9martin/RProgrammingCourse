rankhospital <- function(state, outcome, num="best"){
        
        
        if (num == "best" || num == 1){
                return (best(state,outcome))
        }
        else {
        ## Read data from csv file
        data_read <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check if state and outcome appears in csv file
        if (!state %in% data_read$State){
                stop ("Invalid state.")
        }else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop ("Invalid outcome.")
        }else {
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
                
                
                
                state_index <- which(data_read[, "State"] == state)
                
                if (is.numeric(num) == TRUE) {
                        if (length(state_index) < num) {
                                return(NA)
                        }
                }
                
                
                state_data <- data_read[state_index, ]
                
                state_data[, outcome_column] <- as.numeric(state_data[,outcome_column])
                bad <- is.na(state_data[, outcome_column])
                desired_data <- state_data[!bad, ]
                
                ordered_desired_data <- desired_data[order(desired_data[ ,outcome_column],desired_data[,2]), ]
                
                if (num == "worst") {
                        num = length(ordered_desired_data[, outcome_column])
                }
                
                ordered_desired_data[num, 2]
                
        }
        
        }
}