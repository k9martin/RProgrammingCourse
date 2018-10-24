best <- function(state, outcome){
        
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
                state_data <- data_read[state_index, ]
                
                
                desired_hospitals<- which(as.numeric(state_data[, outcome_column])== min (as.numeric(state_data[, outcome_column]),na.rm = TRUE))
                hospitals <- state_data[desired_hospitals,2]
                hospitals <- sort(hospitals)
                return (hospitals[1])
        }
        
}