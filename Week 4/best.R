best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("./ProgAssignment3-data/outcome-of-care-measures.csv", colClasses="character")
    
    ## Check that state and outcome are valid
    data_state <- subset(data, State == state)
    if(nrow(data_state)==0){
        stop("invalid state")
    }
    
    ## Set outcome into code (the column number)
    switch(outcome, 
           'heart attack'={
               code <- 11
           },
           'heart failure'={
               code <- 17
           },
           'pneumonia'={
               code <- 23
           },
           {
               stop('invalid outcome')
           }
    )
    
    ## Downsize the data into one only with hospital name
    ## and the outcome values
    ## And turn "Not Available" into NA for convenience in
    ## calculation
    ## Also make the outcome column into a numeric type
    hospital_outcome <- data_state[c(2, code)]
    hospital_outcome[hospital_outcome=="Not Available"] <- NA
    hospital_outcome[,2] <- suppressWarnings(as.numeric(hospital_outcome[,2]))
    
    ## Select rows whose outcomes are not NA
    hospital_outcome_complete <- hospital_outcome[complete.cases(hospital_outcome),]
    
    ## Select Hospital Name with minimum outcome
    hospital_outcome_complete$Hospital.Name[hospital_outcome_complete[2] == min(hospital_outcome_complete[2])]
}