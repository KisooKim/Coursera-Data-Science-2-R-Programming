rankhospital <- function(state, outcome, num = "best") {
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
    
    complete_count <- nrow(hospital_outcome_complete)
    
    ## Set num
    if(num=="best"){
        num <- 1
    }else if(num=="worst"){
        num <- complete_count
    }else if(num <= complete_count && num>0){
        num <- num
    }else{
        return(NA)
    }
    
    ## Order by Outcome DESC, Hospital.Name DESC
    hospital_outcome_complete <- hospital_outcome_complete[order(hospital_outcome_complete[2], hospital_outcome_complete[1]),]
    
    ## Choose the first column (name) of num's row
    hospital_outcome_complete[num,1]
}