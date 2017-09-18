rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("./ProgAssignment3-data/outcome-of-care-measures.csv", colClasses="character")
    
    ## Check that outcome is valid and
    ## set outcome into code (the column number)
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
    hospital_outcome <- data[c(2, code, 7)]
    hospital_outcome[hospital_outcome=="Not Available"] <- NA
    hospital_outcome[,2] <- suppressWarnings(as.numeric(hospital_outcome[,2]))
    
    ## Select rows whose outcomes are not NA
    complete <- hospital_outcome[complete.cases(hospital_outcome),]
    
    ## Set the names of complete as conveniently
    names(complete)[1] <- 'hospital'
    names(complete)[2] <- 'outcome'
    names(complete)[3] <- 'state'
    
    class(complete)
    
    ## Split the complete dataframe into splited list
    splited <- split(complete, complete$state)
    list <- lapply(splited, function(x, num){
        # set list as data frame
        df <- data.frame(x)
        df_count <- nrow(df)
        
        # order the data frame
        ordered <- df[order(df$outcome, df$hospital),]
        
        # Set which row to return
        if(num=="best"){
            num <- 1
        }else if(num=="worst"){
            num <- df_count
        }else if(num <= df_count && num>0){
            num <- num
        }else{
            return(c(NA, df$state))
        }
        
        #return that row
        return (ordered[num,])
    }, num)
    
    as.data.frame(do.call(rbind, list))[c(1,3)]
}