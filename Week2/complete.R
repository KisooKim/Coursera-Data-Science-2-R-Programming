complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    j <- 1
    nobs <- c()
    for (i in id) {
        i_use = sprintf("%03d", i)
        directory_use <- paste("./", directory, "/", i_use, ".csv", sep = "")
        mydata <- read.csv(directory_use, header = T)
        mydata_use <- complete.cases(mydata)
        nobs[j] <- sum(mydata_use)
        j <- j+1
    }
    vector <- data.frame(id = id, nobs = nobs)
    vector
}