pollutantmean <- function(directory, pollutant, id = 1:332){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    vector <- c()
    for(i in id){
        i_use = sprintf("%03d", i)
        directory_use <- paste("./", directory, "/", i_use, ".csv", sep="");
        mydata = read.csv(directory_use, header=T)
        vector <- c(vector, mydata[, pollutant])
    }
    mean <- mean(vector[!is.na(vector)])
    mean
}
