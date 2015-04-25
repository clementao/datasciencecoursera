pollutantmean <- function(directory, pollutant, id = 1:332){
    ## directory is a character vector of length 1 indicating the location 
    ## of the CSV files
    
    ## pollutant is a character vector of lenth 1 indicating the name of 
    ## the pollutant for which we will calculate the mean; 
    ## either "sulfate" or "nitrate".
    
    ## id is an integer vector indicating the monitor ID number to be used
    
    ## Return the mean of the pollutant across all monitors listed in 
    ## the id vector (ignoring NA values)
    
    files <- list.files(directory) ##vector with file names as elements
    total <- 0
    count <- 0
    
    for (i in id) {
        data <- read.csv(paste0("./", directory, "/", files[i]))
        bad <- is.na(data[, pollutant])
        good <- data[!bad, ]
        total <- total + sum(good[, pollutant])  ## sums total values
        count <- count + nrow(good) ## counts up total number of values
    }
    averagetotal <- total/count
    averagetotal <- round(averagetotal, digits = 3)
    print (averagetotal)
}