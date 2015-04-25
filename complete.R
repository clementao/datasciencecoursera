complete <- function(directory, id = 1:332){
    ## directory is a character vector of length 1 indicating the location 
    ## of the CSV files
    
    ## id is an integer vector indicating the monitor ID number to be used
    
    ## return a dataframe of the form:
    ## id  nobs
    ## 1    117
    ## 2     1041
    ## ....
    ## where id is the monitor number and nobs is the 
    ## number of complete cases
    
    files <- list.files(directory) ##vector with file names as elements
    nobs <- vector("numeric", length=length(id))
    counter <- 1 ## initializes as first element of nobs vector; increase by 1 each round of loop
    for (i in id) {
        data <- read.csv(paste0("./", directory, "/", files[i]))
        nobs[counter] <- sum(complete.cases(data))
        counter <- counter +1
    }
    CompleteCases <- data.frame(id, nobs)
    print(CompleteCases)
}