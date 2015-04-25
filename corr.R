corr <- function(directory, threshold = 0){
    ## directory is a character vector of length 1 indicating the location 
    ## of the CSV files
    
    ## threshold is a numberic vector of length 1 indicating the number of
    ## completely observed observations on all variables required to compute 
    ## the correlation between nitrate and sulfate.  The default value is 0
    
    ## returns a numeric vector of correlations
    
    files <- list.files(directory) ##vector with file names as elements 
    cr <- vector("numeric", length=0)
    counter <- 1 ## initializes first element of cr
    for(i in 1:332){
        data <- read.csv(paste0("./", directory, "/", files[i]))
        if(sum(complete.cases(data)) > threshold) {
            cr[counter] <- cor(data[, 2], data[, 3], use = "pairwise.complete.obs")
            counter <- counter + 1
        }
    }
    print(cr)
}