## The makeCacheMatrix is a function that takes a matrix as an argument and
## returns a list with 4 elements.

## The first element "set" is a function that sets the matrix of interest to 'x'
## and sets the inverse 'inv' to NULL

## The second element "get" is a function that returns the matrix of interest 'x'

## The third element "setinv" solves the inverse of the matrix 'x'

## The forth element "getinv" returns the inverse of the matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                     ## sets value of inverse to NULL
    set <- function(y){             ## function that sets the matrix of interest
        x <<- y                     ## and resets value of inv to NULL since matrix
        inv <<- NULL                ## may have changed if new argument
    }
    get <- function() x             ## function that returns x
    setinv <- function(solve)inv <<- solve  ## function that stores the inverse
    getinv <- function() inv        ## function that returns the inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)  ## returns the
        ## list with 4 functions
}


## cacheSolve is a function that takes a list of functions created by the 
## function makeCacheMatrix and returns the inverse of the matrix that was the 
## argument of makeCacheMatrix.

## the function calls 'getinv()' and determines if the inverse has already been
## calculated.  If so, it returns the inverse. If not, it retrieves the matrix
## using 'get()', solves for the inverse, sets the value in the makeCacheMean
## list, and returns the inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()                  ## retrieves inverse
    if(!is.null(inv)){                 ## determines if already calculated
        message("getting cached data") ## if so, report message and return inverse
        return(inv)
    }
    data <- x$get()                    ## if not, get the matrix of interest
    inv <- solve(data, ...)            ## solve for the inverse
    x$setinv(inv)                      ## store the inverse for future use
    inv                                ## return the inverse
}