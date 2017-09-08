## This module contains two functions.  One for creating a structure that allows
## memoization of a calculated value alongside the original matrix.  And another
## for calculating the inverse of the matrix and storing the result in the 
## structure for future reference, and ultimately returning that result.


## This function defines variables and declares functions within its closure for 
## containing a matrix and some cached value associated with that matrix. It 
## returns a list containing the functions that operate on the variables 
## defined in the closure.
makeCacheMatrix <- function(x = matrix()) {
    ## Declare a variable to cache a value associated with the matrix x
    cached <- NULL
    
    ## Declare a function for overwriting the argument x and clearing the 
    ## cached value
    set <- function(y) {
        x <<- y
        cached <<- NULL
    }
    
    ## Declare a function for getting the argument x
    get <- function() x
    
    ## Declare a function for setting the cached value
    setCached <- function(value) cached <<- value
    
    ## Declare a function for getting the cached value
    getCached <- function() cached
    
    ## Put all the functions that operate on the variables in the closure
    ## into a list that the function cacheSolve(x, ...) will understand.
    list(set = set, get = get, setCached = setCached, getCached = getCached)
}


## This function calculates the inverse of a matrix that has been stored in
## the structure created by a call to makeCacheMatrix(x).  The inverse matrix
## is calculated and stored in that structure so that subsequent calls to 
## this function using the same structure can used the previously cached 
## value as opposed to calculating the result again.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    value <- x$getCached()
    if (!is.null(value)) {
        message("Using cached value")
        return(value)
    }
    
    message("Calculating new value")
    data <- x$get()
    value <- solve(data, ...)
    x$setCached(value)
    value
}
