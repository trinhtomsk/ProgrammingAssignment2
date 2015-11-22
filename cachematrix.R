## This file contains two functions (makeCacheMatrix and cacheSolve) to enable caching for an inversed matrix
## The code is submitted in order to fulfill the Programming Assignment 2 of the R programming course available on Coursera
## Asumption: input matrix must be invertible

## This function defines the get and set method for inversed matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvMatrix <- function(invMatrix) m <<- invMatrix
        getinvMatrix <- function() m
        list(set = set, get = get,
             setinvMatrix = setinvMatrix,
             getinvMatrix = getinvMatrix)
}


## This function gets the value of inversed matrix if the value is cached, otherwise it computes the inversion of the input matrix
## This function uses solve function to calculate the inversion of the input matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvMatrix(m)
        m
}


