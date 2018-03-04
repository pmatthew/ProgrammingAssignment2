## Mate P 2018-03-04
## This R script contains two functions, one to store a Matrix and its
## inverse Matrix, and one that actually calculates the inverse matrix 
## for the previously described object.

## This function creates and object, that can store a matrix (set), return the
## stored matrix (get); stores another matrix that supposed to be the 
## inverse of it (setinv), and also to return it (getinv). If a new matrix 
## will be set to the object, the inverse variable will be reset. 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function takes an object created by the makeCacheMatrix, and calculates
## the inverse of the stored matrix and updates that object with it. 
## In case the inverse is already calculated, it will not calculate it again. 

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
