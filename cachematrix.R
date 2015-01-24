## Put comments here that give an overall description of what your
## functions do

## These functions are written as part of R Programming Assignment 2.
## The objective of these functions is to cache potentially time-consuming inverse matrix calculation, especially important for big matrices.

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix, invm
## get the value of the mean, invm

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL #initialize inverse matrix x
    set <- function(y) {
        x <<- y 
        invm <<- NULL #initialize inverse matrix x if x is changed
    }
    get <- function() x
    setInv <- function(inverse_matrix) invm <<- inverse_matrix
    getInv <- function() invm
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function

## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse matrix, invm has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the given matrix and sets the value of the inverse matrix in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invm <- x$getInv()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data)
    x$setInv(invm)
    invm
}
