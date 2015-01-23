## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix object that can cache its reverse.
## Input is a matrix, the cachesolve function wil be used to
## calculate the reverse of the matrix and caches it.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function() inv <<- solve(x)
##  Library MASS contains a pseudo inverse function which is
##  a generalization of matrix inversion that works for all matrices:
##  So for me it was easier to test the functions. Above setinverse can be replaced by below 
##  two lines.
##  library("MASS")
##  setinverse <- function() inv <<- ginv(x)
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already
## been calculated (and the matrix has not changed), then
## retrieve the inverse from cache.
## Input : Matrix x
## Output: Reversed matrix inv.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- x$setinverse()
    inv
}
