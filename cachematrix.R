## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## returns array of functions to get/set matrix and its inverse
## matrix and its inverse are stored as key value pairs
## the vector returned by this function is passed as input to cacheSolve function

makeCacheMatrix <- function(mat = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        mat <<- y
        cachedInverse <<- NULL
    }
    get <- function() mat
    getInverse <- function() cachedInverse  
    setInverse <- function(inverseOfMatrix) cachedInverse <<- inverseOfMatrix
    list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## Write a short comment describing this function
## checks if inverse of matrix is already computed. If yes returns.
## If not computes the inverse and returns
## And caches the inverse for future requests to find inveres of same matrix

cacheSolve <- function(x,...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    dataMatrix <- x$get()
    inverse <- solve(dataMatrix)
    x$setInverse(inverse)
    inverse
}
