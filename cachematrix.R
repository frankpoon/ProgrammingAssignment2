## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setmatrix <- function(mt) i <<- mt
    getmatrix <- function() i
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getmatrix()
    if(!is.null(i)) {
        message("getting cached matrix")
        return(i)
    }
    i <- solve(x$get())
    x$setmatrix(i)
    i
}
