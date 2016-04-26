## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Define a Object to storage a special Matrix,
## Which can also save the Matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
    inversem <- NULL
    set <- function(y) {
        x <<- y
        inversem <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inversem <<- inverse
    getinverse <- function() inversem
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
    inversem <- x$getinverse()
    if(!is.null(inversem)) {
        message("getting cached data")
        return(inversem)
    }
    data <- x$get()
    inversem <- solve(data, ...)
    x$setinverse(inversem)
    inversem
}
