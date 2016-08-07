## writing two functions to retrieve cached copy of the inverse of a matrix - if it exists in cache, 
## otherwise we compute the inverse and add it to cache

## special matrix object that can cache
## the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(z) i <<- z
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## method checks if inverse of matrix already exists
## in the special matrix object; if not, it computes
## the inverse using solve(), and updates the
## special matrix object
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
