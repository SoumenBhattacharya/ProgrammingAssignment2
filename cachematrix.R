## Set of functions to compute matrix inverse and cache it.
##
## This will create special matrix objects that can cache their inverse
## and once it is computed, it will retrieve from the cache rather than
## re-computing it.

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL               ## This will store the inverse value
    set <- function(y) {
        x <<- y
        v <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) v <<- inverse
    getinverse <- function() v
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes as input a special matrix created by makeCacheMatrix()
## function and computes its inverse. If inverse is available in cache, it
## returns it from cache and does not re-compute

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    v <- x$getinverse()
    if(!is.null(v)) {
        message("getting cached data")
        return(v)
    }
    data <- x$get()
    v <- solve(data, ...)
    x$setinverse(v)
    v
}

