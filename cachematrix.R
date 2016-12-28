## makeCacheMatrix creates a special "matrix" object that can cache it's inverse.
## The special object is a list of functions to 
## 1. set the value of the special matrix object
## 2. get the value of the special matrix object
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve solves the inverse matrix of a special object created by 
## makeCacheMatrix (above). However, it first checks to see if the inverse
## matrix has already been solved. If so, it gets the inverse matrix from the 
## cache and skips the computation. Otherwise, it solves the inverse of the
## matrix and sets the inverse matrix in the cache via the setinverse function.
##
## NOTE: this function assumes that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
