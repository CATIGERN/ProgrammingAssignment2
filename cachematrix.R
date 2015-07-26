## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
##calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix", which
## is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. get the value of the inverse matrix.
## 4. set the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(provided_inv) inv <<- provided_inv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## If the inverse was already calculated,
## it returns the cached copy of the inverse
## else it computes the inverse and returns it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
