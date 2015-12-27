## Put comments here that give an overall description of what your
## functions do
# 1. makeCacheMatrix function is getter and setter for matrix
# 2. cacheSolve function is to store and retrieve the inversion of matrix set by makeCacheMatrix function

## Write a short comment describing this function
# this function set default functions to get and set matrix. We can use this funtions as a getter and setter function of a matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x

    setInversion <- function(inversion) inv <<- inversion

    getInversion <- function() inv

    list(set = set, get = get, setInversion = setInversion, getInversion = getInversion)
}


## Write a short comment describing this function
# this function retrieve a inversion of a matrix set by makeCacheMatrix. If there is a inversion already calculated, then return that one or calculate, store, and retreive the inversion.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        in <- x$getInversion

        if (!is.null(inv)) {
            message("getting cached inversion")
            return(inv)
        }

        mat <- x$get()
        inv <- solve(mat)
        x$setInversion(inv)
        inv
}