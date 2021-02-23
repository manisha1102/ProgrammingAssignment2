## Put comments here that give an overall description of what your
## The following pair of functions cache and compute the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(x) {
                mtx <<- x;
                inverse <<- NULL;
        }
        get <- function() return(mtx);
        setinv <- function(inv) inverse <<- inv;
        getinv <- function() return (inverse);
        return (list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special "matrix" returned by 'makeCacheMatrix' above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- mtx$getinv()
        if(!is.null(inverse)) {
                message("Getting cached data...")
                return (inverse)
        }
        data <- mtx$get()
        inverse <- solve(data, ...)
        mtx$getinv(inverse)
        return (inverse)
}
