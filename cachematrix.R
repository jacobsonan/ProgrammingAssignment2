## The following pair of functions, when used together, will return the
## inverse of a square invertible matrix and cache it. They will check before
## computing the inverse to determine whether the inverse has already been
## cached, and if it has, the inverse matrix will be retrieved from the cache,
## rather than re-computed.


## the makeCacheMatrix function returns an object that contains a list of four
## functions: set(), get(), setsolve(), and getsolve() when it is passed a
## matrix as an argument. When the resulting object is passed to another
## function, the calling function has access to the four internal functions as
## well as the original argument x and the initiated variable v.

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setsolve <- function(inverse) v <<- inverse
        getsolve <- function() v
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The cacheSolve function takes as an argument an object created by the
## myCacheMatrix function. It uses the functions within that object to first
## check whether the matrix inverse has been computed and return it with a
## "getting cached data" message if it has. If the inverse has not been computed,
## it computes the inverse and returns it, updating the object so that the 
## inverse will be available in the cache if it is called again.

cacheSolve <- function(x, ...) {
        v <- x$getsolve()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        newMatrix <- x$get()
        v <- solve(newMatrix, ...)
        x$setsolve(v)
        v
}
