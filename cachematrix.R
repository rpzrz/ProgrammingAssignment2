## Put comments here that give an overall description of what your
## functions do

##Makes a matrix object that sets up 4 functions in its local environment that can set and get the matric as well as
##set and get the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setmat <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getmat <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(setmat = setmat, getmat = getmat,
             setinv = setinv,
             getinv = getinv)
}


## Solves and returns the inverse of matrix if not done before. If inverse cached then returns cache.

cacheSolve <- function(x, ...) {
        inverse = x$getinv()
        if (!is.null(inverse)) {
                message("Cached inverse present and retrieved")
                return(inverse)
        }
        inverse = solve(x$getmat())
        x$setinv(inverse)
        inverse
}
