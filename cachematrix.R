## makeCacheMatrix and cacheSolve by Mark Smith
## General comment:
## It should be noted that this caching technique is only useful for caching
## one matrix which is computed repeatedly.  Storing the matrix and inverse
## in a cache and then finding/retrieving the result from multiple stored
## results would make it much more useful in the real world but it is out
## of scope for this assignment.

## makeCacheMatrix creates an object to store the state of the cached matrix.
## An environment is created (x) which persists the object state by defining
## functions that use the environment (due to scoping rules) even though it 
## is not the environment that they will be called from.
## This is what I know as a class definition in other languages, which we
## instantiate when we initialise it with an input matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()
                x
        setinverse <- function(inv)
                m <<- inv
        getinverse <- function()
                m
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## cacheSolve operates by getting the supplied makeCacheMatrix objects and
## then checking if the inverse has previously been calculated.  If not, it
## calculates, stores and returns the inverse, otherwise it returns the 
## cached result.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
