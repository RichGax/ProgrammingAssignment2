## I wrote 2 functions. The first one creates a special 
## type of matrix that can be cached. The second one calculates
## the inverse matrix of the stored matrix and, if the inverse
## of such matrix has been calculated, it instead prints the 
## cached solution.

## This function is the one that creates the special 
## matrix, storable in the cache.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    

}


## This function is the second one, and it calculates the 
## inverse of the stored matrix and, if the calculation has
## been done before, it just prints the cached solution.

cacheSolve <- function(x, ...) {
    
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data") ## In case the calculation has been done before.
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    
        ## Return a matrix that is the inverse of 'x'
}
