## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
    set <- function(y) {
        x <<- y
        inversed <<- NULL
    }
    get <- function() x
    setinversed <- function(inversedMatrix) inversed <<- inversedMatrix
    getinversed <- function() inversed
    list(set = set, get = get,
         setinversed = setinversed,
         getinversed = getinversed)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inversed <- x$getinversed()
    if(!is.null(inversed)) {
        message("getting cached data")
        return(inversed)
    }
    data <- x$get()
    inversed <- solve(data, ...)
    x$setinversed(inversed)
    inversed
}
